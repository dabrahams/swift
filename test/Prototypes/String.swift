// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %t/out.swift -o %t/a.out -Onone
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out
// REQUIRES: executable_test

import Swift


%{
  WORD_BITS = int(WORD_BITS)
  builtinIntLiteralBits = 2048
}%


/// The result of transcoding from a collection using a “pull-style” API.
///
/// - Associated value fields:
///  - `output` is the result of error-free transcoding
///  - `resumptionPoint` indicates a position in the input from which to continue
///    transcoding.
///
/// - Parameter T: the result of a successful parse
/// - Parameter Index: the index type of the collection being transcoded.
enum ParseResult<T, Index> {
case valid(T, resumptionPoint: Index)
case error(resumptionPoint: Index)
case emptyInput
  var resumptionPoint: Index? {
    switch self {
    case .valid(_,let r): return r
    case .error(let r): return r
    case .emptyInput: return nil
    }
  }
  var valid : T? {
    if case .valid(let r,_) = self { return r }
    return nil
  }
}

protocol EncodedScalarProtocol : RandomAccessCollection {
  var utf8: UTF8.EncodedScalar { get }
  var utf16: UTF16.EncodedScalar { get }
  var utf32: UTF32.EncodedScalar { get }
}

extension UTF8 {
  /// Returns `true` iff [`c0`, `c1`] is a prefix of a valid 3-byte sequence
  static func isValid3BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b1110_0000__10_100000...0b1110_1101__10_011111 ~= joint
        || 0b1110_1110__10_000000...0b1110_1111__10_111111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid 4-byte sequence
  static func isValid4BytePrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    let joint = UInt16(c0) << 8 | UInt16(c1)
    return 0b11110_000__10_010000...0b11110_100__10_001111 ~= joint
  }

  /// Returns true iff [`c0`, `c1`] is a prefix of a valid sequence
  static func isValidPrefix(_ c0: CodeUnit, _ c1: CodeUnit) -> Bool {
    return isValid3BytePrefix(c0, c1) || isValid4BytePrefix(c0, c1)
  }

  /// Given a valid leading byte of a multibyte sequence, strip the leading 1
  /// bits.
  ///
  /// - Note: Given any other byte, the result is unspecified.
  static func maskLeadByte(_ x: UInt8) -> UInt8 {
    return x & (0b11111 >> (x >> 5 & 1))
  }
  
  /// Parses one scalar forward from `input`.
  ///
  ///   - Parameter knownCountExceeds3: true if and only if the input is known
  ///   be at least 4 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Forward<C: Collection>(
    _ input: C, knownCountExceeds3: Bool = false
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit {

    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.
    if input.isEmpty { return .emptyInput }

    var i = input.startIndex 
    let end = input.endIndex
    @inline(__always) func inputConsumed() -> Bool {
      return _slowPath(!knownCountExceeds3 && i == end)
    }
    
    let u0 = input[i]
    var j = input.index(after: i)
    
    if _fastPath(Int8(bitPattern: u0) >= 0) {
      return .valid(EncodedScalar(u0), resumptionPoint: j)
    }
    i = j // even if there are errors, we eat 1 byte

    // Begin accumulating result
    var r = UInt32(u0)
    var shift: UInt32 = 0
    
    // Mark one more token recognized and get the next lookahead token iff it
    // falls within pattern
    @inline (__always)
    func nextContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      i = j
      if inputConsumed() { return false } // no more tokens
      let u = input[j]
      if _fastPath(pattern ~= u) {
        shift += 8
        r |= UInt32(u) << shift
        j = input.index(after: j)
        return true
      }
      return false
    }
    
    @inline(__always)
    func state7() -> ParseResult<EncodedScalar, C.Index> {
      return nextContinuation(0x80...0xbf)
      ? .valid(EncodedScalar(_bits: r), resumptionPoint: j)
      : .error(resumptionPoint: i)
    }
    
    @inline(__always)
    func state3() -> ParseResult<EncodedScalar, C.Index> {
      return nextContinuation(0x80...0xbf)
        ? state7() : .error(resumptionPoint: i)
    }
    
    // Two-byte case
    if _fastPath(0xc2...0xdf ~= u0) {
      return state7()
    }

    // Three-byte cases
    else if _fastPath(u0 == 0xe0) {
      if nextContinuation(0xa0...0xbf) { return state7() }
    }
    else if _fastPath(u0 == 0xed) { // state 2
      if nextContinuation(0x80...0x9f) { return state7() }
    }
    else if _fastPath(0xe1...0xef ~= u0) { return state3() }
    
    // Four-byte cases
    else if _fastPath(0xf1...0xf3 ~= u0) { // state 5
      if nextContinuation(0x80...0xbf) { return state3() }
    }
    else if u0 == 0xf0 {
      if nextContinuation(0x90...0xbf) { return state3() }
    }
    else if u0 == 0xf4 {
      if nextContinuation(0x80...0x8f) { return state3() }
    }
    
    return .error(resumptionPoint: i)
  }

  /// Parses one scalar in reverse from `input`.
  /// 
  ///   - Parameter knownCountExceeds3: true if and only if the input is known
  ///   be at least 4 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCountExceeds3: Bool = false
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    // See
    // https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5#gistcomment-1931947
    // for an explanation of this state machine.

    if _slowPath(!knownCountExceeds3 && input.isEmpty) { return .emptyInput }

    var i = input.index(before: input.endIndex)
    var j = i
    let j0 = j
    var u = input[j]
    if _fastPath(Int8(bitPattern: u) >= 0) {
      return .valid(EncodedScalar(u), resumptionPoint: j)
    }
    
    let start = input.startIndex
    var r = UInt32(u)

    // Mark one more token recognized and get the next lookahead token iff it
    // satisfies the predicate
    @inline(__always)
    func consumeContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      guard _fastPath(pattern ~= u) else { return false }
      i = j
      guard _fastPath(knownCountExceeds3 || j != start) else { return false }
      r <<= 8
      r |= UInt32(u)
      j = input.index(before: j)
      u = input[j]
      return true
    }

    @inline(__always)
    func accept(_ pat: ClosedRange<UInt8>) -> ParseResult<EncodedScalar, C.Index>? {
      if _fastPath(pat.contains(u)) {
        r <<= 8
        r |= UInt32(u)
        return .valid(EncodedScalar(_bits: r), resumptionPoint: j)
      }
      return nil
    }
    
    @inline(__always)
    func state4_5() -> ParseResult<EncodedScalar, C.Index> {
      return accept(0xf0...0xf3) ?? .error(resumptionPoint: j0)
    }
    
    @inline(__always)
    func state5_6() -> ParseResult<EncodedScalar, C.Index> {
      return accept(0xf1...0xf4) ?? .error(resumptionPoint: j0)
    }

    let u0 = u
    if consumeContinuation(0x80...0xbf) {          // state 7
      if let x = accept(0xc2...0xdf)                     { return x }

      let u1 = u
      if consumeContinuation(0x80...0x9f) {                         // state 2/3
        if let x = accept(0xe1...0xef)                              { return x }
        if consumeContinuation(0x90...0xbf)                { return state4_5() }
        if consumeContinuation(0x80...0x8f)                { return state5_6() }
        if isValid4BytePrefix(u, u1)       { return .error(resumptionPoint: j) }
      }
      else if consumeContinuation(0xa0...0xbf) {                    // state 1/3
        if let x = accept(0xe0...0xec)                              { return x }
        if let x = accept(0xee...0xef)                              { return x }
        if consumeContinuation(0x90...0xbf)                { return state4_5() }
        if consumeContinuation(0x80...0x8f)                { return state5_6() }
        if isValid4BytePrefix(u, u1)       { return .error(resumptionPoint: j) }
      }
      else if isValidPrefix(u, u0)         { return .error(resumptionPoint: j) }
    }
    return .error(resumptionPoint: j0)
  }
  
  /// Parse a whole collection efficiently, using `parse` to read each unicode
  /// scalar value, writing results into `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  static func parseForward<C: Collection>(
    _ input: C,
    using parse: (C.SubSequence, Bool)->ParseResult<EncodedScalar, C.SubSequence.Index>,
    stoppingOnError stopOnError: Bool = false,
    into output: (ParseResult<EncodedScalar, C.SubSequence.Index>)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : Collection, C.SubSequence.SubSequence == C.SubSequence,
    C.SubSequence.Iterator.Element == CodeUnit {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(_ o: ParseResult<EncodedScalar, C.SubSequence.Index>)
      -> ParseResult<EncodedScalar, C.SubSequence.Index>? {
      switch o {
      case .emptyInput:
        // Should we make this case unreachable for the first loop below, or is
        // the compiler smart enough to avoid introducing overhead?
        return nil 
      case .valid(_, _): break
      case .error(_): errorCount += 1
        if stopOnError { return nil }
      }
      remainder = remainder.suffix(from: o.resumptionPoint!)
      return o
    }

    // Repeatedly eat the first 25% without checking for end-of-input.
    while remainder.count >= 4 {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) >> 2 {
        guard let o = eat(parse(remainder, true)) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse(remainder, false)) else {
        return (remainder, errorCount)
      }
      output(o)
    }
  }

  /// Parse a whole collection efficiently in reverse, using `parse`
  /// to read each unicode scalar value, writing results into
  /// `output`.
  ///
  /// - Returns: a pair consisting of:
  ///   0. the suffix of input starting with the first decoding error if
  ///      `stopOnError` is true, and the empty suffix otherwise.
  ///   1. The number of errors that were detected.  If `stopOnError` is true
  ///      this value will never exceed 1.
  ///
  /// - Note: using this function may be faster than repeatedly using `parse`
  ///   directly, because it avoids intra-scalar checks for end of sequence.
  @discardableResult
  static func parseReverse<C: BidirectionalCollection>(
    _ input: C,
    using parse: (C.SubSequence, Bool)->ParseResult<EncodedScalar, C.SubSequence.Index>,
    stoppingOnError stopOnError: Bool = false,
    into output: (ParseResult<EncodedScalar, C.SubSequence.Index>)->Void
  ) -> (remainder: C.SubSequence, errorCount: Int)
  where C.SubSequence : BidirectionalCollection,
        C.SubSequence.SubSequence == C.SubSequence,
        C.SubSequence.Iterator.Element == CodeUnit {
    var remainder = input[input.startIndex..<input.endIndex]
    var errorCount = 0
    
    func eat(_ o: ParseResult<EncodedScalar, C.SubSequence.Index>)
      -> ParseResult<EncodedScalar, C.SubSequence.Index>? {
      switch o {
      case .emptyInput:
        // Should we make this case unreachable for the first loop below, or is
        // the compiler smart enough to avoid introducing overhead?
        return nil 
      case .valid(_, _): break
      case .error(_): errorCount += 1
        if stopOnError { return nil }
      }
      remainder = remainder.prefix(upTo: o.resumptionPoint!)
      return o
    }

    // Repeatedly eat the last 25% without checking for end-of-input.
    while remainder.count >= 4 {
      // This loop could be unrolled, obviously
      for _ in 0 ..< numericCast(remainder.count) >> 2 {
        guard let o = eat(parse(remainder, true)) else {
          return (remainder, errorCount)
        }
        output(o)
      }
    }

    // Handle whatever is left
    while true {
      guard let o = eat(parse(remainder, false)) else {
        return (remainder, errorCount)
      }
      output(o)
    }
  }
  // FIXME: without inducing a speed penalty, can we collapse the logic for
  // parseReverse and parseForward by using a reverse collection view and
  // changing the logic for the low level parsing routine to use forward
  // traversal?  Run the experiment.
}

extension UTF16 {
  /// Returns the decoded scalar value of a valid surrogate pair
  static func decodeValid(_ unit0: CodeUnit, _ unit1: CodeUnit) -> UInt32 {
    return 0x10000 + ((UInt32(unit0 & 0x03ff) << 10) | UInt32(unit1 & 0x03ff))
  }
  
  /// Parses one scalar forward from `input`.
  ///
  ///   - Parameter knownCountExceeds1: true if and only if the input is known
  ///   be at least 2 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Forward<C: Collection>(
    _ input: C, knownCountExceeds1: Bool = false
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit {

    if _slowPath(!knownCountExceeds1 && input.isEmpty) {
      return .emptyInput
    }
    let i0 = input.startIndex
    let unit0 = input[i0]
    let end = input.endIndex
    let i1 = input.index(after: i0)
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit0 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit0), resumptionPoint: i1)
    }

    // Ensure `unit0` is a high-surrogate and there's another byte which is a
    // low-surrogate
    if _fastPath(
      (unit0 >> 10) == 0b1101_10 && knownCountExceeds1 || i1 != end),
      let unit1 = Optional(input[i1]),
      _fastPath((unit1 >> 10) == 0b1101_11) {
      return .valid(
        EncodedScalar(unit0, unit1),
        resumptionPoint: input.index(after: i1)
      )
    }
    return .error(resumptionPoint: i1)
  }

  /// Parses one scalar in reverse from `input`.
  /// 
  ///   - Parameter knownCountExceeds1: true if and only if the input is known
  ///   be at least 2 elements long.  If so, we can skip end checks.  Note: pass
  ///   a compile-time constant here or you will just slow the algorithm down!
  static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C, knownCountExceeds1: Bool = false
  ) -> ParseResult<EncodedScalar, C.Index>
  where C.Iterator.Element == CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element
  {
    if _slowPath(!knownCountExceeds1 && input.isEmpty) { return .emptyInput }

    let i1 = input.index(before: input.endIndex)
    let unit1 = input[i1]
    
    // A well-formed pair of surrogates looks like this:
    //     high-surrogate        low-surrogate
    // [1101 10xx xxxx xxxx] [1101 11xx xxxx xxxx]

    // Common case first, non-surrogate -- just a sequence of 1 code unit.
    if _fastPath((unit1 >> 11) != 0b1101_1) {
      return .valid(EncodedScalar(unit1), resumptionPoint: i1)
    }
    let start = input.startIndex

    // Ensure `unit1` is a low-surrogate and there's another byte which is a
    // high-surrogate
    if _fastPath(
      (unit1 >> 10) == 0b1101_11 && knownCountExceeds1 || i1 != start),
      let i0 = Optional(input.index(before: i1)),
      let unit0 = Optional(input[i0]),
      _fastPath((unit0 >> 10) == 0b1101_10) {
      return .valid(
        EncodedScalar(unit0, unit1),        
        resumptionPoint: i0
      )
    }
    return .error(resumptionPoint: i1)
  }
}

extension UTF8 {
  static func _leading1s(_ x:UInt8) -> UInt8 {
    return UInt8(_leadingZeros((~x)._value))
  }
  
  struct EncodedScalar : RandomAccessCollection {
    let _bits: UInt32
    var startIndex: UInt8 { return 0 }
    
    var endIndex: UInt8 {
      let lowByte = UInt8(truncatingBitPattern: _bits)
      let lead1s = UInt64(_leading1s(lowByte))
      return UInt8(
        truncatingBitPattern: 0x040302FF01 >> (lead1s << 3))
    }
    
    init(_ _0: CodeUnit) {
      _bits = UInt32(_0)
    }
    
    init(_ _0: CodeUnit, _ _1: CodeUnit) {
      _bits = UInt32(_1) << 8 | UInt32(_0)
    }
    
    init(_ _0: CodeUnit, _ _1: CodeUnit, _ _2: CodeUnit) {
      _bits = (UInt32(_2) << 8 | UInt32(_1)) << 8 | UInt32(_0)
    }
    
    init(_ _0: CodeUnit, _ _1: CodeUnit, _ _2: CodeUnit, _ _3: CodeUnit) {
      _bits = ((UInt32(_3) << 8 | UInt32(_2)) << 8 | UInt32(_1)) << 8
        | UInt32(_0)
    }
    
    init(_bits: UInt32) {
      self._bits = _bits
    }
    
    typealias Index = UInt8
    subscript(i: Index) -> UInt8 {
      return UInt8(
        truncatingBitPattern: _bits >> (UInt32(i & (32 - 1)) << 3))
    }
  }
}

// UTF8 <-> UTF16
// ==============
//
//  U+0000...U+007F                                  0_xxxxxxx
//                                          00000000_0_xxxxxxx
//  U+0080...U+07FF                        110_wwwww 10_xxxxxx
//                                         00000_www_ww_xxxxxx
//  U+0800...U+FFFF:             1110_wwww 10_xxxxxx 10_yyyyyy
//                                         wwww_xxxx_xx_yyyyyy
// U+10000...U+10FFFF: 11110_www 10_xxxxxX 10_yyyyyy 10_zzzzzz
//                                 wwww_xxxx_xx_yy yyyy_zzzzzz
extension UTF8.EncodedScalar : EncodedScalarProtocol {
  var utf8: UTF8.EncodedScalar { return self }
  var utf16: UTF16.EncodedScalar {
    return utf32.utf16
  }
  var utf32: UTF32.EncodedScalar {
    if _fastPath(_bits <= 0x7f) {
      return UTF32.EncodedScalar(_bits: _bits)
    }
    var r = UInt32(UTF8.maskLeadByte(UInt8(truncatingBitPattern: _bits)))
    for b in self[1..<endIndex] {
      r <<= 6
      r |= UInt32(b)
    }
    return UTF32.EncodedScalar(_bits: r)
  }
}

extension UTF16 {
  struct EncodedScalar : RandomAccessCollection {
    let _bits: UInt32
    var startIndex: UInt8 { return 0 }
    
    var endIndex: UInt8 {
      return UInt8(truncatingBitPattern: self[1] >> 15) + 1
    }
    
    init(_ _0: CodeUnit) {
      _bits = UInt32(_0)
    }
    
    init(_ _0: CodeUnit, _ _1: CodeUnit) {
      _bits = UInt32(_1) << 16 | UInt32(_0)
    }
    
    init(_bits: UInt32) {
      self._bits = _bits
    }
    
    typealias Index = UInt8
    subscript(i: Index) -> UInt16 {
      return UInt16(
        truncatingBitPattern: _bits >> UInt32((i & 1) << 4))
    }
  }
}

extension UTF16.EncodedScalar : EncodedScalarProtocol {
  var utf8: UTF8.EncodedScalar {
    return utf32.utf8
  }
  var utf16: UTF16.EncodedScalar {
    return self
  }
  var utf32: UTF32.EncodedScalar {
    if _fastPath(_bits >> 16 == 0) {
      return UTF32.EncodedScalar(_bits)
    }
    let h = UInt32(self[0] &- 0xD800)
    let l = UInt32(self[1] &- 0xDC00) & (1 << 10 - 1)
    return UTF32.EncodedScalar(h << 10 + l + 0x10000)
  }
}

extension UTF32 {
  struct EncodedScalar : RandomAccessCollection {
    typealias Index = UInt8
    var startIndex: UInt8 { return 0 }
    var endIndex: UInt8 { return 1 }
    let _bits: UInt32
    init(_ _0: CodeUnit) {
      _bits = _0
    }
    init(_bits: UInt32) {
      self._bits = _bits
    }
    subscript(i: Index) -> UInt32 {
      return _bits
    }
  }
}

extension UTF32.EncodedScalar : EncodedScalarProtocol {
  var utf8: UTF8.EncodedScalar {
    if _fastPath(_bits <= 0x7f) { return UTF8.EncodedScalar(_bits: _bits) }
    if _fastPath(_bits <= 0x7ff) {
      return UTF8.EncodedScalar(
        0b110_00000 | UInt8(truncatingBitPattern: _bits >> 6),
        0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
      )
    }
    if _fastPath(_bits >> 16 == 0) {
      return UTF8.EncodedScalar(
        0b1110_0000 | UInt8(truncatingBitPattern: _bits >> 12),
        0b10_000000 | UInt8(truncatingBitPattern: _bits >> 6) & 0b00_111111,
        0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
      )
    }
    return UTF8.EncodedScalar(
      0b11110_000 | UInt8(truncatingBitPattern: _bits >> 18),
      0b10_000000 | UInt8(truncatingBitPattern: _bits >> 12) & 0b00_111111,
      0b10_000000 | UInt8(truncatingBitPattern: _bits >> 6) & 0b00_111111,
      0b10_000000 | UInt8(truncatingBitPattern: _bits) & 0b00_111111
    )
  }
  var utf16: UTF16.EncodedScalar {
    if _fastPath(_bits >> 16 == 0) {
      return UTF16.EncodedScalar(_bits: _bits)
    }
    let hl = _bits - 0x10000
    return UTF16.EncodedScalar(
      UInt16(truncatingBitPattern: hl >> 10 + 0xD800),
      UInt16(truncatingBitPattern: hl & (1 << 10 - 1) + 0xDC00)
    )
  }
  var utf32: UTF32.EncodedScalar {
    return self
  }
}

protocol UnicodeIndexBuffer : RandomAccessCollection {
  var utf8: UTF8.EncodedScalar { get }
  var utf16: UTF16.EncodedScalar { get }
  var utf32: UTF32.EncodedScalar { get }
  
  init(_ _0: UTF8.CodeUnit)
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit)
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit, _ _2: UTF8.CodeUnit)
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit,
    _ _2: UTF8.CodeUnit, _ _3: UTF8.CodeUnit)
  init(_ _0: UTF16.CodeUnit)
  init(_ _0: UTF16.CodeUnit, _ _1: UTF16.CodeUnit)
  init(_ _0: UTF32.CodeUnit)
  func isCompatible(with other: Self) -> Bool
  subscript(i: Index) -> UInt32 { get }
}

struct UnicodeIndexBuffer0 : UnicodeIndexBuffer {
  typealias Index = UInt8
  var startIndex: UInt8 { return 0 }
  var endIndex: UInt8 { return UInt8(bitPattern: count) }
  let count: Int8
  let _shift: UInt8
  let _bits: UInt32

  var utf8: UTF8.EncodedScalar {
    if _fastPath(_shift == 3) {
      return UTF8.EncodedScalar(_bits: _bits)
    }
    else if _fastPath(_shift == 4) {
      return utf16.utf8
    }
    return utf32.utf8
  }
  var utf16: UTF16.EncodedScalar {
    if _fastPath(_shift == 4) {
      return UTF16.EncodedScalar(_bits: _bits)
    }
    else if _fastPath(_shift == 3) {
      return utf8.utf16
    }
    return utf32.utf16
  }
  var utf32: UTF32.EncodedScalar {
    if _fastPath(_shift == 5) {
      return UTF32.EncodedScalar(_bits: _bits)
    }
    else if _fastPath(_shift == 3) {
      return utf8.utf32
    }
    return utf16.utf32
  }
  
  init(utf8: ()) {
    count = 0
    _shift = 3
    _bits = 0
  }
  init(utf16: ()) {
    count = 0
    _shift = 4
    _bits = 0
  }
  init(utf32: ()) {
    count = 0
    _shift = 5
    _bits = 0
  }
  init(_ _0: UTF8.CodeUnit) {
    count = 1
    _shift = 3
    _bits = UInt32(_0)
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit) {
    count = 2
    _shift = 3
    _bits = UInt32(_1) << 8 | UInt32(_0)
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit, _ _2: UTF8.CodeUnit) {
    count = 3
    _shift = 3
    _bits = (UInt32(_2) << 8 | UInt32(_1)) << 8 | UInt32(_0)
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit,
    _ _2: UTF8.CodeUnit, _ _3: UTF8.CodeUnit) {
    count = 4
    _shift = 3
    _bits = (
      (UInt32(_3) << 8 | UInt32(_2)) << 8 | UInt32(_1)
    ) << 8 | UInt32(_0)
  }
  init(_ _0: UTF16.CodeUnit) {
    count = 1
    _shift = 4
    _bits = UInt32(_0)
  }
  init(_ _0: UTF16.CodeUnit, _ _1: UTF16.CodeUnit) {
    count = 2
    _shift = 4
    _bits = UInt32(_1) << 16 | UInt32(_0)
  }
  init(_ _0: UTF32.CodeUnit) {
    count = 1
    _shift = 5
    _bits = _0
  }
  func isCompatible(with other: UnicodeIndexBuffer0) -> Bool {
    return self._shift == other._shift
  }
  subscript(i: Index) -> UInt32 {
    return _bits >> (UInt32(i << _shift) & (32 - 1))
    & ~0 >> (32 - UInt32(1 << _shift))
  }
}

enum UnicodeIndexBuffer1 : UnicodeIndexBuffer {
  typealias Index = UInt8
case utf8(UTF8.EncodedScalar)
case utf16(UTF16.EncodedScalar)
case utf32(UTF32.EncodedScalar)
  
  var startIndex: UInt8 { return 0 }
  var endIndex: UInt8 {
    switch self {
    case .utf8(let x): return x.endIndex
    case .utf16(let x): return x.endIndex
    case .utf32(let x): return x.endIndex
    }
  }
  var count: Int {
    switch self {
    case .utf8(let x): return x.count
    case .utf16(let x): return x.count
    case .utf32(let x): return x.count
    }
  }
  init(_ _0: UTF8.CodeUnit) {
    self = .utf8(UTF8.EncodedScalar(_0))
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit) {
    self = .utf8(UTF8.EncodedScalar(_0, _1))
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit, _ _2: UTF8.CodeUnit) {
    self = .utf8(UTF8.EncodedScalar(_0, _1, _2))
  }
  init(_ _0: UTF8.CodeUnit, _ _1: UTF8.CodeUnit,
    _ _2: UTF8.CodeUnit, _ _3: UTF8.CodeUnit) {
    self = .utf8(UTF8.EncodedScalar(_0, _1, _2, _3))
  }
  var utf8: UTF8.EncodedScalar {
    switch self {
    case .utf8(let x): return x.utf8
    case .utf16(let x): return x.utf8
    case .utf32(let x): return x.utf8
    }
  }
  var utf16: UTF16.EncodedScalar {
    switch self {
    case .utf8(let x): return x.utf16
    case .utf16(let x): return x.utf16
    case .utf32(let x): return x.utf16
    }
  }
  var utf32: UTF32.EncodedScalar {
    switch self {
    case .utf8(let x): return x.utf32
    case .utf16(let x): return x.utf32
    case .utf32(let x): return x.utf32
    }
  }
  init(_ _0: UTF16.CodeUnit) {
    self = .utf16(UTF16.EncodedScalar(_0))
  }
  init(_ _0: UTF16.CodeUnit, _ _1: UTF16.CodeUnit) {
    self = .utf16(UTF16.EncodedScalar(_0, _1))
  }
  init(_ _0: UTF32.CodeUnit) {
    self = .utf32(UTF32.EncodedScalar(_0))
  }
  func isCompatible(with other: UnicodeIndexBuffer1) -> Bool {
    switch (self, other) {
    case (.utf8(_), .utf8(_)): return true
    case (.utf16(_), .utf16(_)): return true
    case (.utf32(_), .utf16(_)): return true
    default: return false
    }
  }
  subscript(i: Index) -> UInt32 {
    switch(self) {
    case .utf8(let x): return UInt32(x[i])
    case .utf16(let x): return UInt32(x[i])
    case .utf32(let x): return UInt32(x[i])
    }
  }
}

/// An Index type that can be used to index code units, and UTF8 and UTF16 views
/// thereof.
///
/// - Parameter Base: an Index type for a base BidirectionalCollection of code
///   units.
///
/// - Parameter Offset: an unsigned integer type that can represent at least
///   numbers in the range 0..<4.
struct UnicodeIndex<
  Base: Comparable, Offset: UnsignedInteger, Buffer: UnicodeIndexBuffer
> : Comparable {
  init(base: Base, maxOffsetInTranscodedScalar: Offset, buffer: Buffer) {
    self.base = base
    self.buffer = buffer
    self.offsetInTranscodedScalar = 0
    self.maxOffsetInTranscodedScalar = maxOffsetInTranscodedScalar
  }

  internal func _checkCompatibility(with other: UnicodeIndex) {
    _debugPrecondition(
      offsetInTranscodedScalar == 0
      || other.offsetInTranscodedScalar == 0
      || buffer.isCompatible(with: other.buffer),
      "Can't mix indices that don't sit on unicode scalar boundaries or index the same transcoding.")
  }
  
  static func <(l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    l._checkCompatibility(with: r)
    return l.base < r.base
      || l.base == r.base
         && l.offsetInTranscodedScalar < r.offsetInTranscodedScalar
  }

  static func ==(l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    l._checkCompatibility(with: r)
    return l.base == r.base
      && l.offsetInTranscodedScalar == r.offsetInTranscodedScalar
  }

  /// an index into underlying CodeUnits, pointing at a unicode scalar.
  var base: Base

  /// an offset into a transcoded UTF8 or UTF16 representation of the scalar
  var offsetInTranscodedScalar: Offset

  /// the number of bytes in the transcoded scalar, minus 1
  var maxOffsetInTranscodedScalar: Offset

  var buffer: Buffer
}

% for bits in set([2, 3, 62, 63, WORD_BITS - 4]):
%   signed = False
%   (sign, ext) = ('u', 'zext')
%   Self = 'UInt%s' % bits
%   BuiltinName = 'Int%s' % bits
struct ${Self} {
  init(_ storage: Builtin.${BuiltinName}) {
    self._storage = storage
  }
  init(_ value: Int) {
    self.init(UInt64(bitPattern: Int64(value)))
  }
  init(_ value: UInt) {
    self.init(UInt64(value))
  }
  init(_ value: UInt64) {
    _precondition(value >> ${bits} == 0, "${Self} can't represent value")
    _storage = Builtin.trunc_Int64_Int${bits}(value._value)
  }

  var _storage: Builtin.Int${bits}
}

extension ${Self} : Equatable {
  static func ==(l: ${Self}, r: ${Self}) -> Bool {
    return l.toUIntMax() == r.toUIntMax()
  }
}
extension ${Self} : Comparable {
  static func <(l: ${Self}, r: ${Self}) -> Bool {
    return l.toUIntMax() < r.toUIntMax()
  }
}
extension ${Self} : _ExpressibleByBuiltinIntegerLiteral {
  @_transparent public
  init(_builtinIntegerLiteral value: Builtin.Int${builtinIntLiteralBits}) {
    self._storage = Builtin.s_to_u_checked_trunc_Int${builtinIntLiteralBits}_${BuiltinName}(value).0
  }
}
extension ${Self} : ExpressibleByIntegerLiteral {
  init(integerLiteral value: ${Self}) {
    self = value
  }
}
extension ${Self} : CustomStringConvertible {
  var description: String {
    return self.toIntMax().description
  }
}
extension ${Self} : Hashable {
  public var hashValue: Int {
    return self.toIntMax().hashValue
  }
}
extension ${Self} : IntegerArithmetic {
  public func toIntMax() -> IntMax { return numericCast(self) }
% for Method, op in [('add', 'add'), ('subtract', 'sub'), ('multiply', 'mul')]:
  @_transparent public
  static func ${Method}WithOverflow(_ lhs: ${Self}, _ rhs: ${Self}) -> (${Self}, overflow: Bool) {
    let tmp = Builtin.${sign}${op}_with_overflow_${BuiltinName}(lhs._storage, rhs._storage, false._value)
    return (${Self}(tmp.0), Bool(tmp.1))
  }
% end

% for Method, op in [('divide', 'div'), ('remainder', 'rem')]:
  @_transparent public
  static func ${Method}WithOverflow(_ lhs: ${Self}, _ rhs: ${Self}) -> (${Self}, overflow: Bool) {
    if rhs == 0 {
      return (0, true)
    }
    let tmp = Builtin.${sign}${op}_${BuiltinName}(lhs._storage, rhs._storage)
    return (${Self}(tmp), false)
  }
%end
}
extension ${Self} : BitwiseOperations {
% for op, name, comment in (
%   ('&', 'and', 'intersection of bits set in'),
%   ('^', 'xor', 'bits that are set in exactly one of'),
%   ('|', 'or', 'union of bits set in'),
% ):
@_transparent
  public static func ${op} (lhs: ${Self}, rhs: ${Self}) -> ${Self} {
    return ${Self}(Builtin.${name}_${BuiltinName}(lhs._storage, rhs._storage))
  }
% end
  @_transparent
  prefix public static func ~ (rhs: ${Self}) -> ${Self} {
    let mask = ${Self}.subtractWithOverflow(0, 1).0
    return ${Self}(Builtin.xor_${BuiltinName}(rhs._storage, mask._storage))
  }
  public static var allZeros: ${Self} { return 0 }

  % for op, name in (('<<','shl'), ('>>', 'lshr')):
  @_transparent
  public static func ${op} (lhs: ${Self}, rhs: ${Self}) -> ${Self} {
    _precondition(rhs < ${bits},
      "shift amount is larger than type size in bits")
    return ${Self}(Builtin.${name}_${BuiltinName}(lhs._storage, rhs._storage))
  }
% end
}
extension ${Self} : UnsignedInteger {
  func toUIntMax() -> UIntMax {
    return UIntMax(Builtin.zext_${BuiltinName}_Int64(_storage))
  }
}

% end

typealias UIntWordBitsMinus4 = UInt${WORD_BITS - 4}

/// A collection that has an underlying collection of code units and an
/// encoding.  Strings will conform to this protocol so that pattern matching
/// and other facilities can probe for information that will allow them to do
/// their work more efficiently.  For example, a pattern that's represented as
/// UTF8 might probe the string being searched to see if it has a compatible
/// representation, in which case we might be able to bypass transcoding.
///
/// To operate on ordinary collections, a wrapper with Encoding == Void and
/// CodeUnits == EmptyCollection<Void> can be used.
protocol EncodedCollection : Collection {
  associatedtype Encoding
  associatedtype CodeUnits : Collection
  var codeUnits : CodeUnits { get }
}

/// Unicode is a bidirectional EncodedCollection of UnicodeScalars
protocol Unicode : EncodedCollection, BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec
  associatedtype Scalars : BidirectionalCollection
  // where Iterator.Element == UnicodeScalar
  associatedtype UTF8Units : EncodedCollection
}

/// Strings are `BidirectionalCollection`s of `Character` having whose `Index` type is
/// `StringIndex`
protocol StringProtocol : BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec
}


/// Storage for string representations.
///
/// Requires: CodeUnit is trivial.  In practice it will be UInt16 or UInt8.
///
/// Capacity is parameterized because we don't really want to store anything
/// when the storage is immutable (e.g., storage for Character)
final class StringStorage<CodeUnit, Capacity>
  : ManagedBuffer<(count: Int, capacity: Capacity), CodeUnit> {

  // FIXME: we should find a way to share code with ContiguousArrayBuffer.  We
  // can't just use it directly because the storage class for Arrays has to be-a
  // NSArray, while we (eventually) want this class to be-a NSString.  That also
  // means we'll have to stop inheriting ManagedBuffer and used
  // ManagedBufferPointer (or _ManagedBufferPointer if it's internalized)
  // instead.

  /// Makes one.
  ///
  /// Don't forget to assign something to storedCapacity if you're going to use
  /// it!
  class func create(
    minimumCapacity: Int,
    count: Int = 0,
    storedCapacity:
      (ManagedBuffer<(count: Int, capacity: Capacity), CodeUnit>)->Capacity
  ) -> StringStorage {
    let r = super.create(minimumCapacity: minimumCapacity) {
      (count: count, capacity: storedCapacity($0))
    }
    return unsafeDowncast(r, to: StringStorage<CodeUnit, Capacity>.self)
  }

  var count : Int {
    get {
      return withUnsafeMutablePointerToHeader { $0.pointee.count }
    }
    set {
      withUnsafeMutablePointerToHeader { $0.pointee.count = newValue }
    }
  }

  var storedCapacity : Capacity {
    get {
      return withUnsafeMutablePointerToHeader { $0.pointee.capacity }
    }
    set {
      withUnsafeMutablePointerToHeader { $0.pointee.capacity = newValue }
    }
  }
}

extension StringStorage : MutableCollection {
  typealias Index = UIntWordBitsMinus4

  var startIndex : Index { return Index(0) }
  var endIndex : Index { return Index(count) }

  subscript(i: Index) -> CodeUnit {
    get {
      return withUnsafeMutablePointerToElements {
        $0[numericCast(i)]
      }
    }
    set {
      withUnsafeMutablePointerToElements {
        $0[numericCast(i)] = newValue
      }
    }
  }
  public func index(after i: Index) -> Index { return i + (1 as Index) }
}

extension StringStorage : RandomAccessCollection {
  public func index(before i: Index) -> Index { return i - (1 as Index) }
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return numericCast(numericCast(i) + n)
  }

  public subscript(bounds: Range<Index>)
  -> MutableRandomAccessSlice<StringStorage> {
    get {
      return MutableRandomAccessSlice(base: self, bounds: bounds)
    }
    set {
      // Reference semantics means we might already be done.
      if newValue.base === self { return }

      let targetIndices = bounds.lowerBound..<bounds.upperBound
      _sanityCheck(targetIndices.count == newValue.count)
      for (i, e) in zip(targetIndices, newValue) {
        self[i] = e
      }
    }
  }
}
//===--- end StringStorage ------------------------------------------------===//

struct Character {
  enum Content {
    typealias Storage = StringStorage<UTF16.CodeUnit, ()>
  case inline(UInt63) // Up to 4 UTF16 code units
  case allocated(Storage)
  }
}

extension Character.Content : RandomAccessCollection {
  typealias Index = UIntWordBitsMinus4

  // FIXME: should add something that reads from a string until the next
  // grapheme cluster boundary.  That would be the usual way to construct these
  // and would avoid pre-measuring to find the boundary.

  // FIXME: consider making a custom iterator for this.  If we have an inline
  // representation it could be lots faster than IndexingIterator

  // FIXME: get someone who knows the unicode planes really well to tell us
  // whether the inline encoding makes good use of bits, especially since we are
  // dropping the top bit of the last element (e.g. maybe we should do that with
  // the top bit of the first element).
  init<C: Collection>(_ source: C)
  where C.Iterator.Element == UTF16.CodeUnit {
    var i = source.makeIterator()
    let u0 = UInt64(i.next()!) // A character is never empty
    var u1: UInt64 = 0xFFFF_0000
    var u2: UInt64 = 0xFFFF_0000_0000
    var u3: UInt64 = 0x7FFF_0000_0000_0000
    if let u1_ = i.next() {
      u1 = UInt64(u1_) << 16
      if let u2_ = i.next() {
        u2 = UInt64(u2_) << 32
        if let u3_ = i.next() {
          u3 = i.next() != nil ? UInt64(u3_) << 48 : ~0
        }
      }
    }
    if Int64(bitPattern: u3) < 0 {
      self = .inline(UInt63(u3 | u2 | u1 | u0))
    }
    else {
      let n: Int = numericCast(source.count)
      let storage = Storage.create(minimumCapacity: n, count: n) { _ in () }
      for (i, x) in zip(storage.indices, source) {
        storage[i] = x
      }
      self = .allocated(storage)
    }
  }

  var startIndex: Index { return Index(0) }

  var endIndex: Index {
    switch self {
    case .allocated(let storage): return storage.endIndex
    case .inline(let bits):
      if bits >= 0x7fff_ffff_ffff_0000 { return Index(1) }
      if bits >= 0x7fff_ffff_0000_0000 { return Index(2) }
      if bits >= 0x7fff_0000_0000_0000 { return Index(3) }
      return Index(4)
    }
  }

  subscript(i: Index) -> UTF16.CodeUnit {
    switch self {
    case .allocated(let storage): return storage[i]
    case .inline(let bits):
      let _16i = (numericCast(i) as UInt) << 4
      return UTF16.CodeUnit(
        truncatingBitPattern: numericCast(bits) >> _16i)
    }
  }

  func index(after i: Index) -> Index {
    return i + (1 as Index)
  }
  func index(before i: Index) -> Index {
    return i - (1 as Index)
  }
  func index(_ i: Index, offsetBy n: Int) -> Index {
    return numericCast(numericCast(i) + n)
  }
}

/*
extension Character : StringProtocol {

}

final class StringBuffer : ManagedBuffer<(count: ), UInt16> {
  static func create(minimumCapacity: Int) -> StringBuffer {
    return super.create(
      minimumCapacity: minimumCapacity, makingHeaderWith: { _ in () })
    as! StringBuffer
  }

  enum Encoding {
  case ascii
  case latin1
  case utf16
  }
  var encoding: Encoding = .latin1
  var count: Int = 42
  var storageCapacity: Int = 0
}

class AnyStringBase  {
  typealias Encoding = UTF32
}

class AnyString<T: StringProtocol> : AnyStringBase {
  init(_ x: T) {
    stored = x
  }
  var stored: T
}

// Approximates Builtin.Int62, which fits without growing String past
// 64 bits.
enum SixBits : UInt8 {
case _00, _01, _02, _03, _04, _05, _06, _07
case _10, _11, _12, _13, _14, _15, _16, _17
case _20, _21, _22, _23, _24, _25, _26, _27
case _30, _31, _32, _33, _34, _35, _36, _37
case _40, _41, _42, _43, _44, _45, _46, _47
case _50, _51, _52, _53, _54, _55, _56, _57
case _60, _61, _62, _63, _64, _65, _66, _67
case _70, _71, _72, _73, _74, _75, _76, _77
}
typealias Int62 = (UInt32, UInt16, UInt8, SixBits)

struct SmallString {
  var bits: Int62
}

// 64 bits
struct String : StringProtocol {
  enum Content {
  case small(SmallString)
  case large(StringBuffer)
  case any(AnyStringBase)
  }
  var content: Content

  typealias Index = StringIndex

  var startIndex: Index { return Index(offset: 0) }
  var endIndex: Index {
    fatalError()
  }
  subscript(_: Index) -> SubString { fatalError() }
  public func index(after i: Index) -> Index { fatalError() }
  public func index(_ i: Index, offsetBy n: Int) -> Index { fatalError() }
//  public func index(_ i: Self.Index, offsetBy n: Self.IndexDistance, limitedBy limit: Self.Index) -> Self.Index?
  typealias Encoding = Void  // No statically-known encoding
  var codeUnits : EmptyCollection<()>? { return nil }
}

// 16 bytes 32-bit platforms and 24 bytes on 64-bit platforms
struct SubString {
  enum Content {
  case smallish(Int62, UInt, UInt) // Up to 190 bits including offset
  case large(String, String.Index, String.Index)
  }
  var content: Content
}

let _=print(StringBuffer.create(minimumCapacity: 20))
let _=print(MemoryLayout<String>.size, MemoryLayout<String>.stride)
let _=print(MemoryLayout<SubString>.size, MemoryLayout<SubString>.stride)

*/
