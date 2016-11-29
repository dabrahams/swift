// RUN: rm -rf %t && mkdir -p %t && %gyb -DWORD_BITS=%target-ptrsize %s -o %t/out.swift
// RUN: %line-directive %t/out.swift -- %target-build-swift -parse-stdlib -Xfrontend -disable-access-control %t/out.swift -o %t/a.out -Onone
// RUN: %line-directive %t/out.swift -- %target-run %t/a.out
import Swift


%{
  WORD_BITS = int(WORD_BITS)
  builtinIntLiteralBits = 2048
}%


enum ParseResult<T, Index> {
case valid(T, resumptionPoint: Index)
case error(resumptionPoint: Index)
case emptyInput
}

//===--- Bidirectional UTF-8 Decoding -------------------------------------===//
// See https://gist.github.com/dabrahams/1880044370a192ae51c263a93f25a4c5 for
// tests that show these decoders are correct.
//
// To optimize continuous parsing:
// * Add a flag that says "don't check for the end"
// * Unroll count/4 invocations, passing the flag
// * Repeat until count < 4
//
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

  static func leading1s(_ x:UInt8) -> UInt8 {
    return UInt8(_leadingZeros((~x)._value))
  }

  static func maskLeading1s(_ x: UInt8) -> UInt8 {
    return x & ((1 << (7 &- leading1s(x))) - 1)
  }

  /// Parses one scalar forward from `input`.
  static func parse1Forward<C: Collection>(
    _ input: C
  ) -> ParseResult<UInt32, C.Index> where C.Iterator.Element == UTF8.CodeUnit {

    if input.isEmpty { return .emptyInput }

    var i = input.startIndex 
    let u0 = input[i]
    var j = input.index(after: i)
    
    if _fastPath(Int8(bitPattern: u0) >= 0) {
      return .valid(UInt32(u0), resumptionPoint: j)
    }
    i = j // even if there are errors, we eat 1 byte

    // Begin accumulating result
    var r = UInt32(maskLeading1s(u0))

    let end = input.endIndex
    
    // Mark one more token recognized and get the next lookahead token iff it
    // falls within pattern
    @inline (__always)
    func nextContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      i = j
      if _slowPath(j == end) { return false } // no more tokens
      let u = input[j]
      if _fastPath(pattern ~= u) {
        r = r << 6 | UInt32(u & 0b00_111111)
        j = input.index(after: j)
        return true
      }
      return false
    }
    
    @inline(__always)
    func state7() -> ParseResult<UInt32, C.Index> {
      return nextContinuation(0x80...0xbf)
        ? .valid(r, resumptionPoint: j) : .error(resumptionPoint: i)
    }
    
    @inline(__always)
    func state3() -> ParseResult<UInt32, C.Index> {
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
  static func parse1Reverse<C: BidirectionalCollection>(
    _ input: C
  ) -> ParseResult<UInt32, C.Index>
  where C.Iterator.Element == UTF8.CodeUnit,
  // FIXME: drop these constraints once we have the compiler features.
  C.SubSequence.Index == C.Index,
  C.SubSequence.Iterator.Element == C.Iterator.Element {

    if input.isEmpty { return .emptyInput }

    var i = input.index(before: input.endIndex)
    var j = i
    let j0 = j
    var u = input[j]
    if _fastPath(Int8(bitPattern: u) >= 0) {
      return .valid(UInt32(u), resumptionPoint: j)
    }
    
    let start = input.startIndex
    var r: UInt32 = 0
    var shift: UInt32 = 0

    // Mark one more token recognized and get the next lookahead token iff it
    // satisfies the predicate
    @inline(__always)
    func consumeContinuation(_ pattern: ClosedRange<UInt8>) -> Bool {
      guard _fastPath(pattern ~= u) else { return false }
      i = j
      guard j != start else { return false }
      r |= UInt32(u & 0b00_111111) << shift
      shift += 6
      j = input.index(before: j)
      u = input[j]
      return true
    }

    @inline(__always)
    func accept(_ pat: ClosedRange<UInt8>) -> ParseResult<UInt32, C.Index>? {
      if _fastPath(pat.contains(u)) {
        r |= UInt32(maskLeading1s(u)) << shift
        return .valid(r, resumptionPoint: j)
      }
      return nil
    }
    
    @inline(__always)
    func state4_5() -> ParseResult<UInt32, C.Index> {
      return accept(0xf0...0xf3) ?? .error(resumptionPoint: j0)
    }
    
    @inline(__always)
    func state5_6() -> ParseResult<UInt32, C.Index> {
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
}

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

/// An Index type that can be used to index code units, and UTF8 and UTF16 views
/// thereof.
///
/// - Parameter Base: an Index type for a base BidirectionalCollection of code
///   units.
///
/// - Parameter Offset: an unsigned integer type that can represent at least
///   numbers in the range 0..<4.
struct UnicodeIndex<Base: Comparable, Offset: UnsignedInteger> : Comparable {
  init(base: Base, maxOffsetInTranscodedScalar: Offset) {
    self.base = base
    self.offsetInTranscodedScalar = 0
    self.maxOffsetInTranscodedScalar = maxOffsetInTranscodedScalar
  }

  static func <(l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    _debugPrecondition(
      l.base != r.base
      || l.maxOffsetInTranscodedScalar == r.maxOffsetInTranscodedScalar,
      "Can't mix indices that don't sit on unicode scalar boundaries or index the same transcoding.")
    return l.base < r.base
        || l.base == r.base
	         && l.offsetInTranscodedScalar < r.offsetInTranscodedScalar
  }

  static func ==(l: UnicodeIndex, r: UnicodeIndex) -> Bool {
    _debugPrecondition(
      l.base != r.base
      || l.maxOffsetInTranscodedScalar == r.maxOffsetInTranscodedScalar,
      "Can't mix indices that don't sit on unicode scalar boundaries or index the same transcoding.")
    return l.base == r.base
	         && l.offsetInTranscodedScalar == r.offsetInTranscodedScalar
  }

  // FIXME: we may want to augment this with a cache of transcoded units.

  /// an index into underlying CodeUnits, pointing at a unicode scalar.
  var base: Base

  /// an offset into a transcoded UTF8 or UTF16 representation of the scalar
  var offsetInTranscodedScalar: Offset

  /// the number of bytes in the transcoded scalar, minus 1
  var maxOffsetInTranscodedScalar: Offset
}

// A specialized version of UnicodeIndex that might be easier to compile, but
// uses a fixed layout
struct StringIndex : Comparable {
  init(offsetOfScalar: Int, countOfTranscodedScalar: Int) {
    let v = countOfTranscodedScalar &- 1
    _debugPrecondition(
      v & ~0b11 == 0, "countOfTranscodedScalar must be between 1 and 4")
    representation = UInt(offsetOfScalar) << 4 | (UInt(bitPattern: v) << 2)
  }

  // the offset into the underlying CodeUnits, whatever those may be
  private var representation: UInt

  public var offsetOfScalar: Int {
    get {
      return Int(bitPattern: representation >> 4)
    }
    set {
      representation = (representation & 0b1111) | numericCast(newValue) << 4
    }
  }

  public var countOfTranscodedScalar: Int {
    get {
      return Int(bitPattern: (representation >> 2) & 0b11) &+ 1
    }
    set {
      let v = newValue &- 1
      _debugPrecondition(
        v & ~0b11 == 0, "countOfTranscodedScalar must be between 1 and 4")
      representation = (representation & ~0b1100) | UInt(bitPattern: v << 2)
    }
  }

  public var offsetInTranscodedScalar: Int {
    get {
      return Int(bitPattern: representation & 0b11)
    }
    set {
      _debugPrecondition(
        newValue & ~0b11 == 0,
        "offsetInTranscodedScalar must be between 0 and 3")
      representation = (representation & ~0b11) | UInt(bitPattern: newValue)
    }
  }

  static func <(l: StringIndex, r: StringIndex) -> Bool {
    _debugPrecondition(
      (l.representation ^ r.representation) & 0b1100 == 0
      || (l.representation | r.representation) & 0b11 == 0,
      "Can't mix indices that don't sit on unicode scalar boundaries or index the same transcoding.")
    return l.representation < r.representation
  }

  static func ==(l: StringIndex, r: StringIndex) -> Bool  {
    _debugPrecondition(
      (l.representation ^ r.representation) & 0b1100 == 0
      || (l.representation | r.representation) & 0b11 == 0,
      "Can't mix indices that don't sit on unicode scalar boundaries or index the same transcoding.")
    return l.representation == r.representation
  }
}

% for bits in set([2, 62, 63, WORD_BITS - 4]):
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

protocol Unicode : EncodedCollection, BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec
  associatedtype Scalars : BidirectionalCollection
  // where Iterator.Element == UnicodeScalar
  associatedtype UTF8Units : EncodedCollection
}

/// Strings are `BidirectionalCollection`s of `Character` whose `Index` type is
/// `StringIndex`
protocol StringProtocol
  : EncodedCollection, BidirectionalCollection {
  associatedtype CodeUnits : BidirectionalCollection
  associatedtype Encoding : UnicodeCodec

  // This wouldn't be here; it's really a trick to (weakly) simulate
  // the ability to constrain our Index type to be String.Index and
  // our element to be Character.
  associatedtype Index = StringIndex
  subscript(_: StringIndex) -> Character { get }
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
