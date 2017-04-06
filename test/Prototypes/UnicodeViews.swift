//===--- UnicodeViews.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

public enum AnyUnicodeIndex : Comparable {
case encodedOffset(Int)
  
case transcoded(
    inputOffset: Int,
    outputOffset: Int,
    outputEncoding: Any.Type)
  
case character(
    encodedOffset: Int,
    width: Int)
  
case unicodeScalar(encodedOffset: Int, width: Int, scalar: UnicodeScalar?)

  var encodedOffset: Int {
    switch self {
    case .encodedOffset(let x): return x
    case .transcoded(let x, _,_): return x
    case .character(let x, _): return x
    case .unicodeScalar(let x, _, _): return x
    }
  }

  public static func < (lhs: AnyUnicodeIndex, rhs: AnyUnicodeIndex) -> Bool {
    let l = lhs.encodedOffset
    let r = rhs.encodedOffset
    if _fastPath(l < r) { return true }
    if _fastPath(l > r) { return false }
    switch (lhs, rhs) {
    case (.transcoded(_, let l, let tl), (.transcoded(_, let r, let tr))):
      return tl == tr && l < r
    case (.character(_, let l), .character(_, let r)):
      return l < r
    default:
      return false
    }
  }

  public static func == (lhs: AnyUnicodeIndex, rhs: AnyUnicodeIndex) -> Bool {
    let l = lhs.encodedOffset
    let r = rhs.encodedOffset
    if _fastPath(l != r) { return false }
    switch (lhs, rhs) {
    case (.transcoded(_, let l, let tl), (.transcoded(_, let r, let tr))):
      return tl != tr || l == r
    case (.character(_, let l), .character(_, let r)):
      return l == r
    default:
      return true
    }
  }
}

public func __swift_stdlib_U_SUCCESS(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue <= __swift_stdlib_U_ZERO_ERROR.rawValue
}

public func __swift_stdlib_U_FAILURE(_ x: __swift_stdlib_UErrorCode) -> Bool {
 return x.rawValue > __swift_stdlib_U_ZERO_ERROR.rawValue
}

/// Unicode views should conform to this protocol, which supports index
/// interchange and String type erasure.
public protocol _UnicodeView : BidirectionalCollection {
  func nativeIndex(_: AnyUnicodeIndex) -> Index
  func anyIndex(_ : Index) -> AnyUnicodeIndex
}

extension _UnicodeView {
  /// Constructs a copy of other
  public init(_ other: Self) { self = other }
}

/// A UnicodeView that is already using AnyUnicodeIndex has trivial
/// interchange
extension _UnicodeView where Index == AnyUnicodeIndex {
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index { return x }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex { return x }
}

public protocol UnicodeView : _UnicodeView {
  associatedtype SubSequence: _UnicodeView = UnicodeViewSlice<Self>
}

//===----------------------------------------------------------------------===//
public struct UnicodeViewSlice<BaseView: _UnicodeView>
  : BidirectionalCollectionWrapper, UnicodeView {
  public typealias Base = BidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = UnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RandomAccessUnicodeViewSlice<
  BaseView: _UnicodeView & RandomAccessCollection
> : BidirectionalCollectionWrapper, UnicodeView, RandomAccessCollection {
  public typealias Base = RandomAccessSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = RandomAccessUnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RangeReplaceableUnicodeViewSlice<
  BaseView: _UnicodeView & RangeReplaceableCollection
> : BidirectionalCollectionWrapper, UnicodeView, RangeReplaceableCollection {
  public typealias Base = RangeReplaceableBidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = RangeReplaceableUnicodeViewSlice<BaseView>
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
  public mutating func replaceSubrange<C: Collection>(
    _ r: Range<Index>, with replacement: C
  )
  where C.Iterator.Element == Iterator.Element {
    return base.replaceSubrange(r,  with: replacement)
  }
  init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  
  public init() {
    base = Base()
  }
  public mutating func _tryToReplaceSubrange<C>(
    _ target: Range<Index>,
    with replacement: C) -> Bool
  where C : Collection, C.Iterator.Element == Iterator.Element {
    return base._tryToReplaceSubrange(target, with: replacement)
  }
}
//===----------------------------------------------------------------------===//

//===--- RandomAccessUnicodeView ------------------------------------------===//
/// Adapts any `RandomAccessCollection` to a `UnicodeView`, with
/// `encodedOffset`s equal to the number of index steps from the `startIndex`.
///
/// Computing `encodedOffset` this way is pretty safe because if the base view
/// has random access, it must have a constant number N of elements per code
/// unit, and in all the usual instances, N = 1
public struct RandomAccessUnicodeView<Base_: RandomAccessCollection> {
  public typealias Base = Base_
  public typealias Iterator = Base.Iterator
  public var base: Base
  public var _basey: Base { return base }
  public init(_ base: Base) { self.base = base }
}

extension RandomAccessUnicodeView : BidirectionalCollectionWrapper {
  public struct Index : ForwardingWrapper, Comparable {
    public var base: Base_.IndexDistance
  }
  public typealias IndexDistance = Base.IndexDistance
  public func _wrap(_ i: Base_.Index) -> Index {
    return Index(base: base.offset(of: i))
  }
  public func _unwrap(_ i: Index) -> Base.Index {
    return base.index(atOffset: i.base)
  }
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    let b: Base_ = base
    let d: Base_.IndexDistance = x.encodedOffset^
    return b.index(b.startIndex, offsetBy: d)
  }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    let b: Base_ = base
    return .encodedOffset(b.offset(of: x)^)
  }
}

extension RandomAccessUnicodeView {
  public mutating func _tryToReplaceSubrange<C: Collection>(
    _ target: Range<Index>, with replacement: C
  ) -> Bool
  where C.Iterator.Element == Iterator.Element {
    // UnicodeViews must have value semantics.  Note that this check will fail
    // to work if the view being wrapped is itself a wrapper that forwards
    // _tryToReplaceSubrange to an underlying reference type.
    if Base_.self is AnyObject.Type {
      if (
        withUnsafeMutablePointer(to: &base) {
          !_isUnique(&UnsafeMutableRawPointer($0).assumingMemoryBound(
              to: AnyObject.self).pointee)
        }
      ) { return false }
    }
    return base._tryToReplaceSubrange(
      _unwrap(target.lowerBound)..<_unwrap(target.upperBound),
      with: replacement)
  }
}

extension RandomAccessUnicodeView : RandomAccessCollection {}

extension RandomAccessUnicodeView : UnicodeView {
  public typealias SubSequence
  = RandomAccessUnicodeViewSlice<RandomAccessUnicodeView>
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }  
}

//===----------------------------------------------------------------------===//

/// A collection of `CodeUnit`s to be interpreted by some `Encoding`.
///
/// View types nested in _UnicodeViews may be suitable *generic* implementation
/// guts for views for models of Unicode, but specific models may want to
/// provide their own implementations.  For example, the UTF16 view of a
/// Latin1String would might be a simple lazy zero-extended mapping, rather than
/// something that goes through the transcoding machinery.
public struct _UnicodeViews<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {

  public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
    self.codeUnits = codeUnits
  }

  public var codeUnits: CodeUnits
}

//===--- Helper typealias -------------------------------------------------===//
/// A straightforward typealias for _UnicodeViews
///
/// Use this to escape the automatic deduction of the generic arguments given
/// the name `_UnicodeViews` from within nested contexts
/// (https://bugs.swift.org/browse/SR-4155).
internal typealias _UnicodeViews_<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>  = _UnicodeViews<CodeUnits, Encoding>
where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
//===----------------------------------------------------------------------===//


protocol _UnicodeScalarMapping {
  associatedtype Output
  associatedtype InputEncoding : UnicodeEncoding
  func transform(_ input: UnicodeScalar)->Output?
  func transform(_ input: InputEncoding.EncodedScalar)->Output?
  func transform(_ output: Output)->UnicodeScalar
}

//===----------------------------------------------------------------------===//
// _UnicodeViews._MappedScalars
//===----------------------------------------------------------------------===//
/// A lazy collection of the source encoded scalars that for which Transform
/// doesn't return nil.
extension _UnicodeViews {
  public struct _MappedScalars<Mapping: _UnicodeScalarMapping>
  where Mapping.InputEncoding == Encoding {
    public var codeUnits: CodeUnits
    public var mapping: Mapping
    
    public typealias Self_ = _MappedScalars
    
    public init(
      _ codeUnits: CodeUnits,
      mapping: Mapping, _: Encoding.Type = Encoding.self
    ) {
      self.codeUnits = codeUnits
      self.mapping = mapping
    }
  }

  struct _EncodedScalarIdentity : _UnicodeScalarMapping {
    typealias Output = Encoding.EncodedScalar
    typealias InputEncoding = Encoding
    func transform(_ input: UnicodeScalar) -> Encoding.EncodedScalar? {
      return Encoding.EncodedScalar(input)
    }
    func transform(_ input: Encoding.EncodedScalar) -> Encoding.EncodedScalar? {
      return input
    }
    func transform(_ output: Output) -> UnicodeScalar {
      return UnicodeScalar(output)
    }
  }
  
  typealias EncodedScalars = _MappedScalars<_EncodedScalarIdentity>
  
  public var encodedScalars: EncodedScalars {
    return EncodedScalars(
      codeUnits, mapping: _EncodedScalarIdentity(), Encoding.self)
  }
}

extension _UnicodeViews._MappedScalars {
  public struct Index : Comparable {
    // In one call, parsing produces both:
    // - the buffer of code units comprising the scalar and
    // - the position of the next scalar
    // In a typical subscript-and-advance loop, the same call would need to be
    // repeated twice for each iteration if we didn't store all that information
    // in this index.
    //
    // When parsing is updated to handle a larger chunk at a time (e.g. a SIMD
    // vector's worth), this will become more complicated/fun.
    let base: CodeUnits.Index
    let next: CodeUnits.Index
    let output: Mapping.Output?

    // Having an init makes us impervious to member reordering.
    init(
      base: CodeUnits.Index,
      next: CodeUnits.Index,
      output: Mapping.Output? // Cached element
    ) {
      self.base = base
      self.next = next
      self.output = output
    }

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.base < rhs.base
    }
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.base == rhs.base
    }
  }
}

/// Collection Conformance
extension _UnicodeViews._MappedScalars : BidirectionalCollection {
  public var startIndex: Index {
    if _slowPath(codeUnits.isEmpty) { return endIndex }
    return index(after:
      Index(
        base: codeUnits.startIndex, next: codeUnits.startIndex, output: nil))
  }
  
  public var endIndex: Index {
    return Index(
      base: codeUnits.endIndex, next: codeUnits.endIndex, output: nil)
  }
  
  public subscript(i: Index) -> Mapping.Output {
    if let r = i.output {
      return r
    }
    return index(after:
      Index(base: i.base, next: i.base, output: nil)).output!
  }

  public func index(after i: Index) -> Index {
    var start = i.next
    while true {
      let remainder = codeUnits[start...]
      switch Encoding.parse1Forward(remainder, knownCount: 0) {
        
      case .valid(let parsed, let next):
        if let output = mapping.transform(parsed) {
          return Index(base: start, next: next, output: output)
        }
        start = next
                
      case .error(let next):
        if let r = mapping.transform(UnicodeScalar.replacementCharacter) {
          return Index(base: start, next: next, output: r)
        }
        start = next
        
      case .emptyInput:
        return endIndex
      }
    }
  }

  public func index(before i: Index) -> Index {
    // Parse backward from the beginning of the current encoded unicode scalar
    var end = i.base
    
    while true {
      let remainder = codeUnits[..<end]
      switch Encoding.parse1Reverse(remainder, knownCount: 0) {
        
      case .valid(let parsed, let prior):
        if let output = mapping.transform(parsed) {
          return Index(base: prior, next: end, output: output)
        }
        end = prior

      case .error(let prior):
        if let r = mapping.transform(UnicodeScalar.replacementCharacter) {
          return Index(base: prior, next: end, output: r)
        }
        end = prior
        
      case .emptyInput:
        fatalError("Indexing past start of code units")
      }
    }
  }
}

extension _UnicodeViews._MappedScalars : UnicodeView {
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    let p = codeUnits.index(atOffset: x.encodedOffset)
    if case .unicodeScalar(_, let width, let scalar) = x {
      return Index(
        base: p, next: codeUnits.index(p, offsetBy: width^),
        output: scalar == nil ? nil : mapping.transform(scalar!))
    }
    return index(after: Index(base: p, next: p, output: nil))
  }
  
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    return .unicodeScalar(
      encodedOffset: numericCast(codeUnits.offset(of: x.base)),
      width: numericCast(codeUnits.distance(from: x.base, to: x.next)),
      scalar: x.output == nil ? nil : mapping.transform(x.output!))
  }
  
  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}



//===----------------------------------------------------------------------===//
// _UnicodeViews.UnicodeScalars
//===----------------------------------------------------------------------===//

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _UnicodeViews {
  struct _ToUnicodeScalar : _UnicodeScalarMapping {
    typealias Output = UnicodeScalar
    typealias InputEncoding = Encoding
    func transform(_ input: UnicodeScalar)->Output? {
      return input
    }
    func transform(_ input: InputEncoding.EncodedScalar)->Output? {
      return UnicodeScalar(input)
    }
    func transform(_ output: Output)->UnicodeScalar {
      return output
    }
  }
  
  typealias Scalars = _MappedScalars<_ToUnicodeScalar>
  
  public var scalars: Scalars {
    return Scalars(codeUnits, mapping: _ToUnicodeScalar(), Encoding.self)
  }
}

//===----------------------------------------------------------------------===//
// _UnicodeViews.ScalarsTranscoded<ToEncoding>
//===----------------------------------------------------------------------===//

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _UnicodeViews {
  public struct _TranscodeScalar<OutputEncoding: UnicodeEncoding>
    : _UnicodeScalarMapping {
    typealias Output = OutputEncoding.EncodedScalar
    typealias InputEncoding = Encoding
    func transform(_ input: UnicodeScalar)->Output? {
      return OutputEncoding.EncodedScalar(input)
    }
    func transform(_ input: InputEncoding.EncodedScalar)->Output? {
      return OutputEncoding.encode(input)
    }
    func transform(_ output: Output)->UnicodeScalar {
      return UnicodeScalar(output)
    }
  }

  typealias ScalarsTranscoded<
    ToEncoding: UnicodeEncoding
  > = _MappedScalars<_TranscodeScalar<ToEncoding>>
  
  public func scalarsTranscoded<ToEncoding>(
    to dst: ToEncoding.Type
  )
  -> ScalarsTranscoded<ToEncoding> {
    return ScalarsTranscoded<ToEncoding>(
      codeUnits, mapping: _TranscodeScalar<ToEncoding>(), Encoding.self)
  }
}


//===----------------------------------------------------------------------===//
// _UnicodeViews.TranscodedView<ToEncoding>
//===----------------------------------------------------------------------===//
extension _UnicodeViews {
  public typealias TranscodedView<ToEncoding : UnicodeEncoding>
  = _TranscodedView<CodeUnits, Encoding, ToEncoding>
  
  public func transcoded<ToEncoding>(
    to targetEncoding: ToEncoding.Type
  ) -> TranscodedView<ToEncoding> {
    return type(of: self).TranscodedView(self.codeUnits, to: targetEncoding)
  }
}

/// Given `CodeUnits` representing text that has been encoded with
/// `FromEncoding`, provides a collection of `ToEncoding.CodeUnit`s
/// representing the same text.
public struct _TranscodedView<
CodeUnits : RandomAccessCollection,
FromEncoding_ : UnicodeEncoding,
ToEncoding : UnicodeEncoding
> 
  : BidirectionalCollection, BidirectionalCollectionWrapper
where FromEncoding_.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
{
  public typealias FromEncoding = FromEncoding_
  public typealias Self_ = _TranscodedView
  
  // This view is, in spirit, the result of flattening the ScalarsTranscoded
  // view.  We flatten that view of the codeUnits' slice type just to make
  // index translation more straightforward.
  public typealias _Unflattened = _UnicodeViews<CodeUnits, FromEncoding>
    .ScalarsTranscoded<ToEncoding>
  public typealias Base = FlattenBidirectionalCollection<_Unflattened>

  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public var _codeUnits: CodeUnits {
    return base._base.codeUnits
  }
  
  public init(_ codeUnits: CodeUnits,
    from src:  FromEncoding.Type = FromEncoding.self,
    to dst: ToEncoding.Type = ToEncoding.self
  ) {
    base = Base(
      _UnicodeViews(
        codeUnits, FromEncoding.self).scalarsTranscoded(to: ToEncoding.self))
  }
}

extension _TranscodedView : UnicodeView {
  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    let outer = base._base.nativeIndex(x)
    if case .transcoded(_, let outputOffset, let encodingID) = x {
      if encodingID == ToEncoding.EncodedScalar.self
      && outer != base._base.endIndex { 
        let inner = base._base[outer].index(atOffset: outputOffset)
        return Index(outer, inner)
      }
    }
    return Index(outer,nil)
  }
  
  public func anyIndex(_ i: Index) -> AnyUnicodeIndex {
    return .transcoded(
      inputOffset: base._base.anyIndex(i._outer).encodedOffset,
      outputOffset: i._inner == nil ? 0
        : numericCast(base._base[i._outer].offset(of: i._inner!)),
      outputEncoding: ToEncoding.EncodedScalar.self
    )
  }
}

extension _UnicodeViews : _UTextable {
}

extension _UnicodeViews {
  internal func _nativeLength(_ uText: inout _UText) -> Int64 {
    uText.validate()
    return codeUnits.count^
  }

  internal func _parsedSlice(
    _ offset: Int64,
    _ slice: (CodeUnits.Index) -> CodeUnits.SubSequence
  ) -> _UnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _UnicodeViews_(
      slice(codeUnits.index(atOffset: offset)), Encoding.self
    ).encodedScalars.dropFirst(0)
  }

  internal func _parsedSuffix(
    fromOffset offset: Int64
  ) -> _UnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _parsedSlice(offset, codeUnits.suffix(from:))
  }

  internal func _clone(
    _ dst: UnsafeMutablePointer<_UText>?, _ src: UnsafePointer<_UText>,
    _ deep: Bool, _ status: UnsafeMutablePointer<_UErrorCode>?
  ) ->  UnsafeMutablePointer<_UText> {
    _sanityCheck(!deep, "deep cloning not supported")
    UnsafeMutablePointer(mutating: src)[0].validate()
    // _debugLog("_clone with dst = \(String(describing: dst))")
    // _debugLog("src: \(src[0])")
    let r = dst
      ?? UnsafeMutablePointer.allocate(capacity: MemoryLayout<_UText>.size)
    r[0] = src[0]
    r[0].setup()
    r[0].validate()
    // _debugLog("clone result: \(r[0])")
    return r
  }

  internal func _access(
    _ u: inout _UText, _ nativeTargetIndex: Int64, _ forward: Bool
  ) -> Bool {

    // _debugLog("_access(u: \(u), nativeTargetIndex: \(nativeTargetIndex), forward: \(forward))")
    u.validate()
    u.chunkOffset = 0

    let inBoundsTarget = nativeTargetIndex - (forward ? 0 : 1)
    if (u.chunkNativeStart..<u.chunkNativeLimit).contains(inBoundsTarget) {

      var parsedChunk = _parsedSuffix(fromOffset: u.chunkNativeStart)
      
      var nativeOffset = u.chunkNativeStart
      while nativeOffset < nativeTargetIndex,
      let scalar = parsedChunk.popFirst() {
        nativeOffset += scalar.count^
        u.chunkOffset += scalar.utf16.count^
      }
      return true
    }
    // _debugLog("_access: filling buffer")

    // FIXME: should we use parseForward/parseReverse on some slice?
    
    guard (0...codeUnits.count^).contains(nativeTargetIndex)
    else { return false }

    u.chunkLength = 0
    u.chunkNativeStart = nativeTargetIndex
    u.chunkNativeLimit = nativeTargetIndex
    
    u.withBuffer { buffer in
      if forward {
        let scalars = _parsedSuffix(fromOffset: nativeTargetIndex)

        for scalar in scalars {
          // don't overfill the buffer
          if u.chunkLength + scalar.utf16.count^ > buffer.count^ { break }
          for unit in scalar.utf16 {
            // _debugLog("# unit: \(String(unit, radix: 16))")
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeLimit += scalar.count^
        }
      }
      else {
        let scalars
          = _parsedSlice(nativeTargetIndex, codeUnits.prefix(upTo:))

        // Transcode the source in reverse, filling the buffer forward
        for scalar in scalars.reversed() {
          // don't overfill the buffer
          if u.chunkLength + scalar.utf16.count^ > buffer.count^ { break }
          for unit in scalar.utf16.reversed() {
            buffer[u.chunkLength^] = unit
            u.chunkLength += 1
          }
          u.chunkNativeStart -= scalar.count^
        }
        u.chunkOffset = u.chunkLength
        // Reverse the buffer contents to get everything in the right order.
        var b = buffer // copy due to https://bugs.swift.org/browse/SR-3782
        b[..<buffer.index(atOffset: u.chunkLength)].reverse()
      }
    }
    // _debugLog("_access filled buffer, u = \(u)")

    u.validate()
    
    // WORKAROUND? <rdar://30979421>: UBreakIterator attempts to index UText
    // out-of-range if we don't treat requests for empty chunks as
    // out-of-bounds.
    return u.chunkLength > 0
  }
  
  internal func _extract(
    _ u: inout _UText,
    _ nativeStart: Int64, _ nativeLimit: Int64,
    _ destination: UnsafeMutableBufferPointer<__swift_stdlib_UChar>,
    _ error: UnsafeMutablePointer<_UErrorCode>?
  ) -> Int32 {
    // _debugLog("_extract: \(u)")
    u.validate()

    let s = nativeStart.clamped(to: 0...codeUnits.count^)
    let l = nativeLimit.clamped(to: 0...codeUnits.count^)
    u.chunkNativeStart = l
    u.chunkNativeLimit = l
    u.chunkLength = 0
    
    if s < l { // anything to extract?
      let base = codeUnits[
        codeUnits.index(atOffset: s)..<codeUnits.index(atOffset: l)
      ]
      // FIXME: we should be using an associated UTF16View type here rather than
      // the generic TranscodedView, which is likely to be less efficient in
      // some common cases.
      let source
        = _UnicodeViews_(base, Encoding.self).transcoded(to: UTF16.self)
      var d = destination // copy due to https://bugs.swift.org/browse/SR-3782
      let (limit, remainder) = d.copy(from: source)
      
      // Add null termination if it fits
      if limit < d.endIndex { d[limit] = 0 }

      // If no overflow, we're done
      if remainder.isEmpty { return Int32(destination.offset(of: limit)) }

      // Report the error and account for the overflow length in the return value
      error![0] = __swift_stdlib_U_BUFFER_OVERFLOW_ERROR
      return Int32(destination.offset(of: limit) + remainder.count)
    }
    return 0
  }
  
  internal func _mapOffsetToNative(_ u: UnsafePointer<_UText>) -> Int64 {
    UnsafeMutablePointer(mutating: u)[0].validate()
    
    if u[0].chunkOffset == 0 { return u[0].chunkNativeStart }

    // Advance scalar by scalar in the source until we find the offset
    let scalars = _parsedSuffix(fromOffset: u[0].chunkNativeStart)
    var utf16Offset = 0, nativeOffset = 0
    for s in scalars {
      nativeOffset += s.count^
      utf16Offset += s.utf16.count^
      if utf16Offset == u[0].chunkOffset^ {
        return u[0].chunkNativeStart + nativeOffset^
      }
    }
    fatalError("supposed to be unreachable")
  }
  
  internal func _mapNativeIndexToUTF16(_ u: UnsafePointer<_UText>, _ nativeIndex: Int64) -> Int32 {
    // _debugLog("_mapNativeIndexToUTF16: \(u)")
    UnsafeMutablePointer(mutating: u)[0].validate()
    
    if u[0].chunkNativeStart == nativeIndex { return 0 }

    let nativeChunk = codeUnits[
      codeUnits.index(atOffset: u[0].chunkNativeStart)
      ..<
      codeUnits.index(atOffset: nativeIndex)]
    
    return _UnicodeViews_(
      nativeChunk, Encoding.self).transcoded(to: UTF16.self).count^
  }
}

extension _UnicodeViews {
  
  public struct CharacterView : UnicodeView {

    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.codeUnits = codeUnits
    }

    internal let codeUnits: CodeUnits

    public typealias SubSequence = UnicodeViewSlice<CharacterView>
    public subscript(bounds: Range<Index>) -> SubSequence {
      return SubSequence(base: self, bounds: bounds)
    }

    public struct Index : ForwardingWrapper, Comparable {
      public var base: CodeUnits.Index
      public var next: CodeUnits.Index
    }
    
    public var startIndex: Index {
      let baseStart = codeUnits.startIndex
      var r = Index(base: baseStart, next: baseStart)
      if !codeUnits.isEmpty { formIndex(after: &r) }
      return r
    }
    
    public var endIndex: Index {
      return Index(base: codeUnits.endIndex, next: codeUnits.endIndex)
    }

    public func nativeIndex(_ i: AnyUnicodeIndex) -> Index {
      let p = codeUnits.index(atOffset: i.encodedOffset)
      if case .character(_, let width) = i {
        return Index(base: p, next: codeUnits.index(p, offsetBy: width^))
      }
      return index(after: Index(base: p, next: p))
    }
    
    public func anyIndex(_ i: Index) -> AnyUnicodeIndex {
      return .character(
        encodedOffset: codeUnits.offset(of: i.base)^,
        width: codeUnits.distance(from: i.base, to: i.next)^)
    }
    
    public subscript(i: Index) -> Character {
      return Character(_codeUnits: codeUnits[i.base..<i.next], Encoding.self)
    }     

    public func index(after i: Index) -> Index {
      // If the next two scalar values are both < 0x300, we can bypass ICU.
      let u32 = _UnicodeViews_(
        codeUnits[i.next...], Encoding.self).transcoded(to: UTF32.self)
      
      guard let s0 = u32.first else { return Index(base: i.next, next: i.next) }
      if _fastPath(s0 < 0x300) {
        // If we are out of scalars we can pretend the next one is zero
        let s1 = u32.dropFirst().first ?? 0 
        if _fastPath(s1 < 0x300) {
          let width = (s0, s1) == (13, 10) ? 2 : 1
          return Index(
            base: i.next, next: codeUnits.index(i.next, offsetBy: width^))
        }
      }
      
      let k = codeUnits.offset(of: i.next)
      let nextOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_following($0, k^)
      }
      return Index(
        base: i.next, next: codeUnits.index(i.base, offsetBy: nextOffset^))
    }

    public func index(before i: Index) -> Index {
      // If the previous two scalar values are both < 0x300, we can bypass ICU.
      let u32 = _UnicodeViews_(
        codeUnits[..<i.base], Encoding.self).transcoded(to: UTF32.self)
      
      guard let s1 = u32.last else {
        return Index(base: codeUnits.startIndex, next: i.base)
      }
      if _fastPath(s1 < 0x300) {
        // If we are out of scalars we can pretend the previous one is zero
        let s0 = u32.dropLast().last ?? 0 
        if _fastPath(s0 < 0x300) {
          let width = (s0, s1) == (13, 10) ? 2 : 1
          return Index(
            base: i.next, next: codeUnits.index(i.next, offsetBy: -width^))
        }
      }
      
      let k = codeUnits.offset(of: i.base)
      
      // FIXME: there is always a grapheme break between two scalars that are
      // both < U+0300.  Use that to optimize.  Can we make a stronger
      // statement, that there's always a break before any scalar < U+0300?
      let previousOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_preceding($0, k^)
      }
      return Index(
        base: codeUnits.index(atOffset: previousOffset), next: i.base)
    }
    
    internal func _withUBreakIterator<R>(_ body: (OpaquePointer)->R) -> R {
      var err = __swift_stdlib_U_ZERO_ERROR;

      let bi = __swift_stdlib_ubrk_open(
        /*type:*/ __swift_stdlib_UBRK_CHARACTER, /*locale:*/ nil,
        /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
      _precondition(err.isSuccess, "unexpected ubrk_open failure")
      defer { __swift_stdlib_ubrk_close(bi) }

      return _UnicodeViews_(codeUnits, Encoding.self)._withUText { u in
        __swift_stdlib_ubrk_setUText(bi, u, &err)
        _precondition(err.isSuccess, "unexpected ubrk_setUText failure")
        return body(bi)
      }
    }  
  }

  public var characters: CharacterView {
    return CharacterView(codeUnits, Encoding.self)
  }
}

extension _UnicodeViews {
  /// Invokes `body` on a contiguous buffer of our UTF16.
  ///
  /// - Note: `body` should be prepared to deal with invalid UTF16.
  internal func _withContiguousUTF16<R>(
    _ body: (UnsafeBufferPointer<UTF16.CodeUnit>)->R
  ) -> R {
    if Encoding.EncodedScalar.self is UTF16.EncodedScalar.Type {
      // It's a UTF16 encoding
      let r: R? = codeUnits.withExistingUnsafeBuffer {
        body($0 as Any as! UnsafeBufferPointer<UTF16.CodeUnit>)
      }
      if r != nil { return r! }
    }
    return Array(self.transcoded(to: UTF16.self)).withUnsafeBufferPointer(body)
  }
}

//===----------------------------------------------------------------------===//

// Michael NOTE: Trying to nest this crashed the remangler...
//
// A Latin1 character view, more efficient than a general purpose character
// view. Checks for special cases inside Latin1, otherwise code-unit based.
//
// FIXME: Better name here. It's not just Latin1, but it's also a valid (TODO:
// prove) view for many ranges of unicode scalar values. Maybe name based on the
// GB_n level of rule application from the Unicode spec?
//
// TODO: Would it be better to unify under the general character view, and
// incorporate our fast paths as applicable? This seems like the better long-
// term direction to take here.
//
public struct Latin1CharacterView<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
> : UnicodeView
  where Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.Iterator.Element : UnsignedInteger
{
  typealias CodeUnit = CodeUnits.Iterator.Element
  internal let _CR: CodeUnit = 0x0D
  internal let _LF: CodeUnit = 0x0A

  public init(_ codeUnits: CodeUnits) {
    self.codeUnits = codeUnits
  }

  internal let codeUnits: CodeUnits

  public typealias SubSequence = UnicodeViewSlice<Latin1CharacterView>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }

  public struct Index : ForwardingWrapper, Comparable {
    public var base: CodeUnits.IndexDistance
  }

  public func nativeIndex(_ x: AnyUnicodeIndex) -> Index {
    return Index(base: x.encodedOffset^)
  }
  public func anyIndex(_ x: Index) -> AnyUnicodeIndex {
    return .encodedOffset(x.base^)
  }
  
  public var startIndex: Index { return Index(base: 0) }
  public var endIndex: Index { return Index(base: codeUnits.count) }

  internal func getCU(at i: Index) -> CodeUnit {
    let idx = codeUnits.index(atOffset: i.base)
    return codeUnits[idx]
  }
  internal func getIndex(_ i: CodeUnits.Index) -> Index {
    return Index(base: codeUnits.distance(from: codeUnits.startIndex, to: i))
  }

  public subscript(i: Index) -> Character {
    let nextIdx = index(after: i)

    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(nextIdx.base == i.base+1) {
      return Character(UnicodeScalar(numericCast(getCU(at: i))))
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    _sanityCheck(nextIdx.base == i.base+2)
    _sanityCheck(getCU(at: i) == _CR)
    _sanityCheck(getCU(at: nextIdx) == _LF)
    return Character("\u{0D}\u{0A}")
  }

  public func index(after i: Index) -> Index {
    let nextCUIdx = codeUnits.index(atOffset: i.base+1)
    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(getCU(at: i) != _CR) {
      return getIndex(nextCUIdx)
    }

    // Special case: CR-LF is single grapheme
    if nextCUIdx != codeUnits.endIndex && codeUnits[nextCUIdx] == _LF {
      return getIndex(codeUnits.index(after: nextCUIdx))
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    return getIndex(nextCUIdx)
  }

  public func index(before i: Index) -> Index {
    let previousCUIdx = codeUnits.index(atOffset: i.base-1)
    // Fast path: Single code unit character (i.e. not CR-LF)
    if _fastPath(codeUnits[previousCUIdx] != _LF) {
      return getIndex(previousCUIdx)
    }

    // Special case: CR-LF is single grapheme
    if previousCUIdx != codeUnits.startIndex {
      let previousPreviousCUIdx = codeUnits.index(before: previousCUIdx)
      if codeUnits[previousPreviousCUIdx] == _CR {
        return getIndex(previousPreviousCUIdx)
      }
    }

    // FIXME: Is there anything else in Latin1 except CR-LF that's not single-
    // code-unit-is-single-grapheme?
    return getIndex(previousCUIdx)
  }
}

// TODO: Also valid for other encodings when properties over scalar values still
// apply.
extension _UnicodeViews where CodeUnits.Iterator.Element : UnsignedInteger {
  // TODO: Currently, present for all Strings. Might want to have type
  // restrictions.
  public var latin1CharacterView: Latin1CharacterView<CodeUnits, Encoding> {
    return Latin1CharacterView<CodeUnits, Encoding>(codeUnits)
  }
}

//===----------------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

// A normalization segment that is FCC-normalized. This is a collection of
// normalized UTF16 code units.
//
// Note that this is not a UnicodeView. Indices into a normalized segment are
// not native indices, and do not necessarily correspond to any particular code
// unit as they may of undergone composition or decomposition, which in turn may
// also re-order code units. Thus, FCCNormalizedSegments are not suitable for
// queries needing sub-segment granularity. However, FCCNormalizedSegments are
// suitable for segment-level-or-coarser granularity queries, which include any
// grapheme-level queries as segments are sub- grapheme.
//
// TODO: verify above statement about segments being sub-grapheme, that is make
// sure it is not possible to have segments span graphemes.
//
// TODO: Explore coalescing small segments together
public struct FCCNormalizedSegment : BidirectionalCollection {
  let buffer: UTF16CodeUnitBuffer

  public init(_ buffer: UTF16CodeUnitBuffer) {
    self.buffer = buffer
  }
  public init() {
    self.buffer = UTF16CodeUnitBuffer()
  }

  public typealias Index = Int // UTF16CodeUnitBuffer.Index

  public var startIndex : Index {
    return buffer.startIndex
  }
  public var endIndex : Index {
    return buffer.endIndex
  }
  public subscript(i: Index) -> UInt16 {
    return buffer[i]
  }
  public func index(after i: Index) -> Index {
    return buffer.index(after: i)
  }
  public func index(before i: Index) -> Index {
    return buffer.index(before: i)
  }
  public typealias SubSequence = BidirectionalSlice<FCCNormalizedSegment>
}

// Ask ICU if the given unicode scalar value has a normalization boundary before
// it, that is it begins a new normalization segment.
public func _hasBoundary(before value: UInt32) -> Bool {
  return __swift_stdlib_unorm2_hasBoundaryBefore(_fccNormalizer, value) != 0
}

// TODO: explore using hasBoundary(after:), and whether that will identify
// finer-grained segments.

public struct FCCNormalizedLazySegments<
  CodeUnits : RandomAccessCollection,
  FromEncoding : UnicodeEncoding
>
where
  CodeUnits.Index == CodeUnits.SubSequence.Index,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence == CodeUnits.SubSequence.SubSequence,
  CodeUnits.Iterator.Element == CodeUnits.SubSequence.Iterator.Element,
  CodeUnits.Iterator.Element == FromEncoding.EncodedScalar.Iterator.Element
{
  let codeUnits: CodeUnits

  public init(
    _ codeUnits: CodeUnits,
    _: FromEncoding.Type = FromEncoding.self
  ) {
    self.codeUnits = codeUnits
  }

  public init(_ unicodeView: _UnicodeViews<CodeUnits, FromEncoding>) {
    self.init(unicodeView.codeUnits)
  }

  // Find the segment that terminates at `endingAt`. Optimized for backwards
  // traversal.
  internal func priorSegment(
    endingAt end: CodeUnits.Index
  ) -> Index {
    precondition(end != codeUnits.startIndex)

    // Decode scalars backwards, including them until we after we find one that
    // has a boundary before it (and include that one).
    var start = end
    while start != codeUnits.startIndex {
      // Include this scalar
      let (scalarValue: value, startIndex: scalarStart, e)
        = decodeOne(priorTo: start)
      _sanityCheck(e == start, "Internal inconsistency in decodeOne")

      // Include this scalar
      start = scalarStart

      if _hasBoundary(before: value) {
        // We're done
        break
      }
    }

    return Index(
      nativeOffset: codeUnits.distance(
        from: codeUnits.startIndex, to: start
      ),
      nativeCount: codeUnits.distance(from: start, to: end),
      segment: formSegment(from: start, until: end)
    )
  }

  // Find the segment that starts with `startingAt`. Optimized for forwards
  // traversal.
  internal func nextSegment(
    startingAt start: CodeUnits.Index
  ) -> Index {
    if start == codeUnits.endIndex {
      return endIndex
    }

    // Parse the first scalar, it will always be in the segment
    var (scalarValue: value, startIndex: s, endIndex: end)
      = decodeOne(from: start)
    _sanityCheck(start == codeUnits.startIndex || 
                 _hasBoundary(before: value), "Not on segment boundary")
    _sanityCheck(s == start, "Internal inconsistency in decodeOne")

    // Include any subsequent scalars that don't have boundaries before them
    while end != codeUnits.endIndex {
      let (scalarValue: value, startIndex: s, endIndex: scalarEnd)
        = decodeOne(from: end)
      _sanityCheck(s == end, "Internal inconsistency in decodeOne")

      if _hasBoundary(before: value) {
        // Exclude this scalar
        break
      }

      // Include this scalar
      end = scalarEnd
    }

    return Index(
      nativeOffset: codeUnits.distance(
        from: codeUnits.startIndex, to: start
      ),
      nativeCount: codeUnits.distance(from: start, to: end),
      segment: formSegment(from: start, until: end)
    )
  }

  // Normalize a segment. Indices must be on scalar boundaries.
  internal func formSegment(
    from start: CodeUnits.Index,
    until end: CodeUnits.Index
  ) -> FCCNormalizedSegment {
    precondition(start != end, "TODO: should we just have empty segment?")

    let utf16CodeUnits = unicodeView(
      from: start, until: end
    ).scalarsTranscoded(
      to: UTF16.self
    )

    // TODO: Find way to re-use the storage, maybe iterator pattern?
    var buffer = UTF16CodeUnitBuffer(utf16CodeUnits.lazy.joined())

    // TODO: fast pre-normalized checks (worth doing before calling over to
    //       ICU)

    _sanityCheck(buffer.count > 0, "How did this happen? Failed precondition?")

    // Ask ICU to normalize
    //
    // FIXME: withMutableArray kind of defeats the purpose of the small
    // buffer :-(
    buffer.withMutableArray { (array: inout [UInt16]) -> () in
      array.withUnsafeBufferPointer {
        // TODO: Just reserving one or two extra up front. If we're segment-
        // based, should be finite number of possible decomps.
        let originalCount = buffer.count
        while true {
          var error = __swift_stdlib_U_ZERO_ERROR
          let usedCount = __swift_stdlib_unorm2_normalize(
            // FIXME: check valid force-unwrap
            _fccNormalizer, $0.baseAddress!, numericCast($0.count),
            &array, numericCast(array.count), &error)
          if __swift_stdlib_U_SUCCESS(error) {
            array.removeLast(array.count - numericCast(usedCount))
            return
          }
          _sanityCheck(
            error == __swift_stdlib_U_BUFFER_OVERFLOW_ERROR,
            "Unknown normalization error")

          // Maximum number of NFC to FCC decompositions for a single unicode
          // scalar value
          //
          // TODO: what is this really? Should be much less
          let maxDecompSize = 8

          // Very loose canary to check that we haven't grown exceedingly large
          // (indicative of logic error). Loose by assuming that every original
          // character could be decomposed the maximum number of times. Without
          // this, an error would loop until we run out of memory or the array
          // is larger than 2^32 on 64bit platforms.
          _sanityCheck(buffer.count < originalCount*maxDecompSize)

          // extend array storage by 25%
          array.append(
            contentsOf: repeatElement(0, count: (array.count + 3) >> 2))
        }
      }
    }

    return FCCNormalizedSegment(buffer)
  }


  // Decode one or more code units, returning the unicode scalar value and the
  // indices spanning the code units parsed. `from` should be on scalar boundary
  internal func decodeOne(from start: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(start != codeUnits.endIndex, "Given empty slice")

    let encodedScalar = unicodeView(from: start).encodedScalars.first!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: start,
            endIndex: codeUnits.index(start, offsetBy: numericCast(encodedScalar.count)))
  }

  // As decodeOne(from:), but in reverse. `priorTo` is the index after the last
  // code unit in the scalar, i.e. it is exclusive.
  internal func decodeOne(priorTo end: CodeUnits.Index)
    -> (scalarValue: UInt32,
        startIndex: CodeUnits.Index,
        endIndex: CodeUnits.Index) {
    precondition(end != codeUnits.startIndex, "Given empty slice")

    let encodedScalar = unicodeView(until: end).encodedScalars.last!
    return (scalarValue: encodedScalar.utf32[0],
            startIndex: codeUnits.index(end, offsetBy: -numericCast(encodedScalar.count)),
            endIndex: end)
  }

  // Get the rest of the Unicode view
  internal func unicodeView(
    from start: CodeUnits.Index? = nil,
    until end: CodeUnits.Index? = nil
  ) -> _UnicodeViews<CodeUnits.SubSequence, FromEncoding> {
    let end = end ?? codeUnits.endIndex
    let start = start ?? codeUnits.startIndex
    return _UnicodeViews(codeUnits[start..<end], FromEncoding.self)
  }
}

extension FCCNormalizedLazySegments : BidirectionalCollection {
  // TODO?: This is really more like an iterator...
  public struct Index : Comparable {
    // The corresponding native begin/end indices for this segment
    let nativeOffset: CodeUnits.IndexDistance
    let nativeCount: CodeUnits.IndexDistance
    let segment: FCCNormalizedSegment

    public static func <(lhs: Index, rhs: Index) -> Bool {
      if lhs.nativeOffset < rhs.nativeOffset {
        // Our ends should be ordered similarly, unless lhs is the last index
        // before endIndex and rhs is the endIndex.
        _sanityCheck(
          lhs.nativeOffset + lhs.nativeCount 
            < rhs.nativeOffset + rhs.nativeCount ||
          rhs.nativeCount == 0,
          "overlapping segments?")

        return true
      }

      return false
    }

    public static func ==(lhs: Index, rhs: Index) -> Bool {

      if lhs.nativeOffset == rhs.nativeOffset {
        _sanityCheck(
          lhs.nativeCount == rhs.nativeCount,
          "overlapping segments?")

        return true
      }

      return false
    }
  }

  // TODO: formIndex(after:) that re-uses storage

  public var startIndex: Index {
    return nextSegment(startingAt: codeUnits.startIndex)
  }
  public var endIndex: Index {
    return Index(
      nativeOffset: codeUnits.count,
      nativeCount: 0,
      segment: FCCNormalizedSegment()
    )
  }

  public func index(after idx: Index) -> Index {
    return nextSegment(
      startingAt: codeUnits.index(atOffset: idx.nativeOffset + idx.nativeCount)
    )
  }
  public func index(before idx: Index) -> Index {
    return priorSegment(
      endingAt: codeUnits.index(atOffset: idx.nativeOffset)
    )
  }
  public subscript(position: Index) -> FCCNormalizedSegment {
    return position.segment
  }

  public typealias SubSequence = BidirectionalSlice<FCCNormalizedLazySegments>
}

extension _UnicodeViews {
  public struct FCCNormalizedUTF16View: BidirectionalCollectionWrapper {
    public typealias Base = FlattenBidirectionalCollection<
      FCCNormalizedLazySegments<CodeUnits, Encoding>
    >
    public var base: Base
    public typealias Index = Base.Index
    public typealias IndexDistance = Base.IndexDistance
    public typealias Self_ = FCCNormalizedUTF16View
    
    public init(_ unicodeView: _UnicodeViews<CodeUnits, Encoding>) {
      self.base = Base(FCCNormalizedLazySegments(unicodeView))
    }
  }

  public var fccNormalizedUTF16: FCCNormalizedUTF16View {
    return FCCNormalizedUTF16View(self)
  }
}

extension _UnicodeViews.FCCNormalizedUTF16View : UnicodeView {
  public func nativeIndex(_ i: AnyUnicodeIndex) -> Index {
    let segmentIdx = base._base.nextSegment(
      startingAt: base._base.codeUnits.index(
        base._base.codeUnits.startIndex,
        offsetBy: numericCast(i.encodedOffset)
      )
    )
    return Index(segmentIdx, segmentIdx.segment.startIndex)
  }
  
  public func anyIndex(_ i: Index) -> AnyUnicodeIndex {
    return .encodedOffset(numericCast(i._outer.nativeOffset))
  }
  
  public typealias SubSequence = UnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}

