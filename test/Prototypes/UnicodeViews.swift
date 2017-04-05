//===--- XUnicodeViews.swift -----------------------------------------------===//
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

public enum AnyXUnicodeIndex : Comparable {
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

  public static func < (lhs: AnyXUnicodeIndex, rhs: AnyXUnicodeIndex) -> Bool {
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

  public static func == (lhs: AnyXUnicodeIndex, rhs: AnyXUnicodeIndex) -> Bool {
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

/// XUnicode views should conform to this protocol, which supports index
/// interchange and String type erasure.
public protocol _XUnicodeView : BidirectionalCollection {
  func nativeIndex(_: AnyXUnicodeIndex) -> Index
  func anyIndex(_ : Index) -> AnyXUnicodeIndex
}

extension _XUnicodeView {
  /// Constructs a copy of other
  public init(_ other: Self) { self = other }
}

/// A XUnicodeView that is already using AnyXUnicodeIndex has trivial
/// interchange
extension _XUnicodeView where Index == AnyXUnicodeIndex {
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index { return x }
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex { return x }
}

public protocol XUnicodeView : _XUnicodeView {
  associatedtype SubSequence: _XUnicodeView = XUnicodeViewSlice<Self>
}

//===----------------------------------------------------------------------===//
public struct XUnicodeViewSlice<BaseView: _XUnicodeView>
  : BidirectionalCollectionWrapper, XUnicodeView {
  public typealias Base = BidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = XUnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RandomAccessXUnicodeViewSlice<
  BaseView: _XUnicodeView & RandomAccessCollection
> : BidirectionalCollectionWrapper, XUnicodeView, RandomAccessCollection {
  public typealias Base = RandomAccessSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  init(base: BaseView, bounds: Range<BaseView.Index>) {
    self.base = Base(base: base, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = RandomAccessXUnicodeViewSlice
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: base.base, bounds: bounds)
  }
}

public struct RangeReplaceableXUnicodeViewSlice<
  BaseView: _XUnicodeView & RangeReplaceableCollection
> : BidirectionalCollectionWrapper, XUnicodeView, RangeReplaceableCollection {
  public typealias Base = RangeReplaceableBidirectionalSlice<BaseView>
  public typealias Index = Base.Index
  public typealias IndexDistance = Base.IndexDistance
  public typealias Iterator = Base.Iterator
  public var base: Base

  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
    return base.base.nativeIndex(x)
  }
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex {
    return base.base.anyIndex(x)
  }
  public typealias SubSequence = RangeReplaceableXUnicodeViewSlice<BaseView>
  
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
    from targetStart: Index, to targetEnd: Index,
    with replacement: C) -> Bool
  where C : Collection, C.Iterator.Element == Iterator.Element {
    return base._tryToReplaceSubrange(
      from: targetStart, to: targetEnd, with: replacement)
  }
}
//===----------------------------------------------------------------------===//

/*
//===--- RandomAccessXUnicodeView ------------------------------------------===//
/// Adapts any `RandomAccessCollection` to a `XUnicodeView`, with
/// `encodedOffset`s equal to the number of index steps from the `startIndex`.
///
/// Computing `encodedOffset` this way is pretty safe because if the base view
/// has random access, it must have a constant number N of elements per code
/// unit, and in all the usual instances, N = 1
public struct RandomAccessXUnicodeView<Base_: RandomAccessCollection> {
  public typealias Base = Base_
  public typealias Iterator = Base.Iterator
  public var base: Base
  public var _basey: Base { return base }
  public init(_ base: Base) { self.base = base }
}

extension RandomAccessXUnicodeView : BidirectionalCollectionWrapper {
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
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
    let d = numericCast(x.encodedOffset) as Base.IndexDistance
    return self._basey.index(_basey.startIndex, offsetBy: d)
  }
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex {
    return .encodedOffset(base.offset(of: x))
  }
}

extension RandomAccessXUnicodeView {
  public mutating func _tryToReplaceSubrange<C: Collection>(
    from targetStart: Index, to targetEnd: Index, with replacement: C
  ) -> Bool
  where C.Iterator.Element == Iterator.Element {
    // XUnicodeViews must have value semantics.  Note that this check will fail
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
      from: _unwrap(targetStart), to: _unwrap(targetEnd),  with: replacement)
  }
}

extension RandomAccessXUnicodeView : RandomAccessCollection {}

extension RandomAccessXUnicodeView : XUnicodeView {
  public typealias SubSequence
  = RandomAccessXUnicodeViewSlice<RandomAccessXUnicodeView>
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }  
}
*/

//===----------------------------------------------------------------------===//

/// A collection of `CodeUnit`s to be interpreted by some `Encoding`.
///
/// View types nested in _XUnicodeViews may be suitable *generic* implementation
/// guts for views for models of XUnicode, but specific models may want to
/// provide their own implementations.  For example, the UTF16 view of a
/// Latin1String would might be a simple lazy zero-extended mapping, rather than
/// something that goes through the transcoding machinery.
public struct _XUnicodeViews<
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
/// A straightforward typealias for _XUnicodeViews
///
/// Use this to escape the automatic deduction of the generic arguments given
/// the name `_XUnicodeViews` from within nested contexts
/// (https://bugs.swift.org/browse/SR-4155).
internal typealias _XUnicodeViews_<
  CodeUnits : RandomAccessCollection,
  Encoding : UnicodeEncoding
>  = _XUnicodeViews<CodeUnits, Encoding>
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
// _XUnicodeViews._MappedScalars
//===----------------------------------------------------------------------===//
/// A lazy collection of the source encoded scalars that for which Transform
/// doesn't return nil.
extension _XUnicodeViews {
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

extension _XUnicodeViews._MappedScalars {
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
extension _XUnicodeViews._MappedScalars : BidirectionalCollection {
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

extension _XUnicodeViews._MappedScalars : XUnicodeView {
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
    let p = codeUnits.index(atOffset: x.encodedOffset)
    if case .unicodeScalar(_, let width, let scalar) = x {
      return Index(
        base: p, next: codeUnits.index(p, offsetBy: width^),
        output: scalar == nil ? nil : mapping.transform(scalar!))
    }
    return index(after: Index(base: p, next: p, output: nil))
  }
  
  public func anyIndex(_ x: Index) -> AnyXUnicodeIndex {
    return .unicodeScalar(
      encodedOffset: numericCast(codeUnits.offset(of: x.base)),
      width: numericCast(codeUnits.distance(from: x.base, to: x.next)),
      scalar: x.output == nil ? nil : mapping.transform(x.output!))
  }
  
  public typealias SubSequence = XUnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
}



//===----------------------------------------------------------------------===//
// _XUnicodeViews.Scalars
//===----------------------------------------------------------------------===//

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _XUnicodeViews {
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
// _XUnicodeViews.ScalarsTranscoded<ToEncoding>
//===----------------------------------------------------------------------===//

/// A lazy collection of `Encoding.EncodedScalar` that results
/// from parsing an instance of codeUnits using that `Encoding`.
extension _XUnicodeViews {
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
// _XUnicodeViews.TranscodedView<ToEncoding>
//===----------------------------------------------------------------------===//
extension _XUnicodeViews {
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
  public typealias _Unflattened = _XUnicodeViews<CodeUnits, FromEncoding>
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
      _XUnicodeViews(
        codeUnits, FromEncoding.self).scalarsTranscoded(to: ToEncoding.self))
  }
}

extension _TranscodedView : XUnicodeView {
  public typealias SubSequence = XUnicodeViewSlice<Self_>
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(base: self, bounds: bounds)
  }
  public func nativeIndex(_ x: AnyXUnicodeIndex) -> Index {
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
  
  public func anyIndex(_ i: Index) -> AnyXUnicodeIndex {
    return .transcoded(
      inputOffset: base._base.anyIndex(i._outer).encodedOffset,
      outputOffset: i._inner == nil ? 0
        : numericCast(base._base[i._outer].offset(of: i._inner!)),
      outputEncoding: ToEncoding.EncodedScalar.self
    )
  }
}

extension _XUnicodeViews /*: _UTextable*/ {
  internal func _nativeLength(_ uText: inout _UText) -> Int64 {
    uText.validate()
    return codeUnits.count^
  }

  internal func _parsedSlice(
    _ offset: Int64,
    _ slice: (CodeUnits.Index) -> CodeUnits.SubSequence
  ) -> _XUnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
    return _XUnicodeViews_(
      slice(codeUnits.index(atOffset: offset)), Encoding.self
    ).encodedScalars.dropFirst(0)
  }

  internal func _parsedSuffix(
    fromOffset offset: Int64
  ) -> _XUnicodeViews<CodeUnits.SubSequence,Encoding>.EncodedScalars.SubSequence {
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
        = _XUnicodeViews_(base, Encoding.self).transcoded(to: UTF16.self)
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
    
    return _XUnicodeViews_(
      nativeChunk, Encoding.self).transcoded(to: UTF16.self).count^
  }
}

extension _XUnicodeViews {
  
  public struct CharacterView : XUnicodeView {

    public init(_ codeUnits: CodeUnits, _: Encoding.Type = Encoding.self) {
      self.storage = _XUnicodeViews(codeUnits)
    }

    internal let storage: _XUnicodeViews

    public typealias SubSequence = XUnicodeViewSlice<CharacterView>
    public subscript(bounds: Range<Index>) -> SubSequence {
      return SubSequence(base: self, bounds: bounds)
    }

    public struct Index : ForwardingWrapper, Comparable {
      public var base: CodeUnits.IndexDistance
      public var width: CodeUnits.IndexDistance
    }
    
    public var startIndex: Index { return Index(base: 0) }
    public var endIndex: Index {
      return Index(base: storage.codeUnits.count)
    }

    public subscript(i: Index) -> Character {
      let j = index(after: i)
      let contents = _XUnicodeViews_(
        storage.codeUnits[
          storage.codeUnits.index(atOffset: i.base)
          ..< storage.codeUnits.index(atOffset: j.base)],
        Encoding.self)
        
      if let small = Character(_smallUtf8: contents.transcoded(to: UTF8.self)) {
        return small
      }
      else {
        // FIXME: there is undoubtley a less ridiculous way to do this
        let scalars = contents.encodedScalars.lazy.map(UnicodeScalar.init)
        let string = Swift.String(Swift.String.UnicodeScalarView(scalars))
        return Character(_largeRepresentationString: string)
      }
    }     

    public func index(after i: Index) -> Index {
      // FIXME: there is always a grapheme break between two scalars that are
      // both < U+0300.  Use that to optimize.  Can we make a stronger
      // statement, that there's always a break before any scalar < U+0300?
      // _debugLog("index(after: \(i))")
      let nextOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_following($0, i.base^)
      }
      // _debugLog("  index(after: \(i)): \(nextOffset)")
      return Index(base: nextOffset^)
    }

    public func index(before i: Index) -> Index {
      // FIXME: there is always a grapheme break between two scalars that are
      // both < U+0300.  Use that to optimize.  Can we make a stronger
      // statement, that there's always a break before any scalar < U+0300?
      // _debugLog("index(before: \(i))")
      let previousOffset = _withUBreakIterator {
        __swift_stdlib_ubrk_preceding($0, i.base^)
      }
      // _debugLog("  -> \(previousOffset)")
      return Index(base: previousOffset^)
    }
    
    internal func _withUBreakIterator<R>(_ body: (OpaquePointer)->R) -> R {
      var err = __swift_stdlib_U_ZERO_ERROR;

      // _debugLog("ubrk_open")
      let bi = __swift_stdlib_ubrk_open(
        /*type:*/ __swift_stdlib_UBRK_CHARACTER, /*locale:*/ nil,
        /*text:*/ nil, /*textLength:*/ 0, /*status:*/ &err)
      _precondition(err.isSuccess, "unexpected ubrk_open failure")
      defer { __swift_stdlib_ubrk_close(bi) }

      fatalError()
      /*
      return storage._withUText { u in
        // _debugLog("ubrk_setUText(breakIterator: \(bi), u: \(u)")
        // _debugLog("u: \(u.pointee)")
        __swift_stdlib_ubrk_setUText(bi, u, &err)
        _precondition(err.isSuccess, "unexpected ubrk_setUText failure")
        return body(bi)
      }
      */
    }  
  }

  public var characters: CharacterView {
    return CharacterView(codeUnits, Encoding.self)
  }
}

/*
internal func _makeFCCNormalizer() -> OpaquePointer {
  var err = __swift_stdlib_U_ZERO_ERROR;
  let ret = __swift_stdlib_unorm2_getInstance(
    nil, "nfc", __swift_stdlib_UNORM2_COMPOSE_CONTIGUOUS, &err)
  _precondition(err.isSuccess, "unexpected unorm2_getInstance failure")
  return ret!
}

// Michael NOTE: made public for prototype, should be internal
public var _fccNormalizer = _makeFCCNormalizer()

extension _XUnicodeViews {
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
*/
