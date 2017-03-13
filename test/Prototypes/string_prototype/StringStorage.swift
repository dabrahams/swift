// Prototype code to be added to an eventual StringStorage.swift

import Swift
import SwiftShims

// This is a hack to work around the inability to assign to self in a class init
protocol FactoryInitializable {}
extension FactoryInitializable {
  @nonobjc
  init(_ me: Self) {
    self = me
  }
}

protocol ContiguouslyStored : RandomAccessCollection {
  func withUnsafeBufferPointer<R>(
    _: (UnsafeBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

protocol MutableContiguouslyStored : ContiguouslyStored {
  mutating func withUnsafeMutableBufferPointer<R>(
    _: (UnsafeMutableBufferPointer<Iterator.Element>) throws -> R
  ) rethrows -> R
}

protocol _AnyUnicode {
  var encoding: AnyUnicodeEncoding.Type { get }
  
  var isKnownLatin1: Bool { get }
  var isKnownASCII: Bool { get }
  var isKnownValidEncoding: Bool { get }
  var isKnownFCCNormalized: Bool { get }
  var isKnownFCDForm: Bool { get }
  var isKnownNFDNormalized: Bool { get }
  var isKnownNFCNormalized: Bool { get }

  func isLatin1() -> Bool
  func isASCII() -> Bool
  func isValidEncoding() -> Bool
}

protocol _FixedFormatUnicode : _AnyUnicode {
  associatedtype Encoding: UnicodeEncoding
  var encoding: Encoding.Type { get }
  
  // func isFCCNormalized() -> Bool
  
  associatedtype CodeUnits : RandomAccessCollection
  // where Iterator.Element == Encoding.CodeUnit
  
  var codeUnits : CodeUnits { get }
  
  /// A type that presents the string's UTF-16 code units without necessarily
  /// correcting encoding errors
  associatedtype RawUTF16View : BidirectionalCollection
  // where Iterator.Element == UInt16

  /// The string's UTF-16 code units, without necessarily correcting encoding
  /// errors.
  var rawUTF16 : RawUTF16View { get }

  /// A type that presents an FCC-normalized view of the string
  associatedtype FCCNormalizedUTF16View : BidirectionalCollection
  // where Iterator.Element == UInt16

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16 : FCCNormalizedUTF16View { get }
}

/// Default implementations
extension _FixedFormatUnicode
where RawUTF16View.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : Collection, 
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == Encoding.EncodedScalar.Iterator.Element
{
  var encoding: Encoding.Type { return Encoding.self }
  
  var isKnownLatin1: Bool { return false }
  var isKnownASCII: Bool { return false }
  var isKnownValidEncoding: Bool { return false }
  var isKnownFCCNormalized: Bool { return false }
  var isKnownFCDForm: Bool {
    return isKnownFCCNormalized || isKnownNFDNormalized
  }

  func isLatin1() -> Bool {
    return isKnownLatin1 || !rawUTF16.contains { $0 > 0xFF }
  }
  
  func isASCII() -> Bool {
    return isKnownASCII || !rawUTF16.contains { $0 > 0x7f }
  }
  
  func isValidEncoding() -> Bool {
    return Encoding.parseForward(
      codeUnits, repairingIllFormedSequences: false
    ) { _ in }.errorCount == 0
  }
}

/// Latin-1
extension _FixedFormatUnicode
where Encoding == Latin1, CodeUnits.Iterator.Element : UnsignedInteger {

  var isKnownLatin1: Bool { return true }
  var isKnownValidEncoding: Bool { return true }
  var isKnownFCCNormalized: Bool { return true }

  var rawUTF16 : LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return codeUnits.lazy.map { numericCast($0) }
  }

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16 : LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return codeUnits.lazy.map { numericCast($0) }
  }
}

/// UTF16 and ValidUTF16
extension _FixedFormatUnicode
where Encoding.EncodedScalar == UTF16.EncodedScalar,
  CodeUnits.Iterator.Element == UTF16.CodeUnit,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element{

  var rawUTF16 : CodeUnits {
    return codeUnits
  }

  // FIXME: we should have a way to represent the validity of the encoding of
  // this result—and maybe other nice properties—in the type system.  So maybe
  // this thing should conform to _FixedFormatUnicode
  var fccNormalizedUTF16
  : UnicodeStorage<CodeUnits,Encoding>.FCCNormalizedUTF16View {
    return UnicodeStorage(codeUnits, Encoding.self).fccNormalizedUTF16
  }
}

@nonobjc
protocol _DynamicStringBuffer : _NSStringCore {
  associatedtype Element
  @nonobjc var count: Int
  @nonobjc var capacity: Int
}

extension _DynamicStringBuffer {
  @nonobjc
  internal func allocatedCapacity() -> Int {
    let startAddr = UnsafeRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let selfAddr = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let endAddr = startAddr + _swift_stdlib_malloc_size(selfAddr)
    return endAddr.assumingMemoryBound(to: Element.self)
         - startAddr.assumingMemoryBound(to: Element.self)
  }
  
  @nonobjc
  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws->R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    let base = UnsafeMutablePointer<Element>(
      Builtin.projectTailElems(self, Element.self))
    var buffer = UnsafeMutableBufferPointer(start: base, count: count)
    return try body(&buffer)
  }

  @nonobjc
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws->R
  ) rethrows -> R {
    return try withUnsafeMutableBufferPointer {
      try body(UnsafeBufferPointer(start: $0.baseAddress, count: $0.count))
    }
  }

  @nonobjc
  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    set {
      return withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}

//===--- Implementation of _NSStringCore requirements ---------------------===//
extension _DynamicStringBuffer {
  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(self[index])
  }

  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }

    // Need to do manual projection, because compiler can't prove
    // firstElementAddress has the right type.
    return UnsafeMutablePointer(Builtin.projectTailElems(self, Element.self))
  }

  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }
  
  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

/// Buffer of contiguously-stored UTF-16-compatible code units, meaning it can
/// hold ASCII, Latin1, or UTF-16.
/// 
/// - Requires: Element is trivial (UInt8/UInt16 in practice)
@_versioned
class _StringStorage<
  Header,
  Element : UnsignedInteger, 
  Derived : AnyObject
>
  : _SwiftNativeNSString, // Dynamically provides inheritance from NSString
    _NSStringCore         // Constrains us to supply essential NSString methods
{
  @nonobjc
  var _header: Header
  
  @nonobjc
  var count: Int {
    get { return numericCast(_header.count) }
    set { _header.count = numericCast(newValue) }
  }
  
  @nonobjc
  var capacity: Int {
    get { return numericCast(_header.capacity) }
    set { _header.capacity = numericCast(newValue) }
  }

  /// Satisfies the compiler's need for a designated initializer.
  @nonobjc
  internal init(_NeverActuallyCalled: ()) {
    fatalError("Unexpected call to StringStorage designated initializer")
  }

  @nonobjc
  internal func allocatedCapacity() -> Int {
    let startAddr = UnsafeRawPointer(
      Builtin.projectTailElems(self, Element.self))
    let selfAddr = UnsafeRawPointer(Builtin.bridgeToRawPointer(self))
    let endAddr = startAddr + _swift_stdlib_malloc_size(selfAddr)
    return endAddr.assumingMemoryBound(to: Element.self)
         - startAddr.assumingMemoryBound(to: Element.self)
  }

  @nonobjc
  convenience init(
    count: Int,
    minimumCapacity: Int = 0
  ) {
    self.init(
      unsafeDowncast(
        Builtin.allocWithTailElems_1(
          Derived.self,
          Swift.max(count, minimumCapacity)._builtinWordValue, Element.self),
        to: Self.self))
  }
  
  /// The empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  @nonobjc
  internal static var emptyInstance: _StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }

  @nonobjc
  func withUnsafeMutableBufferPointer<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws->R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    let base = UnsafeMutablePointer<Element>(
      Builtin.projectTailElems(self, Element.self))
    var buffer = UnsafeMutableBufferPointer(start: base, count: count)
    return try body(&buffer)
  }

  @nonobjc
  func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws->R
  ) rethrows -> R {
    return try withUnsafeMutableBufferPointer {
      try body(UnsafeBufferPointer(start: $0.baseAddress, count: $0.count))
    }
  }

  //===--- Implementation of _NSStringCore requirements -------------------===//
  // Can't use an extension because of https://bugs.swift.org/browse/SR-4173
  // @objc is not supported within extensions of generic classes

  @objc
  func length() -> Int {
    return count
  }

  @objc
  func characterAtIndex(_ index: Int) -> UInt16 {
    return numericCast(self[index])
  }

  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    guard Element.self is UInt16.Type else { return nil }

    // Need to do manual projection, because compiler can't prove
    // firstElementAddress has the right type.
    return UnsafeMutablePointer(Builtin.projectTailElems(self, Element.self))
  }

  // WARNING: don't use this method from Swift code; ARC may end the lifetime of
  // self before you get a chance to use the result.
  // WARNING: Before you implement this as anything other than “return nil,”
  // see https://github.com/apple/swift/pull/3151#issuecomment-285583557
  @objc
  public func _fastCStringContents(
    _ nullTerminationRequired: Int8
  ) -> UnsafePointer<CChar>? {
    return nil
  }
  
  @objc(copyWithZone:)
  internal func copy(with _: _SwiftNSZone?) -> AnyObject {
    return self
  }
}

class X {
  
}
final class _UTF16StringStorage
  : _StringStorage<_SwiftUTF16StringHeader, UTF16.CodeUnit, X> {
  
  @nonobjc
  var isKnownLatin1: Bool {
    get { return _header.flags & 1<<0 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt8 }
      else { _header.flags &= ~(1<<0) as UInt8 }
    }
  }
  
  @nonobjc
  var isKnownASCII: Bool {
    get { return _header.flags & 1<<1 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<1 as UInt8 }
      else { _header.flags &= ~(1<<1) }
    }
  }

  @nonobjc
  var isKnownValidEncoding: Bool {
    get { return _header.flags & 1<<2 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<2 as UInt8 }
      else { _header.flags &= ~(1<<2) as UInt8 }
    }
  }
  
  @nonobjc
  var isKnownFCCNormalized: Bool {
    get { return _header.flags & 1<<3 as UInt8 != 0 }
    set {
      if newValue { _header.flags |= 1<<3 as UInt8 }
      else { _header.flags &= ~(1<<3) as UInt8 }
    }
  }
}

extension _StringStorage : RandomAccessCollection, MutableCollection {
  @nonobjc
  var startIndex : Int { return 0 }
  @nonobjc
  var endIndex : Int { return count }

  @nonobjc
  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    set {
      return withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}

extension _UTF16StringStorage {
  @nonobjc
  internal func _setMaxStored(_ maxCodeUnit: UInt16) {
    switch maxCodeUnit {
    case 0..<0x80: self.isKnownASCII = true; fallthrough
    case 0..<0x100: self.isKnownLatin1 = true; fallthrough
    case 0..<0x300: self.isKnownFCCNormalized = true; fallthrough
    case 0..<0xD800: self.isKnownValidEncoding = true
    default: break
    }
  }
  
  /// Initialize from a sequence of valid UTF16 code unit values (possibly
  /// represented with a different code unit type).
  @nonobjc
  internal convenience init<OtherCodeUnits : Collection>(
    utf16CodeUnitValues: OtherCodeUnits,
    isKnownASCII: Bool = false
  )
  // FIXME: when new integers land, we won't need this constraint anymore.
  where OtherCodeUnits.Iterator.Element : UnsignedInteger {
    // No need for transcoding, since we're not trying to ensure our own
    // encoding is valid UTF16.  We'll just copy the same code units (possibly
    // zero-extended).
    self.init(count: numericCast(utf16CodeUnitValues.count))
    
    var maxCodeUnit: UInt16 = 0
    withUnsafeMutableBufferPointer {
      if _fastPath(isKnownASCII) {
        // Don't look for the maximal code unit value; we already know everything
        // we can learn from it
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
      else {
        // FIXME: hoping for loop fusion here.
        maxCodeUnit = numericCast(utf16CodeUnitValues.max()!)
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
    }
    _setMaxStored(maxCodeUnit)
  }
  
  @nonobjc
  internal convenience init<OtherCodeUnits, OtherEncoding>(
    _ other: UnicodeStorage<OtherCodeUnits, OtherEncoding>,
    isKnownLatin1: Bool = false,
    isKnownASCII: Bool = false,
    isKnownValidEncoding: Bool = false,
    isKnownFCCNormalized: Bool = false
  )
  // FIXME: when new integers land, we won't need this constraint anymore.
  where OtherCodeUnits.Iterator.Element : UnsignedInteger
  {
    let isLatin1 = OtherEncoding.self is Latin1.Type
    let isUTF16Compatible = isLatin1
      || OtherEncoding.EncodedScalar.self is UTF16.EncodedScalar.Type

    // Avoid transcoding if we can
    if _fastPath(isUTF16Compatible) {
      self.init(
        utf16CodeUnitValues: other.codeUnits, isKnownASCII: isKnownASCII)
    }
    else {
      self.init(
        utf16CodeUnitValues: other.transcoded(to: UTF16.self),
        isKnownASCII: isKnownASCII)
      self.isKnownValidEncoding = true
    }
    if isKnownLatin1 { self.isKnownLatin1 = true }
    if isKnownValidEncoding { self.isKnownValidEncoding = true }
    if isKnownFCCNormalized { self.isKnownFCCNormalized = true }
  }    
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Element: UnsignedInteger> {
  internal var _storage: _StringStorage<Element>

  init(_ storage: _StringStorage<Element>) { self._storage = storage }

  init() {
    self.init(_StringStorage.emptyInstance)
  }

  init(_buffer source: Buffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    self.init(source._storage)
  }

  init(_uninitializedCount: Int, minimumCapacity: Int) {
    _storage = _UTF16StringStorage(
      count: _uninitializedCount, minimumCapacity: minimumCapacity)
      as Any as! _StringStorage<Element>
  }

}

extension _StringBuffer : _ArrayBufferProtocol, _ContiguousBufferProtocol {
  var count: Int { 
    get { return _storage.count }
    nonmutating set { _storage.count = newValue }
  }

  var capacity: Int { return _storage.capacity }
  var owner: AnyObject { return _storage }
  
  var firstElementAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(Builtin.projectTailElems(_storage, Element.self))
  }
  
  var firstElementAddressIfContiguous: UnsafeMutablePointer<Element>? {
    return firstElementAddress
  }
  
  var identity: UnsafeRawPointer { return UnsafeRawPointer(firstElementAddress) }

  internal mutating func isUniquelyReferenced() -> Bool {
    return _isUnique(&_storage)
  }

  internal mutating func requestUniqueMutableBackingBuffer(minimumCapacity: Int)
  -> _StringBuffer? {
    if _fastPath(isUniquelyReferenced()) {
      if _fastPath(capacity >= minimumCapacity) {
        return self
      }
    }
    return nil
  }

  mutating func isMutableAndUniquelyReferenced() -> Bool {
    return isUniquelyReferenced()
  }

  func requestNativeBuffer() -> _StringBuffer? {
    return self
  }

  @_versioned
  internal func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeMutableBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @_versioned
  internal func withUnsafeBufferPointer<R>(
    _ body: (UnsafeBufferPointer<Element>) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try body(
      UnsafeBufferPointer(
        start: firstElementAddress,
        count: count
      )
    )
  }

  @discardableResult
  internal func _copyContents(
    subRange bounds: Range<Int>,
    initializing target: UnsafeMutablePointer<Element>
  ) -> UnsafeMutablePointer<Element> {
    _sanityCheck(bounds.lowerBound >= 0)
    _sanityCheck(bounds.upperBound >= bounds.lowerBound)
    _sanityCheck(bounds.upperBound <= count)

    defer { _fixLifetime(self) }

    let initializedCount = bounds.upperBound - bounds.lowerBound
    target.initialize(
      from: firstElementAddress + bounds.lowerBound, count: initializedCount)
    
    return target + initializedCount
  }

}

extension _StringBuffer : RandomAccessCollection, MutableCollection, 
    RangeReplaceableCollection {
  var startIndex : Int { return _storage.startIndex }
  var endIndex : Int { return _storage.endIndex }

  subscript(i: Int) -> Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    nonmutating set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}
