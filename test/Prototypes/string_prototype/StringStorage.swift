import Swift
import SwiftShims

//===--- General Utilities ------------------------------------------------===//

//===--- String Specifics -------------------------------------------------===//
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
  var rawUTF16: RawUTF16View { get }

  /// A type that presents an FCC-normalized view of the string
  associatedtype FCCNormalizedUTF16View : BidirectionalCollection
  // where Iterator.Element == UInt16

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16 : FCCNormalizedUTF16View { get }
}

extension _FixedFormatUnicode {
  var encoding: AnyUnicodeEncoding.Type {
    return encoding as Encoding.Type
  }
}

/// Default implementations
extension _FixedFormatUnicode {
  
  var isKnownLatin1: Bool { return false }
  var isKnownASCII: Bool { return false }
  var isKnownValidEncoding: Bool { return false }
  var isKnownFCCNormalized: Bool { return false }
  var isKnownFCDForm: Bool {
    return isKnownFCCNormalized || isKnownNFDNormalized
  }
}

extension _FixedFormatUnicode
where RawUTF16View.Iterator.Element : UnsignedInteger {
  // FIXME: we'd like to put this up in the unconditional extension, but we are
  // forbidden.
  var encoding: Encoding.Type { return Encoding.self }
  
  func isLatin1() -> Bool {
    return isKnownLatin1 || !rawUTF16.contains { $0 > 0xFF }
  }
  
  func isASCII() -> Bool {
    return isKnownASCII || !rawUTF16.contains { $0 > 0x7f }
  }
}

extension _FixedFormatUnicode
where RawUTF16View.Iterator.Element : UnsignedInteger,
  CodeUnits.SubSequence : Collection, 
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == Encoding.EncodedScalar.Iterator.Element
{
  func isValidEncoding() -> Bool {
    return Encoding.parseForward(
      codeUnits, repairingIllFormedSequences: false
    ) { _ in }.errorCount == 0
  }
}

//===--- Defaults for Latin-1 ---------------------------------------------===//
extension _FixedFormatUnicode where Encoding == Latin1 {
  var isKnownLatin1: Bool { return true }
  var isKnownValidEncoding: Bool { return true }
  var isKnownFCCNormalized: Bool { return true }
}
  
extension _FixedFormatUnicode
where Encoding == Latin1, CodeUnits.Iterator.Element : UnsignedInteger {
  var rawUTF16 : LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return fccNormalizedUTF16
  }

  /// An FCC-normalized view of the string
  var fccNormalizedUTF16 : LazyMapRandomAccessCollection<CodeUnits, UInt16> {
    return codeUnits.lazy.map { numericCast($0) }
  }
}

//===--- Defaults for UTF16 and ValidUTF16 --------------------------------===//
extension _FixedFormatUnicode
where Encoding.EncodedScalar == UTF16.EncodedScalar,
  CodeUnits.Iterator.Element : UnsignedInteger,
  CodeUnits.Iterator.Element == UTF16.CodeUnit,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element {
  
  // FIXME: we should have a way to represent the validity of the encoding of
  // this result—and maybe other nice properties—in the type system.  So maybe
  // this thing should conform to _FixedFormatUnicode
  var fccNormalizedUTF16
  : UnicodeStorage<CodeUnits,Encoding>.FCCNormalizedUTF16View {
    return UnicodeStorage(codeUnits, Encoding.self).fccNormalizedUTF16
  }
  
  var rawUTF16 : CodeUnits {
    return codeUnits
  }
}

extension _SwiftUTF16StringHeader : _BoundedBufferHeader {
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}
extension _SwiftLatin1StringHeader : _BoundedBufferHeader {
  public init(count: Int, capacity: Int) {
    self.count = numericCast(count)
    self.capacity = numericCast(capacity)
    self.flags = 0
  }
}

/// Common base class of our string storage classes
@_versioned
class _StringStorageBase<
  Header: _BoundedBufferHeader,
  Element: UnsignedInteger
> :
  // Dynamically provides inheritance from NSString
  _SwiftNativeNSString,
  // Allows us to code init in terms of Builtin.allocWithTailElems_1
  FactoryInitializable  
{
  var _header: _SwiftUTF16StringHeader
  
  @objc
  final public func length() -> Int {
    return numericCast(_header.count)
  }

  @objc(copyWithZone:)
  final public func copy(with: _SwiftNSZone?) -> AnyObject {
    return self
  }
  
  // satisfies the compiler's demand for a designated initializer
  init(_doNotCallMe: ()) { fatalError("do not call me") }

  @nonobjc
  convenience init(uninitializedWithMinimumCapacity n: Int) {
    self.init(
      Builtin.allocWithTailElems_1(
        type(of: self), n._builtinWordValue, Element.self))
  }

  @objc
  final public func characterAtIndex(_ index: Int) -> UInt16 {
    defer { _fixLifetime(self) }
    return numericCast(_baseAddress[index])
  }

  @nonobjc
  var _baseAddress: UnsafeMutablePointer<Element> {
    // WORKAROUND: rdar://31047127 prevents us from implementing _baseAddress as
    // final here.
    fatalError("Override me!")
  }
}

//===--- UTF16 String Storage ---------------------------------------------===//
@_versioned
final class _UTF16StringStorage
  : _StringStorageBase<_SwiftUTF16StringHeader, UTF16.CodeUnit>
  , _NSStringCore // Ensures that we implement essential NSString methods.  
{
  // WORKAROUND: helping type inference along will be unnecessary someday
  typealias _Element = UInt16
  typealias Iterator = IndexingIterator<_UTF16StringStorage>
  
  //===--- _NSStringCore conformance --------------------------------------===//
  // There doesn't seem to be a way to write these in an extension

  /// Returns a pointer to contiguously-stored UTF-16 code units
  /// comprising the whole string, or NULL if such storage isn't
  /// available.
  ///
  /// WARNING: don't use this method from Swift code; ARC may end the
  /// lifetime of self before you get a chance to use the result.
  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return _baseAddress
  }

  /// Returns a pointer to contiguously-stored code units in the
  /// system encoding comprising the whole string, or NULL if such
  /// storage isn't available.
  ///
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

  // WORKAROUND: rdar://31047127 prevents us from hoisting this into
  // _StringStorageBase
  @nonobjc
  override var _baseAddress: UnsafeMutablePointer<UTF16.CodeUnit> {
    return UnsafeMutablePointer(
      Builtin.projectTailElems(self, Element.self))
  }
}

extension _UTF16StringStorage : _BoundedBufferReference {
  /// Returns empty singleton that is used for every single empty String.
  /// The contents of the storage should never be mutated.
  @nonobjc
  internal static func _emptyInstance() -> _UTF16StringStorage {
    return Builtin.bridgeFromRawPointer(
      Builtin.addressof(&_swiftEmptyStringStorage))
  }
}

extension _UTF16StringStorage : _FixedFormatUnicode {
  typealias Encoding = UTF16

  // WORKAROUND: helping type inference along will be unnecessary someday
  typealias RawUTF16View = _UTF16StringStorage
  typealias CodeUnits = _UTF16StringStorage
  typealias FCCNormalizedUTF16View = UnicodeStorage<
    CodeUnits, Encoding
  >.FCCNormalizedUTF16View
  
  var encoding: UTF16.Type { return UTF16.self }
  var codeUnits: _UTF16StringStorage { return self }
  
  @nonobjc
  var isKnownASCII: Bool {
    get { return _header.flags & 1<<0 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt16 }
      else { _header.flags &= ~(1<<0) }
    }
  }

  @nonobjc
  var isKnownLatin1: Bool {
    get { return _header.flags & 1<<1 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<1 as UInt16 }
      else { _header.flags &= ~(1<<1) as UInt16 }
    }
  }
  
  @nonobjc
  var isKnownValidEncoding: Bool {
    get { return _header.flags & 1<<2 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<2 as UInt16 }
      else { _header.flags &= ~(1<<2) as UInt16 }
    }
  }
  
  @nonobjc
  var isKnownFCCNormalized: Bool {
    get { return _header.flags & 1<<3 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<3 as UInt16 }
      else { _header.flags &= ~(1<<3) as UInt16 }
    }
  }
  
  @nonobjc
  var isKnownNFCNormalized: Bool {
    get { return _header.flags & 1<<4 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<4 as UInt16 }
      else { _header.flags &= ~(1<<4) as UInt16 }
    }
  }
  
  @nonobjc
  var isKnownNFDNormalized: Bool {
    get { return _header.flags & 1<<5 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<5 as UInt16 }
      else { _header.flags &= ~(1<<5) as UInt16 }
    }
  }

  
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
      if _fastPath(isKnownASCII || utf16CodeUnitValues.isEmpty) {
        // Don't look for the maximal code unit value; we already know everything
        // we can learn from it
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
      else {
        // FIXME: hoping for loop fusion here; check to make sure we get it.
        maxCodeUnit = numericCast(utf16CodeUnitValues.max()!)
        utf16CodeUnitValues.lazy.map {
          numericCast($0)
        }._copyCompleteContents(initializing: $0)
      }
    }
    _setMaxStored(maxCodeUnit)
  }

  @nonobjc
  convenience init(
    count: Int,
    minimumCapacity: Int = 0
  ) {
    self.init(minimumCapacity: Swift.max(count, minimumCapacity)) {
      _SwiftUTF16StringHeader(
        count: UInt32(count), capacity: UInt32($0), flags: 0)
    }
  }

  // Eventually this should work for _AnyUnicode existentials too.
  @nonobjc
  internal convenience init<Other: _FixedFormatUnicode>(
    _ other: Other
  )
  where Other.CodeUnits.Iterator.Element : UnsignedInteger,

  // FIXME: drop these constraints once we have the compiler features.
  Other.CodeUnits.Index == Other.CodeUnits.SubSequence.Index, 
  Other.CodeUnits.SubSequence : RandomAccessCollection, 
  Other.CodeUnits.SubSequence == Other.CodeUnits.SubSequence.SubSequence, 
  Other.CodeUnits.Iterator.Element == Other.CodeUnits.SubSequence.Iterator.Element, 
  Other.CodeUnits.SubSequence.Iterator.Element == Other.Encoding.EncodedScalar.Iterator.Element  
  {
    let otherIsLatin1 = Other.Encoding.self is Latin1.Type
    let otherIsUTF16Compatible = otherIsLatin1
      || Other.Encoding.EncodedScalar.self is UTF16.EncodedScalar.Type

    if _fastPath(otherIsUTF16Compatible) {
      self.init(
        utf16CodeUnitValues: other.codeUnits,
        isKnownASCII: other.isKnownASCII)
      
      if other.isKnownValidEncoding { isKnownValidEncoding = true }
      if other.isKnownFCCNormalized { isKnownFCCNormalized = true }
      if other.isKnownNFCNormalized { isKnownNFCNormalized = true }
      if other.isKnownNFDNormalized { isKnownNFDNormalized = true }
    }
    else {
      var count = 0
      var maxCodeUnit: UInt16 = 0
      Other.Encoding.parseForward(
        other.codeUnits,
        repairingIllFormedSequences: true
      ) {
        count += $0.utf16.count
        maxCodeUnit = Swift.max(maxCodeUnit, $0.utf16.max()!)
      }

      self.init(count: count)
      withUnsafeMutableBufferPointer {
        UnicodeStorage(
          other.codeUnits, Other.Encoding.self).transcoded(
          to: UTF16.self
        )._copyCompleteContents(initializing: $0)
      }
      _setMaxStored(maxCodeUnit)
      isKnownValidEncoding = true // repairs had to be made
    }
  }
}

//===--- Latin-1 String Storage -------------------------------------------===//
@_versioned
final class _Latin1StringStorage
  : _StringStorageBase<_SwiftLatin1StringHeader, UInt8>
  , _NSStringCore // Ensures that we implement essential NSString methods.  
{
  // WORKAROUND: helping type inference along will be unnecessary someday
  typealias _Element = UInt8
  typealias Iterator = IndexingIterator<_Latin1StringStorage>
  
  /// Returns a pointer to contiguously-stored UTF-16 code units
  /// comprising the whole string, or NULL if such storage isn't
  /// available.
  ///
  /// WARNING: don't use this method from Swift code; ARC may end the
  /// lifetime of self before you get a chance to use the result.
  @objc
  func _fastCharacterContents() -> UnsafeMutablePointer<UInt16>? {
    return nil
  }

  /// Returns a pointer to contiguously-stored code units in the
  /// system encoding comprising the whole string, or NULL if such
  /// storage isn't available.
  ///
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
  
  // WORKAROUND: rdar://31047127 prevents us from hoisting this into
  // _StringStorageBase
  @nonobjc
  override var _baseAddress: UnsafeMutablePointer<Latin1.CodeUnit> {
    return UnsafeMutablePointer(
      Builtin.projectTailElems(self, Element.self))
  }
}

extension _Latin1StringStorage : _BoundedBufferReference {
  @nonobjc
  internal static func _emptyInstance() -> _Latin1StringStorage {
    return _Latin1StringStorage(uninitializedWithMinimumCapacity: 0)
  }
}

extension _Latin1StringStorage : _FixedFormatUnicode {
  typealias Encoding = Latin1
  
  // WORKAROUND: helping type inference along will be unnecessary someday
  typealias CodeUnits = _Latin1StringStorage
  typealias FCCNormalizedUTF16View = LazyMapRandomAccessCollection<CodeUnits, UTF16.CodeUnit>
  typealias RawUTF16View = FCCNormalizedUTF16View
  
  var codeUnits: CodeUnits { return self }

  var isKnownNFDNormalized: Bool { return true }
  var isKnownNFCNormalized: Bool { return true }

  @nonobjc
  var isKnownASCII: Bool {
    get { return _header.flags & 1<<0 as UInt16 != 0 }
    set {
      if newValue { _header.flags |= 1<<0 as UInt16 }
      else { _header.flags &= ~(1<<0) }
    }
  }
}

/// - Requires: Element is trivial (UInt8/UInt16)
struct _StringBuffer<Storage: _BoundedBufferReference> {
  internal var _storage: Storage

  init(_ storage: Storage) { self._storage = storage }

  init() {
    self.init(Storage._emptyInstance())
  }

  init(_buffer source: _StringBuffer, shiftedToStartIndex: Int) {
    _sanityCheck(shiftedToStartIndex == 0, "shiftedToStartIndex must be 0")
    self.init(source._storage)
  }

  init(_uninitializedCount: Int, minimumCapacity: Int) {
    _storage = Storage(
      _uninitializedCount: _uninitializedCount,
      minimumCapacity: minimumCapacity)
  }
}

extension _StringBuffer : _ArrayBufferProtocol, _ContiguousBufferProtocol {
  typealias Element = Storage.Iterator.Element
  
  var count: Int { 
    get { return _storage.count }
    nonmutating set { _storage.count = newValue }
  }

  var capacity: Int { return _storage.capacity }
  var owner: AnyObject { return _storage }
  
  var firstElementAddress: UnsafeMutablePointer<Element> {
    return UnsafeMutablePointer(_storage._baseAddress)
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

  subscript(i: Int) -> Storage.Element {
    // FIXME: Add addressors
    get {
      return withUnsafeBufferPointer { $0[i] }
    }
    nonmutating set {
      withUnsafeMutableBufferPointer { $0[i] = newValue }
    }
  }
}
