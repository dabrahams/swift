import Swift

extension _BoundedBufferReference {
  /// Calls `body` on a mutable buffer that covers the entire extent of
  /// allocated memory.
  func _withMutableCapacity<R>(
    body: (inout UnsafeMutableBufferPointer<Element>)->R
  ) -> R {
    return self.withUnsafeMutableBufferPointer { buf in
      var fullBuf = UnsafeMutableBufferPointer(
        start: buf.baseAddress, count: capacity)
      return body(&fullBuf)
    }
  }
}

internal struct _Concat3<C0: Collection, C1: Collection, C2: Collection>
where C0.Element == C1.Element, C1.Element == C2.Element {
  var c0: C0
  var c1: C1
  var c2: C2

  init(_ c0: C0, _ c1: C1, _ c2: C2) {
    self.c0 = c0
    self.c1 = c1
    self.c2 = c2
  }
}

extension _Concat3 : Sequence {
  struct Iterator : IteratorProtocol {
    var i0: C0.Iterator
    var i1: C1.Iterator
    var i2: C2.Iterator

    mutating func next() -> C0.Element? {
      if let r = i0.next() { return r }
      if let r = i1.next() { return r }
      return i2.next()
    }
  }

  func makeIterator() -> Iterator {
    return Iterator(
      i0: c0.makeIterator(),
      i1: c1.makeIterator(),
      i2: c2.makeIterator()
    )
  }
}

extension _Concat3 {
  public enum Index {
  case _0(C0.Index)
  case _1(C1.Index)
  case _2(C2.Index)
  }
}

extension _Concat3.Index : Comparable {
  static func == (lhs: _Concat3.Index, rhs: _Concat3.Index) -> Bool {
    switch (lhs, rhs) {
    case (._0(let l), ._0(let r)): return l == r
    case (._1(let l), ._1(let r)): return l == r
    case (._2(let l), ._2(let r)): return l == r
    default: return false
    }
  }
  
  static func < (lhs: _Concat3.Index, rhs: _Concat3.Index) -> Bool {
    switch (lhs, rhs) {
    case (._0, ._1), (._0, ._2), (._1, ._2): return true
    case (._1, ._0), (._2, ._0), (._2, ._1): return false
    case (._0(let l), ._0(let r)): return l < r
    case (._1(let l), ._1(let r)): return l < r
    case (._2(let l), ._2(let r)): return l < r
    }
  }
}

extension _Concat3 : Collection {
  var startIndex: Index {
    return !c0.isEmpty ? ._0(c0.startIndex)
         : !c1.isEmpty ? ._1(c1.startIndex) : ._2(c2.startIndex)
  }

  var endIndex: Index {
    return ._2(c2.endIndex)
  }

  func index(after i: Index) -> Index {
    switch i {
    case ._0(let j):
      let r = c0.index(after: j)
      if _fastPath(r != c0.endIndex) { return ._0(r) }
      if !c1.isEmpty { return ._1(c1.startIndex) }
      return ._2(c2.startIndex)
      
    case ._1(let j):
      let r = c1.index(after: j)
      if _fastPath(r != c1.endIndex) { return ._1(r) }
      return ._2(c2.startIndex)
      
    case ._2(let j):
      return ._2(c2.index(after: j))
    }
  }

  subscript(i: Index) -> C0.Element {
    switch i {
    case ._0(let j): return c0[j]
    case ._1(let j): return c1[j]
    case ._2(let j): return c2[j]
    }
  }
}

extension String {
  internal struct _XContent {
    var _ownerX: AnyObject?
    enum _Layout : UInt8 { case inline8, inline16, unowned8, unowned16, latin1, utf16, nsString }
    
    typealias _InlineStorage = (UInt64, UInt32, UInt16, UInt8)
    var _inlineBits: (_InlineStorage, UInt8)
    
    var _layout: _Layout {
      get {
        return _Layout(rawValue: _inlineBits.1 & 0b1111)._unsafelyUnwrappedUnchecked
      }
      set {
        _inlineBits.1 = _inlineBits.1 & ~0b1111 | UInt8(newValue.rawValue)
      }
    }
    
    internal struct _Inline<CodeUnit : FixedWidthInteger> {
      var _storage: _InlineStorage
      var _flags: UInt8 = 0
    }

    
    public var _inline8: _Inline<UInt8>? {
      guard _layout == .inline8 else { return nil }
      return unsafeBitCast(_inlineBits, to: _Inline<UInt8>.self)
    }

    init(_ x: _Inline<UInt8>) {
      _inlineBits = unsafeBitCast(x, to: (_InlineStorage, UInt8).self)
      _layout = .inline8
      _sanityCheck(_layout == .inline8)
    }
    
    public var _inline16: _Inline<UInt16>? {
      guard _layout == .inline16 else { return nil }
      return unsafeBitCast(_inlineBits, to: _Inline<UInt16>.self)
    }
     
    init(_ x: _Inline<UInt16>) {
      _inlineBits = unsafeBitCast(x, to: (_InlineStorage, UInt8).self)
      _layout = .inline16
      _sanityCheck(_layout == .inline16)
    }
    
    internal struct _Unowned<CodeUnit : FixedWidthInteger> {
      var _start: UnsafePointer<CodeUnit>
      var _count: UInt32
      var _flags: UInt16 = 0
      var _unused: UInt8 = 0
      
      var isASCII: Bool? {
        get {
          if _flags & 0b01 == 0 { return nil }
          return _flags & 0b10 != 0
        }
        set {
          _flags |= ~0b11
          if newValue == true { _flags |= 0b11 }
          else if newValue == false { _flags |= 0b01 }
        }
      }
      
      var isNULTerminated: Bool {
        get {
          return _flags & 0b100 != 0
        }
        set {
          _flags = _flags & ~0b100
          if newValue { _flags |= 0b100 }
        }
      }
    }
    
    public var _unowned8: _Unowned<UInt8>? {
      guard _layout == .unowned8 else { return nil }
      return unsafeBitCast(_inlineBits.0, to: _Unowned<UInt8>.self)
    }

    init(_ x: _Unowned<UInt8>) {
      _inlineBits = (
        unsafeBitCast(x, to: _InlineStorage.self),
        UInt8(_Layout.unowned8.rawValue))
    }
    
    public var _unowned16: _Unowned<UInt16>? {
      guard _layout == .unowned16 else { return nil }
      return unsafeBitCast(_inlineBits.0, to: _Unowned<UInt16>.self)
    }

    
    init(_ x: _Unowned<UInt16>) {
      _inlineBits = (
        unsafeBitCast(x, to: _InlineStorage.self),
        UInt8(_Layout.unowned16.rawValue))
    }
    
    public var _latin1: _Latin1Storage?  {
      guard _layout == .latin1 else { return nil }
      return unsafeDowncast(
        _ownerX._unsafelyUnwrappedUnchecked,
        to: _Latin1Storage.self)
    }

    init(_ x: _Latin1Storage) {
      _ownerX = x
      _inlineBits = ((0,0,0,0), UInt8(_Layout.latin1.rawValue))
    }
    
    public var _utf16: _UTF16Storage?  {
      guard _layout == .utf16 else { return nil }
      return unsafeDowncast(
        _ownerX._unsafelyUnwrappedUnchecked,
        to: _UTF16Storage.self)
    }
    
    init(_ x: _UTF16Storage) {
      _ownerX = x
      _inlineBits = ((0,0,0,0), UInt8(_Layout.utf16.rawValue))
    }
    
    public var _nsstring: _NSStringCore?  {
      guard _layout == .nsString else { return nil }
      return unsafeDowncast(
        _ownerX._unsafelyUnwrappedUnchecked, to: _NSStringCore.self)
    }

    init(_ x: _NSStringCore) {
      _ownerX = x
      _inlineBits = ((0,0,0,0), UInt8(_Layout.nsString.rawValue))
    }
  }
}

extension String._XContent {
  public typealias _Scratch = (UInt64, UInt64)
  internal static func _scratch() -> _Scratch { return (0,0) }
}

extension String._XContent._Inline {
  public var capacity: Int {
    return MemoryLayout.size(ofValue: _storage)
      / MemoryLayout<CodeUnit>.stride
  }

  public var count : Int {
    get {
      return Int(_flags &>> 4)
    }
    
    set {
      _flags = _flags & 0b1111 | UInt8(extendingOrTruncating: newValue) &<< 4
    }
  }
  
  public init?<S: Sequence>(_ s: S) where S.Element : BinaryInteger {
    _storage = (0,0,0,0)
    let failed: Bool = withUnsafeMutableBufferPointer {
      let start = $0.baseAddress._unsafelyUnwrappedUnchecked
      for i in s {
        guard count < capacity, let u = CodeUnit(exactly: i)
        else { return true }
        start[count] = u
        count = count &+ 1
      }
      return false
    }
    if failed { return nil }
  }

  public mutating func withUnsafeMutableBufferPointer<R>(
    _ body: (UnsafeMutableBufferPointer<CodeUnit>)->R
  ) -> R {
    let count = self.count
    let capacity = self.capacity
    return withUnsafeMutablePointer(to: &_storage) {
      let start = UnsafeMutableRawPointer($0).bindMemory(
        to: CodeUnit.self,
        capacity: capacity
      )
      return body(
        UnsafeMutableBufferPointer(start: start, count: count))
    }
  }

  /// Calls `body` on a mutable buffer that covers the entire extent of
  /// allocated memory.
  public mutating func _withMutableCapacity<R>(
    body: (inout UnsafeMutableBufferPointer<CodeUnit>)->R
  ) -> R {
    let capacity = self.capacity
    return self.withUnsafeMutableBufferPointer { buf in
      var fullBuf = UnsafeMutableBufferPointer(
        start: buf.baseAddress, count: capacity)
      return body(&fullBuf)
    }
  }
  
  public func copiedToUnsafeBuffer(in scratch: inout String._XContent._Scratch)
  -> UnsafeBufferPointer<CodeUnit> {
    return withUnsafeMutablePointer(to: &scratch) {
      UnsafeMutableRawPointer($0).storeBytes(
        of: _storage, as: String._XContent._InlineStorage.self)
      
      let start = UnsafeRawPointer($0).bindMemory(
        to: CodeUnit.self,
        capacity: capacity
      )
      _sanityCheck(start[count] == 0)
      return UnsafeBufferPointer(start: start, count: count)
    }
  }

  public mutating func append(_ u: CodeUnit) {
    let oldCount = count
    count = count &+ 1
    withUnsafeMutableBufferPointer { $0[oldCount] = u }
  }
}

extension String._XContent._Inline where CodeUnit == UInt8 {
  internal var isASCII : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0x8080_8080__8080_8080 as UInt64).littleEndian == 0
  }
}

extension String._XContent._Inline where CodeUnit == UInt16 {
  
  internal var isASCII : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF80_FF80__FF80_FF80 as UInt64).littleEndian == 0
  }
  
  internal var isLatin1 : Bool {
    return (UInt64(_storage.0) | UInt64(_storage.1) | UInt64(_storage.2))
      & (0xFF00_FF00__FF00_FF00 as UInt64).littleEndian == 0
  }
  
}

extension String._XContent._Unowned {
  init?(
    _ source: UnsafeBufferPointer<CodeUnit>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    guard
      let count = UInt32(exactly: source.count),
      let start = source.baseAddress
    else { return nil }

    self._count = count
    self._start = start
    self.isASCII = isASCII
    self.isNULTerminated = isNULTerminated
  }

  public var unsafeBuffer: UnsafeBufferPointer<CodeUnit> {
    return UnsafeBufferPointer(start: _start, count: Int(_count))
  }
}

extension String._XContent {

  init() {
    _inlineBits = (
      (0, 0, 0, 0), UInt8(extendingOrTruncating: _Layout.inline16.rawValue))
    _sanityCheck(_layout == .inline16)
    _sanityCheck(_inline16!.count == 0)
  }
  
  func _existingLatin1(
    in scratch: inout _Scratch
  ) -> UnsafeBufferPointer<UInt8>? {
    if let x = _inline8 { return x.copiedToUnsafeBuffer(in: &scratch) }
    if let x = _latin1 { return x.withUnsafeBufferPointer { return $0 } }
    if let x = _unowned8 { return x.unsafeBuffer }
      /*
    case .nsString(let x):
      return x._fastCStringContents(false).map {
        UnsafeBufferPointer(start: x, count: x.length())
      }
      */
    return nil
  }

  func _existingUTF16(
    in scratch: inout _Scratch
  ) -> UnsafeBufferPointer<UInt16>? {
    if let x = _inline16 { return x.copiedToUnsafeBuffer(in: &scratch) }
    if let x = _utf16 { return x.withUnsafeBufferPointer { $0 } }
    if let x = _unowned16 { return x.unsafeBuffer }
    if let x = _nsstring {
      return x._fastCharacterContents().map {
        UnsafeBufferPointer(start: $0, count: x.length())
      }
    }
    return nil
  }

  var isASCII: Bool? {
    get {
      if let x = _inline8 { return x.isASCII }
      if let x = _inline16 { return x.isASCII  }
      if let x = _unowned8 { return x.isASCII }
      if let x = _unowned16 { return x.isASCII }
      if let x = _latin1 { return x.isASCII }
      if let x = _utf16 { return x.isASCII }
      return nil
    }
  }
}

extension String._XContent {
  struct UTF16View {
    var _content: String._XContent
  }
  
  var _nsString : _NSStringCore {
    if let x = _nsstring { return x }
    if let x = _utf16 { return x }
    if let x = _latin1 { return x }
    _sanityCheckFailure("unreachable")
  }
}

struct _TruncExt<Input: BinaryInteger, Output: FixedWidthInteger>
: _Function {
  func apply(_ input: Input) -> Output {
    return Output(extendingOrTruncating: input)
  }
}

extension String._XContent.UTF16View : Sequence {
  struct Iterator : IteratorProtocol {
    internal enum _Buffer {
    case deep8(UnsafePointer<UInt8>, UnsafePointer<UInt8>)
    case deep16(UnsafePointer<UInt16>, UnsafePointer<UInt16>)
    case inline8(String._XContent._Inline<UInt8>, UInt8)
    case inline16(String._XContent._Inline<UInt16>, UInt8)
    case nsString(Int)
    }
    
    internal var _buffer: _Buffer
    internal var _owner: AnyObject?

    @inline(__always)
    init(_ content: String._XContent) {
      defer { _fixLifetime(content) }
      if let x = content._inline8 { _buffer = .inline8(x, 0) }
      else if let x = content._inline16 { _buffer = .inline16(x, 0) }
      else if let x = content._unowned8 {
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep8(s, s + b.count)
      }
      else if let x = content._unowned16 {
        _owner = nil
        let b = x.unsafeBuffer
        let s = b.baseAddress._unsafelyUnwrappedUnchecked
        _buffer = _Buffer.deep16(s, s + b.count)
      }
      else if let x = content._latin1 {
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep8(s, s + $0.count)
        }
      }
      else if let x = content._utf16 {
        _owner = x
        _buffer = x.withUnsafeBufferPointer {
          let s = $0.baseAddress._unsafelyUnwrappedUnchecked
          return .deep16(s, s + $0.count)
        }
      }
      else if let x = content._nsstring {
        _buffer = .nsString(0)
        _owner = x
      }
      else {
        _sanityCheckFailure("Unreachable")
      }
    }

    @inline(__always)
    mutating func next() -> UInt16? {
      switch _buffer {
      case .deep8(let start, let end):
        guard start != end else { return nil }
        _buffer = .deep8(start + 1, end)
        return UInt16(start.pointee)
      case .deep16(let start, let end):
        guard start != end else { return nil }
        _buffer = .deep16(start + 1, end)
        return start.pointee
      case .inline8(var x, let i):
        return x.withUnsafeMutableBufferPointer {
          if i == $0.count { return nil }
          _buffer = .inline8(x, i + 1)
          return UInt16($0[Int(i)])
        }
      case .inline16(var x, let i):
        return x.withUnsafeMutableBufferPointer {
          if i == $0.count { return nil }
          _buffer = .inline16(x, i + 1)
          return $0[Int(i)]
        }
      case .nsString(let i):
        let s = unsafeBitCast(_owner, to: _NSStringCore.self)
        if i == s.length() { return nil }
        _buffer = .nsString(i + 1)
        return s.characterAtIndex(i)
      }
    }
  }

  @inline(__always)
  func makeIterator() -> Iterator {
    return Iterator(_content)
  }
}

extension String._XContent.UTF16View : BidirectionalCollection {
  init<C : Collection>(
    _ c: C, maxElement: UInt16? = nil, minCapacity: Int = 0
  )
  where C.Element == UInt16 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .init(x)
    }
    else if let x = String._XContent._Inline<UInt16>(c) {
      _content = .init(x)
    }
    else  {
      let maxCodeUnit = maxElement ?? c.max() ?? 0
      if maxCodeUnit <= 0xFF {
        _content = .init(
          unsafeDowncast(
            _mkLatin1(
              _MapCollection(c, through: _TruncExt()),
              minCapacity: minCapacity,
              isASCII: maxCodeUnit <= 0x7f),
            to: String._Latin1Storage.self))
      }
      else {
        _content = .init(
          unsafeDowncast(
            _mkUTF16(
              c,
              minCapacity: minCapacity,
              maxElement: maxCodeUnit),
            to: String._UTF16Storage.self))
      }
    }
  }
  
  init<C : Collection>(
    _ c: C, minCapacity: Int = 0, isASCII: Bool? = nil
  ) where C.Element == UInt8 {
    if let x = String._XContent._Inline<UInt8>(c) {
      _content = .init(x)
    }
    else {
      _content = .init(//.init(c)
        unsafeDowncast(
          _mkLatin1(c, minCapacity: minCapacity, isASCII: isASCII),
          to: String._Latin1Storage.self))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt8>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .init(x)
    }
    else if let x = String._XContent._Unowned<UInt8>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .init(x)
    }
    else {
      _content = .init(
        unsafeDowncast(
          _mkLatin1(source, isASCII: isASCII),
          to: String._Latin1Storage.self))
    }
  }
  
  init(
    unowned source: UnsafeBufferPointer<UInt16>,
    isASCII: Bool?,
    isNULTerminated: Bool
  ) {
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .init(x)
    }
    else if let x = String._XContent._Inline<UInt16>(source) {
      _content = .init(x)
    }
    else if let x = String._XContent._Unowned<UInt16>(
      source, isASCII: isASCII,
      isNULTerminated: isNULTerminated
    ) {
      _content = .init(x)
    }
    else if isASCII == true || !source.contains { $0 > 0xFF } {
      _content = .init(
        unsafeDowncast(
          _mkLatin1(
            _MapCollection(source, through: _TruncExt()),
            isASCII: true),
          to: String._Latin1Storage.self))
    }
    else {
      _content = .init(//.init(c)            
        unsafeDowncast(
          _mkUTF16(source), to: String._UTF16Storage.self))
    }
  }
  
  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  var count: Int {
    @inline(__always)
    get {
      if let x = _content._inline8 { return x.count }
      if let x = _content._inline16 { return x.count }
      if let x = _content._unowned8  { return Int(x._count) }
      if let x = _content._unowned16  { return Int(x._count) }
      if let x = _content._latin1 { return x.count }
      if let x = _content._utf16 { return x.count }
      if let x = _content._nsstring { return x.length() }
      _sanityCheckFailure("unreachable")
    }
  }
  
  subscript(i: Int) -> UInt16 {
    @inline(__always)
    get {
      if var x = _content._inline8 {
        return x.withUnsafeMutableBufferPointer { UInt16($0[i]) }
      }
      if var x = _content._inline16 {
        return x.withUnsafeMutableBufferPointer { $0[i] }
      }
      if let x = _content._unowned8  { return UInt16(x.unsafeBuffer[i]) }
      if let x = _content._unowned16  { return x.unsafeBuffer[i] }
      if let x = _content._latin1 { return UInt16(x[i]) }
      if let x = _content._utf16 { return x[i] }
      if let x = _content._nsstring { return x.characterAtIndex(i) }
      _sanityCheckFailure("unreachable")
    }
  }
  
  func index(after i: Int) -> Int { return i + 1 }
  func index(before i: Int) -> Int { return i - 1 }
}

extension String._XContent.UTF16View : RangeReplaceableCollection {
  public var capacity: Int {
    @inline(__always)
    get {
      if let x = _content._inline8 { return x.capacity }
      if let x = _content._inline16 { return x.capacity }
      if let x = _content._unowned8  { return Int(x._count) }
      if let x = _content._unowned16  { return Int(x._count) }
      if let x = _content._latin1 { return x.capacity }
      if let x = _content._utf16 { return x.capacity }
      if let x = _content._nsstring { return x.length() }
      _sanityCheckFailure("unreachable")
    }
  }
  
  public init() {
    _content = String._XContent()
  }

  internal var _dynamicStorageIsMutable: Bool? {
    mutating get {
      return _content._layout == .utf16 || _content._layout == .latin1 ?
      isKnownUniquelyReferenced(&_content._ownerX) : nil
    }
  }

  /// Reserve space for appending `s`, gathering as much of the appropriate space
  /// as possible without consuming `s`.
  ///
  /// - Returns: `true` if `self` is known to have mutable capacity.
  @inline(__always)
  mutating func _reserveCapacity<S: Sequence>(forAppending s: S) -> Bool
  where S.Element == UInt16 {
    let growth = s.underestimatedCount
    guard growth > 0 else { return false }

    let minCapacity = count + growth

    var forceUTF16 = false

    // We have enough capacity and can write our storage
    if capacity >= minCapacity && _dynamicStorageIsMutable != false {
      // If our storage is already wide enough, we're done
      if _content._utf16 != nil { return true }
      if _content._inline16 != nil { return true }
      if (s._preprocessingPass { s.contains { $0 > 0xFF } } != true) {
        return true
      }
      // Otherwise, widen when reserving
      forceUTF16 = true
    }
    
    _allocateCapacity(
      Swift.max(minCapacity, 2 * count), forcingUTF16: forceUTF16)
    return true
  }

  mutating func _allocateCapacity(_ minCapacity: Int, forcingUTF16: Bool) {
    var scratch = String._XContent._scratch()
    defer {
      _fixLifetime(self)
      _fixLifetime(scratch)
    }
    
    if let codeUnits = _content._existingUTF16(in: &scratch) {
      self._content = .init(
        String._UTF16Storage.copying(codeUnits, minCapacity: minCapacity))
    }
    else if let codeUnits = _content._existingLatin1(in: &scratch) {
      if !forcingUTF16 {
        self._content = .init(
          String._Latin1Storage.copying(
            codeUnits, minCapacity: minCapacity, isASCII: _content.isASCII))
      }
      else {
        self._content = .init(
          String._UTF16Storage.copying(
            _MapCollection(codeUnits, through: _TruncExt()),
            minCapacity: minCapacity,
            maxElement: _content.isASCII == true ? 0x7F
            : _content.isASCII == false ? 0xFF : nil)
        )
      }
    }
    else {
      self._content = .init(
        String._UTF16Storage.copying(self, minCapacity: minCapacity))
    }
  }
  
  mutating func reserveCapacity(_ minCapacity: Int) {
    if capacity < minCapacity || _dynamicStorageIsMutable == false {
      _allocateCapacity(minCapacity, forcingUTF16: false)
    }
  }

  mutating func append<S: Sequence>(contentsOf source_: S)
  where S.Element == Element {
    let knownMutable = _reserveCapacity(forAppending: source_)
    
    var source = IteratorSequence(source_.makeIterator())
    defer { _fixLifetime(self) }

    if _slowPath(!knownMutable && _dynamicStorageIsMutable == false) {
      // skip the fast path in this case
    }
    else if var x = _content._inline8 {
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          let u = source.next()
          guard _fastPath(u != nil && u! <= 0xFF) else {
            let newContent = String._XContent._Inline<UInt8>(buf[..<i])!
            _content = .init(newContent)
            if u != nil { self.append(u!) }
            break
          }
          buf[i] = UInt8(extendingOrTruncating: u!)
        }
      }
    }
    else if let x = _content._latin1 {
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          guard let u = source.next() else { break }
          guard _fastPath(u <= 0xFF) else {
            self.append(u)
            break
          }
          buf[i] = UInt8(extendingOrTruncating: u)
          x.count += 1
        }
      }
    }
    else if var x = _content._inline16 {
      x._withMutableCapacity { buf in
        for i in count..<buf.count {
          let u = source.next()
          guard _fastPath(u != nil) else {
            _content = .init(String._XContent._Inline<UInt16>(buf[..<i])!)
            break
          }
          buf[i] = u!
        }
      }
    }
    else if let x = _content._utf16 {
      x._withMutableCapacity { buf in
        let availableCapacity = UnsafeMutableBufferPointer(
          start: buf.baseAddress._unsafelyUnwrappedUnchecked + x.count,
          count: buf.count - x.count)
        let (newSource, copiedCount) = source._copyContents(
          initializing: availableCapacity
        )
        x.count += copiedCount
        source = newSource
      }
    }
    for u in source { append(u) }
  }

  mutating func append(_ u: UInt16) {
    let knownUnique = _reserveCapacity(forAppending: CollectionOfOne(u))
    
    defer { _fixLifetime(self) }
    
    // In-place mutation
    if _fastPath(knownUnique || _dynamicStorageIsMutable != false) {
      if u <= 0xFF, var x = _content._inline8 {
        x.append(UInt8(u))
        self._content = .init(x)
        return
      }
      else if var x = _content._inline16 {
        x.append(u)
        self._content = .init(x)
        return
      }
      else if u <= 0xFF, let x = _content._latin1 {
        x.append(UInt8(u))
        return
      }
      else if let x = _content._utf16  {
        x.append(u)
        return
      }
    }
    _replaceSubrangeSlow(
      endIndex..<endIndex, with: CollectionOfOne(u), maxNewElement: u)
  }

  mutating func replaceSubrange<C : Collection>(
    _ target: Range<Index>,
    with newElements_: C
  ) where C.Element == Element {
    defer { _fixLifetime(self) }

    let newElements = _Counted(newElements_)
    var maxNewElement: UInt16? = nil
    
    // In-place dynamic buffer
    if _dynamicStorageIsMutable == true {
      if let x = _content._latin1 {
        maxNewElement = newElements.max() ?? 0
        if maxNewElement! <= 0xFF && x._tryToReplaceSubrange(
          target,
          with: _MapCollection(newElements, through: _TruncExt())
        ) {
          return
        }
      }
      else if let x = _content._utf16 {
        if x._tryToReplaceSubrange(target, with: newElements) {
          return
        }
      }
    }
    _replaceSubrangeSlow(
      target, with: newElements, maxNewElement: maxNewElement)
  }

  mutating func _replaceSubrangeSlow<C : Collection>(
    _ target: Range<Index>,
    with newElements: C,
      maxNewElement: UInt16?
  ) where C.Element == Element {
    let minCapacity
      = target.upperBound == count && !newElements.isEmpty ? count * 2 : count

    var scratch = String._XContent._scratch()
    defer {
      _fixLifetime(self)
      _fixLifetime(scratch)
    }
    
    if let codeUnits = _content._existingLatin1(in: &scratch),
    (
      maxNewElement.map { $0 <= 0xFF }
      ?? !newElements.contains { $0 > 0xFF }
    ) {
      self = .init(
        _Concat3(
          codeUnits[..<target.lowerBound],
          _MapCollection(newElements, through: _TruncExt()),
          codeUnits[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
    else if let codeUnits = _content._existingUTF16(in: &scratch) {
      self = .init(
        _Concat3(
          codeUnits[..<target.lowerBound],
          newElements,
          codeUnits[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
    else {
      self = .init(
        _Concat3(
          self[..<target.lowerBound],
          newElements,
          self[target.upperBound...]),
        minCapacity: minCapacity
      )
    }
  }
}

extension String._XContent.UTF16View {
  init(legacy source: _StringCore) {
    var isASCII: Bool? = nil
    
    defer { _fixLifetime(source) }
    if let x = String._XContent._Inline<UInt8>(source) {
      _content = .init(x)
      return
    }
    else if let x = String._XContent._Inline<UInt16>(source) {
      _content = .init(x)
      return
    }
    else if source._owner == nil {
      if let a = source.asciiBuffer {
        let base = a.baseAddress
        if let me = String._XContent._Unowned<UInt8>(
          UnsafeBufferPointer<UInt8>(
            start: base, count: source.count),
          isASCII: true,
          isNULTerminated: true
        ) {
          _content = .init(me)
          return
        }
      }
      else {
        isASCII = source.contains { $0 > 0x7f }
        if let me = String._XContent._Unowned<UInt16>(
          UnsafeBufferPointer(
            start: source.startUTF16, count: source.count),
        isASCII: isASCII,
        isNULTerminated: true
        ) {
          _content = .init(me)
          return
        }
      }
    }
    
    if isASCII == true || !source.contains { $0 > 0xff } {
      self = String._XContent.UTF16View(
        _MapCollection(source, through: _TruncExt()),
        isASCII: isASCII ?? false
      )
    }
    else {
      self = String._XContent.UTF16View(source)
    }
  }
}

let testers: [String] = [
  "foo", "foobar", "foobarbaz", "foobarbazniz", "foobarbaznizman", "the quick brown fox",
  "f\u{f6}o", "f\u{f6}obar", "f\u{f6}obarbaz", "f\u{f6}obarbazniz", "f\u{f6}obarbaznizman", "the quick br\u{f6}wn fox",
  "ƒoo", "ƒoobar", "ƒoobarba", "ƒoobarbazniz", "ƒoobarbaznizman", "the quick brown ƒox"
]

import Dispatch
import Darwin

func time<T>(_ _caller : String = #function, _ block: () -> T) -> T {
  let start = DispatchTime.now()
  let res = block()
  let end = DispatchTime.now()
  let milliseconds = (Double(end.uptimeNanoseconds) - Double(start.uptimeNanoseconds)) / 1_000_000.0
  print("\(_caller),\(milliseconds)")        
  return res
}


func testme2() {
  let cores
  = testers.map { $0._core } + testers.map { ($0 + "X")._core }

  let arrays = cores.map(Array.init)
  
  let contents = cores.map {
    String._XContent.UTF16View(legacy: $0)
  }

  var N = 10000
  _sanityCheck({ N = 1; return true }()) // Reset N for debug builds
  
  for (x, y) in zip(cores, contents) {
    if !x.elementsEqual(y) {
      debugPrint(String(x), Array(x))
      dump(y)
      print(Array(y))
      fatalError("unequal")
    }
    _sanityCheck(
      {
        debugPrint(String(x))
        dump(y)
        print()
        return true
      }())
  }

  var total = 0
  @inline(never)
  func lex_new() {
    time {
      for _ in 0...N {
        for a in contents {
          for b in contents {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }

  @inline(never)
  func lex_old() {
    time {
      for _ in 0...N {
        for a in cores {
          for b in cores {
            if a.lexicographicallyPrecedes(b) { total = total &+ 1 }
          }
        }
      }
    }
  }
  lex_old()
  lex_new()
  print()
  
  @inline(never)
  func init_new() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ String._XContent.UTF16View(a).count
        }
      }
    }
  }
  
  @inline(never)
  func init_old() {
    time {
      for _ in 0...10*N {
        for a in arrays {
          total = total &+ _StringCore(a).count
        }
      }
    }
  }
  init_old()
  init_new()
  print()
  
  @inline(never)
  func concat3Iteration() {
    time {
      for _ in 0...100*N {
        for x in _Concat3(5..<90, 6...70, (4...30).dropFirst()) {
          total = total &+ x
        }
      }
    }
  }
  concat3Iteration()
  print()
  
  let a_old = "a"._core
  let a_new = String._XContent.UTF16View(a_old)
  
  let short8_old = ["b","c","d","pizza"].map { $0._core }
  let short8_new = short8_old.map { String._XContent.UTF16View($0) }
  
  @inline(never)
  func  buildString_old() {
    time {
      var sb = a_old
      for _ in 0...N*200 {
        for x in short8_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildString_old()
  
  @inline(never)
  func  buildString_new() {
    time {
      var sb = a_new
      for _ in 0...N*200 {
        for x in short8_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildString_new()
  print()
  
  let short16_old = ["🎉","c","d","pizza"].map { $0._core }
  let short16_new = short16_old.map { String._XContent.UTF16View($0) }

  @inline(never)
  func  buildStringUTF16_old() {
    time {
      var sb = a_old
      for _ in 0...N*300 {
        for x in short16_old {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildStringUTF16_old()
  
  @inline(never)
  func  buildStringUTF16_new() {
    time {
      var sb = a_new
      for _ in 0...N*300 {
        for x in short16_new {
          sb.append(contentsOf: x)
        }
      }
      total = total &+ sb.count
    }
  }
  buildStringUTF16_new()
  print()
  
  let ghost_old = "👻"._core
  let ghost_new = String._XContent.UTF16View(ghost_old)
  
  let long_old = "Swift is a multi-paradigm, compiled programming language created for iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is designed to work with Apple's Cocoa and Cocoa Touch frameworks and the large body of existing Objective-C code written for Apple products. Swift is intended to be more resilient to erroneous code (\"safer\") than Objective-C and also more concise. It is built with the LLVM compiler framework included in Xcode 6 and later and uses the Objective-C runtime, which allows C, Objective-C, C++ and Swift code to run within a single program."._core
  let long_new = String._XContent.UTF16View(long_old)
  
  @inline(never)
  func  buildStringLong_old() {
    time {
      var sb = ghost_old
      for _ in 0...N*20 {
        sb.append(contentsOf: long_old)
      }
      total = total &+ sb.count
    }
  }
  buildStringLong_old()
  
  @inline(never)
  func  buildStringLong_new() {
    time {
      var sb = ghost_new
      for _ in 0...N*20 {
        sb.append(contentsOf: long_new)
      }
      total = total &+ sb.count
    }
  }
  buildStringLong_new()
  print()
  
  @inline(never)
  func  buildShortString_old() {
    time {
      for _ in 0...N*200 {
        var sb = a_old
        for x in short8_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortString_old()
  
  @inline(never)
  func  buildShortString_new() {
    time {
      for _ in 0...N*200 {
        var sb = a_new
        for x in short8_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortString_new()
  print()
  
  @inline(never)
  func  buildShortStringUTF16_old() {
    time {
      for _ in 0...N*300 {
        var sb = a_old
        for x in short16_old {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortStringUTF16_old()
  
  @inline(never)
  func  buildShortStringUTF16_new() {
    time {
      for _ in 0...N*300 {
        var sb = a_new
        for x in short16_new {
          sb.append(contentsOf: x)
        }
        total = total &+ sb.count
      }
    }
  }
  buildShortStringUTF16_new()
  print()
  
  @inline(never)
  func  buildShortStringLong_old() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_old
        sb.append(contentsOf: long_old)
        total = total &+ sb.count
      }
    }
  }
  buildShortStringLong_old()
  
  @inline(never)
  func  buildShortStringLong_new() {
    time {
      for _ in 0...N*20 {
        var sb = ghost_new
        sb.append(contentsOf: long_new)
      }
    }
  }
  buildShortStringLong_new()
  print()
  
  if total == 0 { print() }
}

let cat = _Concat3(5..<10, 15...20, (25...30).dropFirst())
print(Array(cat))
print(cat.indices.map { cat[$0] })
print(MemoryLayout<String._XContent>.size)
//assert(MemoryLayout<String._XContent>.size <= 16)
testme2()


/*
let samples = (0...1000000000).map {
  _ in UInt8(extendingOrTruncating: arc4random())
}

@inline(never)
func mix(_ x: [UInt8]) -> UInt8 {
  return x.max() ?? 0
}

@inline(never)
func mask(_ x: [UInt8]) -> UInt8 {
  return x.reduce(0) { $0 | $1 }
}


_ = time { max(samples) }
_ = time { mask(samples) }
*/

