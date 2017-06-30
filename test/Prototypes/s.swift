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

  var _mutableCapacity: UnsafeMutableBufferPointer<Element> {
    return UnsafeMutableBufferPointer(start: _baseAddress, count: capacity)
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
    var _ownerX: AnyObject = unsafeBitCast(_objCTaggedPointerBits, to: AnyObject.self)
    enum _Layout { case utf16 }
    
    var _layout: _Layout {

      get {
        return .utf16
      }
      set {
        /* nop */
      }
    }
    
    public var _utf16: _UTF16Storage {
      _sanityCheck(_layout == .utf16)
      return unsafeBitCast(
        _ownerX,
        to: _UTF16Storage.self)
    }

    init(_ x: _UTF16Storage) {
      _ownerX = x
    }
  }
}

extension String._XContent {
  public typealias _Scratch = (UInt64, UInt64)
  internal static func _scratch() -> _Scratch { return (0,0) }
}

extension String._XContent {

  init() {
    self.init(String._UTF16Storage())
    
  }
  
  func _existingLatin1(
    in scratch: inout _Scratch
  ) -> UnsafeBufferPointer<UInt8>? {

      
    return nil
  }

  func _existingUTF16(
    in scratch: inout _Scratch
  ) -> UnsafeBufferPointer<UInt16>? {
    if _layout == .utf16 { return _utf16.withUnsafeBufferPointer { $0 } }
    
    return nil
  }

  var isASCII: Bool? {
    get {
      
      if _layout == .utf16 { return _utf16.isASCII }
      return nil
    }
  }
}

extension String._XContent {
  struct UTF16View {
    var _content: String._XContent
  }
  
  var _nsString : _NSStringCore {
    if _layout == .utf16 { return _utf16 }
    
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
    internal var _start: Int = 0
    internal let _end: Int
    internal let _stride: Int
    
    internal enum _Layout {
    case deep, inline, cocoa
    }
    internal let _layout: _Layout
    internal let _inline: Builtin.Int128
      = Builtin.zext_Int8_Int128((0 as Int8)._value)
    internal let _baseAddress: UnsafePointer<UInt8>
    internal let _owner: AnyObject?

    @inline(__always)
    init(_ content: String._XContent) {
      defer { _fixLifetime(content) }
      if content._layout == .utf16 {
        let x = content._utf16
        _end = x.count &<< 1
        _stride = 2
        _layout = .deep
        _baseAddress = UnsafeRawPointer(x._baseAddress)
          .assumingMemoryBound(to: UInt8.self)
        _owner = x
      }
      else {
        _sanityCheckFailure("Unreachable")
      }
    }

    @inline(__always)
    mutating func next() -> UInt16? {
      if _start != _end {
        defer { _start = _start &+ _stride }
        
        if _layout == .deep {
          let x0 = UInt16(_baseAddress[_start])
          let oneIfWide = _stride &- 1
          let x1 = UInt16(_baseAddress[_start + oneIfWide])
#if _endian(little)
          return x1 &<< (oneIfWide &<< 3) | x0
#else
          return x0 &<< (oneIfWide &<< 3) | x1
#endif
        }
        else if _layout == .inline {
          return UInt16(
            Builtin.trunc_Int128_Int16(
              Builtin.lshr_Int128(
                _inline, Builtin.zext_Int64_Int128(_start._value))))
        }
        else {
          let s = unsafeBitCast(
            _owner._unsafelyUnwrappedUnchecked, to: _NSStringCore.self)
          return s.characterAtIndex(_start)
        }
      }
      return nil
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
    do  {
      let maxCodeUnit = maxElement ?? c.max() ?? 0
      do {
        _content = .init(
          unsafeBitCast(
            _mkUTF16(
              c,
              minCapacity: minCapacity,
              maxElement: maxCodeUnit),
            to: String._UTF16Storage.self))
      }
    }
  }

  var startIndex: Int { return 0 }
  var endIndex: Int { return count }
  var count: Int {
    @inline(__always)
    get {
      
      if _content._layout == .utf16 { return _content._utf16.count }      
      _sanityCheckFailure("unreachable")
    }
  }
  
  subscript(i: Int) -> UInt16 {
    @inline(__always)
    get {
      
      if _content._layout == .utf16 { return _content._utf16[i] }
      
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
      
      if _content._layout == .utf16 { return _content._utf16.capacity }
      _sanityCheckFailure("unreachable")
    }
  }
  
  public init() {
    _content = String._XContent()
  }

  internal var _dynamicStorageIsMutable: Bool? {
    mutating get {
      return _content._layout == .utf16 ?
      isKnownUniquelyReferenced(&_content._ownerX) : nil
    }
  }

  /// Reserve space for appending `s`, gathering as much of the appropriate space
  /// as possible without consuming `s`.
  ///
  /// - Returns: `true` if `self` is known to have mutable capacity.
  @inline(__always)
  mutating func _reserveCapacity<S: Sequence>(forAppending s: S)
  where S.Element == UInt16 {
    let growth = s.underestimatedCount

    let minCapacity = count + growth

    var forceUTF16 = false

    // We have enough capacity and can write our storage
    if _fastPath(capacity >= minCapacity && _dynamicStorageIsMutable != false) {
      // If our storage is already wide enough, we're done
      if _content._layout == .utf16 { return }
    }
    
    _allocateCapacity(
      Swift.max(minCapacity, 2 * count), forcingUTF16: forceUTF16)
    return
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
    _reserveCapacity(forAppending: source_)
    
    var source = source_.makeIterator()
    defer { _fixLifetime(self) }

     if _content._layout == .utf16 {
      let x = _content._utf16
      let buf = x._mutableCapacity
      let availableCapacity = UnsafeMutableBufferPointer(
        start: buf.baseAddress._unsafelyUnwrappedUnchecked + x.count,
        count: buf.count - x.count)
      let (newSource, copiedCount) = source_._copyContents(
        initializing: availableCapacity
      )
      x.count += copiedCount
      source = newSource
    }
    while let u = source.next() { append(u) }
  }

  mutating func append(_ u: UInt16) {
    _reserveCapacity(forAppending: CollectionOfOne(u))
    
    // In-place mutation
    if true {
       if _content._layout == .utf16 {
        _content._utf16.append(u)
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
       if _content._layout == .utf16 {
        if _content._utf16._tryToReplaceSubrange(target, with: newElements) {
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

     if let codeUnits = _content._existingUTF16(in: &scratch) {
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
    
    self = String._XContent.UTF16View(source)
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
    
  let a_old = "a"._core
  let a_new = String._XContent.UTF16View(a_old)

  
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
  
  print()
  
  if total == 0 { print() }
}

let cat = _Concat3(5..<10, 15...20, (25...30).dropFirst())
print(Array(cat))
print(cat.indices.map { cat[$0] })
print(MemoryLayout<String._XContent>.size)
//assert(MemoryLayout<String._XContent>.size <= 16)
testme2()




