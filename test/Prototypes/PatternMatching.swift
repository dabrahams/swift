//===--- PatternMatching.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

//===--- Niceties ---------------------------------------------------------===//
typealias Element_<S: Sequence> = S.Iterator.Element
extension Collection {
  func index(_ d: IndexDistance) -> Index {
    return index(startIndex, offsetBy: d)
  }
  func offset(of i: Index) -> IndexDistance {
    return distance(from: startIndex, to: i)
  }
}
class AnyEquatableBase {
  func isEqual(to other: AnyEquatableBase) -> Bool {
    fatalError("overrideMe")
  }
}

class AnyEquatableBox<T: Equatable> : AnyEquatableBase {
  let value: T

  init(_ value: T) { self.value = value }
  
  override func isEqual(to other: AnyEquatableBase) -> Bool {
    if let otherSelf = other as? AnyEquatableBox<T> {
      return self.value == otherSelf.value
    }
    return false
  }
}

struct AnyEquatable : Equatable {
  let box: AnyEquatableBase
  init<T: Equatable>(_ x: T) {
    box = AnyEquatableBox(x)
  }
  static func == (l: AnyEquatable, r: AnyEquatable) -> Bool {
    return l.box.isEqual(to: r.box)
  }
}

class AnyComparableBase : AnyEquatableBase {
  func isLess(than other: AnyComparableBase) -> Bool {
    fatalError("overrideMe")
  }
}

class AnyComparableBox<T: Comparable> : AnyComparableBase {
  let value: T

  init(_ value: T) { self.value = value }
  
  override func isEqual(to other: AnyEquatableBase) -> Bool {
    if let otherSelf = other as? AnyComparableBox<T> {
      return self.value == otherSelf.value
    }
    return false
  }
  override func isLess(than other: AnyComparableBase) -> Bool {
    guard let otherSelf = other as? AnyComparableBox<T> else {
      fatalError("mixed type ordering comparison not supported")
    }
    return self.value < otherSelf.value
  }
}

struct AnyComparable : Comparable {
  let box: AnyComparableBase
  init<T: Comparable>(_ x: T) {
    box = AnyComparableBox(x)
  }
  static func == (l: AnyComparable, r: AnyComparable) -> Bool {
    return l.box.isEqual(to: r.box)
  }
  static func < (l: AnyComparable, r: AnyComparable) -> Bool {
    return l.box.isLess(than: r.box)
  }
}
//===--- Niceties ---------------------------------------------------------===//

enum MatchResult<Index: Comparable, MatchData> {
case found(end: Index, data: MatchData)
case notFound(resumeAt: Index?)
}

protocol Pattern {
  associatedtype Element : Equatable
  associatedtype MatchData = ()
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, MatchData>
  where Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
}

extension Pattern {
  func found<C: Collection>(in c: C, storingCapturesIn captures: inout [Range<C.Index>]) -> (extent: Range<C.Index>, data: MatchData)?
  where Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var i = c.startIndex
    let originalCaptureCount = captures.count
    while i != c.endIndex {
      let m = self.matched(atStartOf: c[i..<c.endIndex], storingCapturesIn: &captures)
      switch m {
      case .found(let end, let data):
        return (extent: i..<end, data: data)
      case .notFound(let j):
        i = j ?? c.index(after: i)
        captures.removeLast(captures.count - originalCaptureCount)
      }
    }
    return nil
  }
}

// FIXME: Using this matcher for found(in:) has worst-case performance
// O(pattern.count * c.count).
//
// Also implement one or more of
// KMP/Boyer-Moore[-Galil]/Sustik-Moore/Z-algorithm which run in O(pattern.count
// + c.count)
struct LiteralMatch<T: Collection, Index: Comparable> : Pattern
where Element_<T> : Equatable {
  typealias Element = Element_<T>
  init(_ pattern: T) { self.pattern = pattern }
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn: inout [Range<C.Index>])
    -> MatchResult<C.Index, ()>
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var i = c.startIndex
    for p in pattern {
      if i == c.endIndex || c[i] != p {
        return .notFound(resumeAt: nil)
      }
      i = c.index(after: i)
    }
    return .found(end: i, data: ())
  }

  fileprivate let pattern: T
}

extension LiteralMatch where Element == Character {
  var caseInsensitive : () {  return () }
  var diacriticInsensitive : () { return ()  }
  var localized : () { return ()  }
}

struct MatchAnyOne<T : Equatable> : Pattern {
  typealias Element = T
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, ()>
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    return c.isEmpty
    ? .notFound(resumeAt: c.endIndex)
    : .found(end: c.index(after: c.startIndex), data: ())
  }
}

extension MatchAnyOne : CustomStringConvertible {
  var description: String { return "." }
}

enum MatchAny {}
var __ : MatchAny.Type { return MatchAny.self }
prefix func % <
  T : Equatable
>(_: MatchAny.Type) -> MatchAnyOne<T> {
  return MatchAnyOne()
}

/// A matcher for two other matchers in sequence.
struct ConsecutiveMatches<M0: Pattern, M1: Pattern> : Pattern
where M0.Element == M1.Element {
  init(_ m0: M0, _ m1: M1) { self.matchers = (m0, m1) }
  fileprivate let matchers: (M0, M1)
  
  typealias Element = M0.Element
  typealias MatchData = (midPoint: Any, data: (M0.MatchData, M1.MatchData))

  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, MatchData>
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var src0 = c[c.startIndex..<c.endIndex]
    let originalCaptureCount = captures.count
    while true {
      switch matchers.0.matched(atStartOf: src0, storingCapturesIn: &captures) {
      case .found(let end0, let data0):
        switch matchers.1.matched(
          atStartOf: c[end0..<c.endIndex], storingCapturesIn: &captures) {
        case .found(let end1, let data1):
          return .found(end: end1, data: (midPoint: end0, data: (data0, data1)))
        case .notFound(_):
          if src0.isEmpty {
            // I don't think we can know anything interesting about where to
            // begin searching again, because there's no communication between
            // the two matchers that would allow it.
            return .notFound(resumeAt: nil)
          }
          // backtrack
          src0 = src0.dropLast()
          captures.removeLast(captures.count - originalCaptureCount)
        }
      case .notFound(let j):
        return .notFound(resumeAt: j)
      }
    }
  }
}

extension ConsecutiveMatches : CustomStringConvertible {
  var description: String { return "(\(matchers.0))(\(matchers.1))" }
}

struct RepeatMatch<M0: Pattern> : Pattern {
  typealias Element = M0.Element
  typealias MatchData = [(end: Any, data: M0.MatchData)]
  
  let singlePattern: M0
  var repeatLimits: ClosedRange<Int>
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, MatchData>
  where Element_<C> == M0.Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == M0.Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    var lastEnd = c.startIndex
    var rest = c.dropFirst(0)
    var data: MatchData = []

  searchLoop:
    while !rest.isEmpty {
      switch singlePattern.matched(atStartOf: rest, storingCapturesIn: &captures) {
      case .found(let x):
        data.append((end: x.end, data: x.data))
        lastEnd = x.end
        if data.count == repeatLimits.upperBound { break }
        rest = rest[x.end..<rest.endIndex]
      case .notFound(let r):
        if !repeatLimits.contains(data.count)  {
          return .notFound(resumeAt: r)
        }
        break searchLoop
      }
    }
    return .found(end: lastEnd, data: data)
  }
}

extension RepeatMatch : CustomStringConvertible {
  var description: String {
    let suffix: String
    switch (repeatLimits.lowerBound, repeatLimits.upperBound) {
    case (0, Int.max):
      suffix = "*"
    case (1, Int.max):
      suffix = "+"
    case (let l, Int.max):
      suffix = "{\(l)...}"
    default:
      suffix = "\(repeatLimits)"
    }
    return "(\(singlePattern))\(suffix)"
  }
}

enum OneOf<A, B> {
  case a(A)
  case b(B)
}

extension OneOf : CustomStringConvertible {
  var description: String {
    switch self {
    case .a(let x):
      return "\(x)"
    case .b(let x):
      return "\(x)"
    }
  }
}

struct MatchOneOf<M0: Pattern, M1: Pattern> : Pattern
where M0.Element == M1.Element {
  init(_ m0: M0, _ m1: M1) { self.matchers = (m0, m1) }
  fileprivate let matchers: (M0, M1)
  
  typealias Element = M0.Element
  typealias MatchData = OneOf<M0.MatchData,M1.MatchData>

  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, MatchData>
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence
  {
    switch matchers.0.matched(atStartOf: c, storingCapturesIn: &captures) {
    case .found(let end, let data):
      return .found(end: end, data: .a(data))
    case .notFound(let r0):
      switch matchers.1.matched(atStartOf: c, storingCapturesIn: &captures) {
      case .found(let end, let data):
        return .found(end: end, data: .b(data))
      case .notFound(let r1):
        if let s0 = r0, let s1 = r1 {
          return .notFound(resumeAt: min(s0, s1))
        }
        return .notFound(resumeAt: nil)
      }
    }
  }
}

extension MatchOneOf : CustomStringConvertible {
  var description: String { return "\(matchers.0)|\(matchers.1)" }
}


infix operator .. : AdditionPrecedence
postfix operator *
postfix operator +

func .. <M0: Pattern, M1: Pattern>(m0: M0, m1: M1) -> ConsecutiveMatches<M0,M1>
where M0.Element == M1.Element {
  return ConsecutiveMatches(m0, m1)
}

postfix func * <M: Pattern>(m: M) -> RepeatMatch<M> {
  return RepeatMatch(singlePattern: m, repeatLimits: 0...Int.max)
}

postfix func + <M: Pattern>(m: M) -> RepeatMatch<M> {
  return RepeatMatch(singlePattern: m, repeatLimits: 1...Int.max)
}

func | <M0: Pattern, M1: Pattern>(m0: M0, m1: M1) -> MatchOneOf<M0,M1>
where M0.Element == M1.Element {
  return MatchOneOf(m0, m1)
}

extension String : Collection {}

//===--- Just for testing -------------------------------------------------===//
struct MatchStaticString : Pattern {
  typealias Element = UTF8.CodeUnit
  typealias Buffer = UnsafeBufferPointer<Element>
  typealias Index = Buffer.Index

  let content: StaticString
  init(_ x: StaticString) { content = x }
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, ()>
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence {
    return content.withUTF8Buffer {
      LiteralMatch<Buffer, Index>($0).matched(atStartOf: c, storingCapturesIn: &captures)
    }
  }
}
extension MatchStaticString : CustomStringConvertible {
  var description: String { return String(describing: content) }
}

// A way to force string literals to be interpreted as StaticString
prefix operator %
extension StaticString {  
  static prefix func %(x: StaticString) -> MatchStaticString {
    return MatchStaticString(x)
  }
}

extension Collection where Iterator.Element == UTF8.CodeUnit {
  var u8str : String {
    var a = Array<UTF8.CodeUnit>()
    a.reserveCapacity(numericCast(count) + 1)
    a.append(contentsOf: self)
    a.append(0)
    return String(reflecting: String(cString: a))
  }
}

extension Pattern where Element == UTF8.CodeUnit {
  func searchTest<C: Collection>(
    in c: C,
    format: (MatchData)->String = { String(reflecting: $0) })
  where  Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence {
    print("searching for /\(self)/ in \(c.u8str)...", terminator: "")
    var captures: [Range<C.Index>] = []
    
    if let (extent, data) = self.found(in: c, storingCapturesIn: &captures) {
      print(
        "\nfound at",
        "\(c.offset(of: extent.lowerBound)..<c.offset(of: extent.upperBound)):",
        c[extent].u8str,
        MatchData.self == Void.self ? "" : "\ndata: \(format(data))")
      print(captures.map { c[$0] })
    }
    else {
      print("NOT FOUND")
    }
    print()
  }
}

//===--- Just for testing -------------------------------------------------===//

//===--- Tests ------------------------------------------------------------===//
let source = Array("the quick brown fox jumps over the lazy dog".utf8)

let source2 = Array("hack hack cough cough cough spork".utf8)

(%"fox").searchTest(in: source)
(%"fog").searchTest(in: source)
(%"fox" .. %" box").searchTest(in: source)
(%"fox" .. %" jump").searchTest(in: source)
(%"cough")*.searchTest(in: source2)
(%"sneeze")+.searchTest(in: source2)
(%"hack ")*.searchTest(in: source2)
(%"cough ")+.searchTest(in: source2)

// The final * steps around <rdar://29229409>
let fancyPattern
  = %"quick "..((%"brown" | %"black" | %"fox" | %"chicken") .. %" ")+ 
  .. (%__)* .. %"do"

fancyPattern.searchTest(in: source)

//===--- Parsing pairs ----------------------------------------------------===//
// The beginnings of what it will take to wrap and indent m.data in the end of
// the last test, to make it readable.
struct PairedStructure<I: Comparable> {
  let bounds: Range<I>
  let subStructure: [PairedStructure<I>]
}


struct Paired<T: Hashable> : Pattern {
  typealias Element = T
  typealias MatchData  = PairedStructure<AnyComparable>
  
  let pairs: Dictionary<T,T>
  
  func matched<C: Collection>(atStartOf c: C, storingCapturesIn captures: inout [Range<C.Index>])
    -> MatchResult<C.Index, MatchData>
  where Element_<C> == Element
  // The following requirements go away with upcoming generics features
  , C.SubSequence : Collection, Element_<C.SubSequence> == Element
  , C.SubSequence.Index == C.Index, C.SubSequence.SubSequence == C.SubSequence {
    guard let closer = c.first.flatMap({ pairs[$0] }) else {
      return .notFound(resumeAt: nil)
    }
    var subStructure: [PairedStructure<AnyComparable>] = []
    var i = c.index(after: c.startIndex)
    var resumption: C.Index? = nil
    
    while i != c.endIndex {
      if let m = self.found(in: c[i..<c.endIndex], storingCapturesIn: &captures) {
        i = m.extent.upperBound
        subStructure.append(m.data)
        resumption = resumption ?? i
      }
      else {
        let nextI = c.index(after: i)
        if c[i] == closer {
          return .found(
            end: nextI,
            data: PairedStructure(
              bounds: AnyComparable(c.startIndex)..<AnyComparable(nextI),
              subStructure: subStructure))
        }
        i = nextI
      }
    }
    return .notFound(resumeAt: resumption)
  }
}

// Local Variables:
// swift-syntax-check-fn: swift-syntax-check-single-file
// End:

