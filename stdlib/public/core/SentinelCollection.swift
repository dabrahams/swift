//===--- SentinelCollection.swift -----------------------------------------===//
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

struct _SentinelCollection<
  Base: Collection, 
  IsSentinel : _Predicate
> : _CollectionWrapper where IsSentinel.Input == Base.Iterator.Element {
  typealias _Base = Base
  
  let _isSentinel: IsSentinel
  var _base : _Base

  struct Index {
    var _base: _Base.Index?
    let _isSentinel: IsSentinel
    
    static func == (lhs: Index, rhs: Index) -> Bool {
      switch (lhs._base, rhs._base) {
      case (let x, let y): return x == y
      case (nil, let x), (let x, nil): return lhs._isSentinel.apply(x)
      case (nil, nil): return true
      }
    }
    
    static func < (lhs: Index, rhs: Index) -> Bool {
      switch (lhs._base, rhs._base) {
      case (let x, let y): return x < y
      case (let x, nil): return !lhs._isSentinel.apply(x)
      case (nil, nil): return false
      }
    }
  }
  
  init(_ base: Base, until condition: IsSentinel) {
    _base = base
    _isSentinel = condition
  }
  
  func _wrap(_ i: _Base.Index) -> Index {
    return Index(_base: i, _isSentinel)
  }
  
  func _unwrap(_ i: Index) -> _Base.Index {
    return i._base ?? _base.endIndex
  }
}
