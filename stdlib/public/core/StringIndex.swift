//===--- StringIndex.swift ------------------------------------------------===//
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
extension String {
  public struct Index {
    internal var _compoundOffset : UInt64
    internal var _cache: _Cache
    
    internal enum _Cache {
    case utf16
    case utf8(encodedScalar: Unicode.UTF8.EncodedScalar)
    case character(stride: UInt16)
    case unicodeScalar(value: Unicode.Scalar)
    }
  }
}

extension String.Index : Equatable {
  public static func == (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs._compoundOffset == rhs._compoundOffset
  }
}

extension String.Index : Comparable {
  public static func < (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs._compoundOffset < rhs._compoundOffset
  }
}

extension String.Index {
  internal init(_ c: Cache, atEncodedOffset o: Int) {
    self._compoundOffset = UInt64(o << _strideBits)
    self.cache = c
  }
  
  internal var _strideBits : Int { return 16 }

  internal mutating func _setEncodedOffset(_ x: Int) {
    _compoundOffset = UInt64(x << _strideBits)
  }
  
  public var encodedOffset : Int {
    return Int(_compoundOffset >> _strideBits)
  }
}
