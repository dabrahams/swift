//===--- UTFEncoding.swift - Common guts of the big 3 UnicodeEncodings ----===//
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
//
//  These components would be internal if it were possible to use internal
//  protocols to supply public conformance requirements.
//
//===----------------------------------------------------------------------===//

internal protocol _UTFEncoding {
  associatedtype CodeUnit
  associatedtype EncodedScalar = _UIntBuffer<_UInt32, CodeUnit>
    where EncodedScalar == _UIntBuffer<_UInt32, CodeUnit>
  associatedtype _UInt32 = UInt32 where _UInt32 == UInt32
  
  /// Returns true if `x` only appears in this encoding as the representation of
  /// a complete scalar value.
  static func _isScalar(_ x: CodeUnit) -> Bool
}

public protocol _UTFParser : UnicodeParser
where Encoding : UnicodeEncoding & _UTFEncoding {
  associatedtype Buffer : RangeReplaceableCollection
    where Buffer.Iterator.Element == Encoding.CodeUnit
  
  func _parseMultipleCodeUnits() -> (isValid: Bool, bitCount: UInt8)
  var _buffer: Buffer { get set }
  func _bufferedScalar(bitCount: UInt8) -> Encoding.EncodedScalar
}

extension _UTFParser where Buffer == Encoding.EncodedScalar {
  public mutating func parseScalar<I : IteratorProtocol>(
    from input: inout I
  ) -> _Unicode.ParseResult<Encoding.EncodedScalar>
    where I.Element == Encoding.CodeUnit {

    // Bufferless single-scalar fastpath.
    if _fastPath(_buffer.isEmpty) {
      guard let codeUnit = input.next() else { return .emptyInput }
      // ASCII, return immediately.
      if Encoding._isScalar(codeUnit) {
        return .valid(Encoding.EncodedScalar(containing: codeUnit))
      }
      // Non-ASCII, proceed to buffering mode.
      _buffer.append(codeUnit)
    } else if Encoding._isScalar(
      Encoding.CodeUnit(extendingOrTruncating: _buffer._storage)
    ) {
      // ASCII in _buffer.  We don't refill the buffer so we can return
      // to bufferless mode once we've exhausted it.
      let codeUnit = Encoding.CodeUnit(extendingOrTruncating: _buffer._storage)
      _buffer.remove(at: _buffer.startIndex)
      return .valid(Encoding.EncodedScalar(containing: codeUnit))
    }
    // Buffering mode.
    // Fill buffer back to 4 bytes (or as many as are left in the iterator).
    repeat {
      if let codeUnit = input.next() {
        _buffer.append(codeUnit)
      } else {
        if _buffer.isEmpty { return .emptyInput }
        break // We still have some bytes left in our buffer.
      }
    } while _buffer.count < _buffer.capacity

    // Find one unicode scalar.
    let (isValid, scalarBitCount) = _parseMultipleCodeUnits()
    _sanityCheck(scalarBitCount % numericCast(Encoding.CodeUnit.bitWidth) == 0)
    _sanityCheck(1...4 ~= scalarBitCount / 8)
    _sanityCheck(scalarBitCount <= _buffer._bitCount)
    
    // Consume the decoded bytes (or maximal subpart of ill-formed sequence).
    let encodedScalar = _bufferedScalar(bitCount: scalarBitCount)
    
    _buffer._storage = UInt32(
      // widen to 64 bits so that we can empty the buffer in the 4-byte case
      extendingOrTruncating: UInt64(_buffer._storage) &>> scalarBitCount)
      
    _buffer._bitCount = _buffer._bitCount &- scalarBitCount

    if _fastPath(isValid) {
      return .valid(encodedScalar)
    }
    return .invalid(
      length: Int(scalarBitCount / numericCast(Encoding.CodeUnit.bitWidth)))
  }
}
