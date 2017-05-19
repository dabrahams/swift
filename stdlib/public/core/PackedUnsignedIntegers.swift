//===--- PackedUnsignedIntegers.swift -------------------------------------===//
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

/// Bit packing
///
/// Packing a sequence of source integer values with a given limited bit width
/// into a single representation integer.
///
/// * Lower-valued elements are assumed to be more common than higher-valued
///   elements.  We want to optimize the representation for storing smaller
///   numbers.
///
/// * We divide the representation up into segments of `bitsPerElement` bits
///   each, starting with the low bit, storing one source value per segment.
///
/// * In the general case, we have a partial segment in the high order bits.  
///
/// * We don't want to spend storage on representing a length.  A contiguous run
///   of zero segments starting with the high order (partial) segment is
///   considered to be empty space, containing no elements.  That means we can't
///   represent any sequence whose last element would be represented as zero; it
///   would be indistinguishable from a missing element.
///
/// * For each source element `e`, the stored representation is `e &+ 1`, masked
///   to `bitsPerElement` bits.  The value `1 << bitsPerElement - 1` is stored as
///   `0`.
///
/// * Therefore, the following are unrepresentable:
///   - elements >= `1 << bitsPerElement`
///   - a sequence ending with `1 << bitsPerElement - 1`, the least common
///     element
///
/// * Any partial segment is very likely to be able to represent the most common
///   values.
internal struct _PackedUnsignedIntegers<
  Representation : UnsignedInteger, Element : UnsignedInteger
> {
  public init?<C: Collection>(
    _ c: C,
    bitsPerElement: Representation,
  )
  where C.Iterator.Element : UnsignedInteger
  {
    guard let representation = Representation(
      packing: c, bitsPerElement: bitsPerElement) else { return nil }
    self.representation = representation
    self.bitsPerElement = bitsPerElement
  }

  public init(representation: Representation, bitsPerElement: Representation) {
    self.representation = representation
    self.bitsPerElement = bitsPerElement
  }
  
  public let bitsPerElement: Representation
  public let representation: Representation
}

extension _PackedUnsignedIntegers : Sequence {
  public struct Iterator : IteratorProtocol {
    public var bits: Representation
    public let shift: Representation

    @inline(__always)
    public mutating func next() -> Element? {
      if (bits == 0) {
        return nil
      }

      // Zero means empty, so we bias every element by 1
      let bias: Representation = 1
      let mask = (1 << shift) &- 1

      let result = mask & (bits &- bias)
      bits = bits >> shift
      return numericCast(result)
    }

    public init(bits: Representation, shift: Representation) {
      self.bits = bits
      self.shift = shift
    }
  }

  @inline(__always)
  public func makeIterator() -> Iterator {
    return Iterator(
      bits: numericCast(representation),
      shift: numericCast(bitsPerElement)
    )
  }
}

extension _PackedUnsignedIntegers : RandomAccessCollection {
  public var startIndex: Int { return 0 }
  public var endIndex: Int { return count }
  
  public var count: Int {
    let significantBits = UInt(
      Representation.bitWidth &- representation.leadingZeroBitCount)
    
    let n = significantBits &+ numericCast(bitsPerElement) &- 1
    
    // NOTE: this will be division by a small constant factor, which is
    // implemented with multiplication and shifts
    return Int(n / bitsPerElement)
  }
  
  public subscript(i: Int) -> Element {
    let bitsPerElement_ = Representation(bitsPerElement)
    let mask = ~(~0 << bitsPerElement_)
    let representation_ = representation.toUIntMax()
    let i_ = UInt(extendingOrTruncating: i).toUIntMax()
    let r = (representation_ >> (bitsPerElement_ &* i_) &- 1) & mask
    return numericCast(r)
  }
}
