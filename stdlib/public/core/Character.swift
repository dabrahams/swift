//===----------------------------------------------------------------------===//
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

/// A single extended grapheme cluster that approximates a user-perceived
/// character.
///
/// The `Character` type represents a character made up of one or more Unicode
/// scalar values, grouped by a Unicode boundary algorithm. Generally, a
/// `Character` instance matches what the reader of a string will perceive as
/// a single character. Strings are collections of `Character` instances, so
/// the number of visible characters is generally the most natural way to
/// count the length of a string.
///
///     let greeting = "Hello! 🐥"
///     print("Length: \(greeting.count)")
///     // Prints "Length: 8"
///
/// Because each character in a string can be made up of one or more Unicode
/// code points, the number of characters in a string may not match the length
/// of the Unicode code point representation or the length of the string in a
/// particular binary representation.
///
///     print("Unicode code point count: \(greeting.unicodeScalars.count)")
///     // Prints "Unicode code point count: 15"
///
///     print("UTF-8 representation count: \(greeting.utf8.count)")
///     // Prints "UTF-8 representation count: 18"
///
/// Every `Character` instance is composed of one or more Unicode code points
/// that are grouped together as an *extended grapheme cluster*. The way these
/// code points are grouped is defined by a canonical, localized, or otherwise
/// tailored Unicode segmentation algorithm.
///
/// For example, a country's Unicode flag character is made up of two regional
/// indicator code points that correspond to that country's ISO 3166-1 alpha-2
/// code. The alpha-2 code for The United States is "US", so its flag
/// character is made up of the Unicode code points `"\u{1F1FA}"` (REGIONAL
/// INDICATOR SYMBOL LETTER U) and `"\u{1F1F8}"` (REGIONAL INDICATOR SYMBOL
/// LETTER S). When placed next to each other in a Swift string literal, these
/// two code points are combined into a single grapheme cluster, represented
/// by a `Character` instance in Swift.
///
///     let usFlag: Character = "\u{1F1FA}\u{1F1F8}"
///     print(usFlag)
///     // Prints "🇺🇸"
///
/// For more information about the Unicode terms used in this discussion, see
/// the [Unicode.org glossary][glossary]. In particular, this discussion
/// mentions [extended grapheme clusters][clusters] and [Unicode scalar
/// values][scalars].
///
/// [glossary]: http://www.unicode.org/glossary/
/// [clusters]: http://www.unicode.org/glossary/#extended_grapheme_cluster
/// [scalars]: http://www.unicode.org/glossary/#unicode_scalar_value
@_fixed_layout
public struct Character :
  _ExpressibleByBuiltinExtendedGraphemeClusterLiteral,
  ExpressibleByExtendedGraphemeClusterLiteral, Hashable {

  // Fundamentally, it is just a String, but it is optimized for the
  // common case where the UTF-8 representation fits in 63 bits.  The
  // remaining bit is used to discriminate between small and large
  // representations.  In the small representation, the unused bytes
  // are filled with 0xFF.
  //
  // If the grapheme cluster can be represented as `.small`, it
  // should be represented as such.
  @_versioned
  internal enum Representation {
    case large(_StringBuffer._Storage)
    case small(Builtin.Int63)
  }

  /// Creates a character containing the given Unicode scalar value.
  ///
  /// - Parameter scalar: The Unicode scalar value to convert into a character.
  public init(_ scalar: Unicode.Scalar) {
    let bits = UTF8.encode(scalar)._unsafelyUnwrappedUnchecked._biasedBits
    _representation = .small(Builtin.zext_Int32_Int63(bits._value))
  }

  @effects(readonly)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = Character(UnicodeScalar(_unchecked: UInt32(value)))
  }

  // Inlining ensures that the whole constructor can be folded away to a single
  // integer constant in case of small character literals.
  @inline(__always)
  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    let input = UnsafeBufferPointer<UInt8>(
      start: UnsafePointer(start),
      count: Int(utf8CodeUnitCount)
    )
    
    if let smallRepresentation = Character(_smallValidUTF8: input) {
      self = smallRepresentation
      return
    }
    
    // For anything that doesn't fit in 63 bits, build the large
    // representation.
    self = Character(_largeRepresentationString:
      String(
        _builtinExtendedGraphemeClusterLiteral: start,
        utf8CodeUnitCount: utf8CodeUnitCount,
        isASCII: isASCII))
  }

  @inline(__always)
  public init?<Input: Collection>(_smallValidUTF8 input: Input)
  where Input.Element == UInt8 {
    guard _fastPath(input.count <= MemoryLayout<UInt64>.size) else {
      return nil
    }
    let bits = _ValidUTF8Buffer<UInt64>(input)._biasedBits
    guard _fastPath(Int64(extendingOrTruncating: bits) >= 0) else {
      return nil
    }
    _representation = .small(Builtin.trunc_Int64_Int63(bits._value))
  }

  /// Creates a character with the specified value.
  ///
  /// Do not call this initalizer directly. It is used by the compiler when
  /// you use a string literal to initialize a `Character` instance. For
  /// example:
  ///
  ///     let oBreve: Character = "o\u{306}"
  ///     print(oBreve)
  ///     // Prints "ŏ"
  ///
  /// The assignment to the `oBreve` constant calls this initializer behind the
  /// scenes.
  public init(extendedGraphemeClusterLiteral value: Character) {
    self = value
  }

  /// Creates a character from a single-character string.
  ///
  /// The following example creates a new character from the uppercase version
  /// of a string that only holds one character.
  ///
  ///     let a = "a"
  ///     let capitalA = Character(a.uppercased())
  ///
  /// - Parameter s: The single-character string to convert to a `Character`
  ///   instance. `s` must contain exactly one extended grapheme cluster.
  public init(_ s: String) {
    // The small representation can accept up to 8 code units as long
    // as the last one is a continuation.  Since the high bit of the
    // last byte is used for the enum's discriminator, we have to
    // reconstruct it.  As a result, we can't store 0x7f in the final
    // byte, because we wouldn't be able to distinguish it from an
    // unused 0xFF byte.  Rather than trying to squeeze in other
    // one-byte code points there, we simplify decoding by banning
    // starting a code point in the last byte, and assuming that its
    // high bit is 1.
    _precondition(
      s._core.count != 0, "Can't form a Character from an empty String")
    _precondition(
      s.index(after: s.startIndex) == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    if let smallRepresentation = Character(_smallValidUTF8: s.utf8) {
      self = smallRepresentation
      return
    }
    self = Character(_largeRepresentationString: s)
  }

  /// Creates a Character from a String that is already known to require the
  /// large representation.
  ///
  /// - Note: `s` should contain only a single grapheme, but we can't require
  ///   that formally because of grapheme cluster literals and the shifting
  ///   sands of Unicode.  https://bugs.swift.org/browse/SR-4955
  @_versioned
  internal init(_largeRepresentationString s: String) {
    if let native = s._core.nativeBuffer,
      native.start == s._core._baseAddress!,
      native.usedCount == s._core.count {
      _representation = .large(native._storage)
      return
    }
    var nativeString = ""
    nativeString.append(s)
    _representation = .large(nativeString._core.nativeBuffer!._storage)
  }

  /// Returns the index of the lowest byte that is 0xFF, or 8 if
  /// there is none.
  static func _smallSize(_ value: UInt64) -> Int {
    var mask: UInt64 = 0xFF
    for i in 0..<8 {
      if (value & mask) == mask {
        return i
      }
      mask &<<= 8
    }
    return 8
  }

  static func _smallValue(_ value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value)) | (1 &<< 63)
  }

  /// The character's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  public var hashValue: Int {
    // FIXME(performance): constructing a temporary string is extremely
    // wasteful and inefficient.
    return String(self).hashValue
  }

  typealias UTF16View = String.UTF16View
  var utf16: UTF16View {
    return String(self).utf16
  }

  @_versioned
  internal var _representation: Representation
}

extension Character : CustomStringConvertible {
  public var description: String {
    return String(describing: self)
  }
}

extension Character : LosslessStringConvertible {}

extension Character : CustomDebugStringConvertible {
  /// A textual representation of the character, suitable for debugging.
  public var debugDescription: String {
    return String(self).debugDescription
  }
}

extension String {
  /// Creates a string containing the given character.
  ///
  /// - Parameter c: The character to convert to a string.
  public init(_ c: Character) {
    switch c._representation {
    case let .small(_63bits):
      let bytes = _ValidUTF8Buffer(_biasedBits: Character._smallValue(_63bits))
      self = String._fromWellFormedCodeUnitSequence(UTF8.self, input: bytes)
    case let .large(value):
      self = String(_StringCore(_StringBuffer(value)))
    }
  }
}

/// `.small` characters are stored in an Int63 with their UTF-8 representation,
/// with any unused bytes set to 0xFF. ASCII characters will have all bytes set
/// to 0xFF except for the lowest byte, which will store the ASCII value. Since
/// 0x7FFFFFFFFFFFFF80 or greater is an invalid UTF-8 sequence, we know if a
/// value is ASCII by checking if it is greater than or equal to
/// 0x7FFFFFFFFFFFFF00.
internal var _minASCIICharReprBuiltin: Builtin.Int63 {
  @inline(__always) get {
    let x: Int64 = 0x7FFFFFFFFFFFFF00
    return Builtin.truncOrBitCast_Int64_Int63(x._value)
  }
}

extension Character : Equatable {
  public static func == (lhs: Character, rhs: Character) -> Bool {
    switch (lhs._representation, rhs._representation) {
    case let (.small(lbits), .small(rbits)) where
      Bool(Builtin.cmp_uge_Int63(lbits, _minASCIICharReprBuiltin))
      && Bool(Builtin.cmp_uge_Int63(rbits, _minASCIICharReprBuiltin)):
      return Bool(Builtin.cmp_eq_Int63(lbits, rbits))
    default:
      // FIXME(performance): constructing two temporary strings is extremely
      // wasteful and inefficient.
      return String(lhs) == String(rhs)
    }
  }
}

extension Character : Comparable {
  public static func < (lhs: Character, rhs: Character) -> Bool {
    switch (lhs._representation, rhs._representation) {
    case let (.small(lbits), .small(rbits)) where
      // Note: This is consistent with Foundation but unicode incorrect.
      // See String._compareASCII.
      Bool(Builtin.cmp_uge_Int63(lbits, _minASCIICharReprBuiltin))
      && Bool(Builtin.cmp_uge_Int63(rbits, _minASCIICharReprBuiltin)):
      return Bool(Builtin.cmp_ult_Int63(lbits, rbits))
    default:
      // FIXME(performance): constructing two temporary strings is extremely
      // wasteful and inefficient.
      return String(lhs) < String(rhs)
    }
  }
}
