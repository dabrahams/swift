# String Processing For Swift 4

The goal of re-evaluating Strings for Swift 4 has been fairly ill-defined thus
far, with just this short blurb in the
[list of goals](https://lists.swift.org/pipermail/swift-evolution/Week-of-Mon-20160725/025676.html):

> **String re-evaluation**: String is one of the most important fundamental
> types in the language.  The standard library leads have numerous ideas of how
> to improve the programming model for it, without jeopardizing the goals of
> providing a unicode-correct-by-default model.  Our goal is to be better at
> string processing than Perl!
   
With this document, we elaborate on the goals and sugest a few possible
ways to acheive them.

For Swift 4 we want to improve three dimensions of String and Text processing:

  1. Ergonomics
  2. Correctness
  3. Performance

It's worth noting that ergonomics and correctness are mutually-reinforcing.  An
API that is easy to use—but incorrectly—cannot be considered an ergonomic
success.  Conversely, an API that's simply hard to use is also hard to use
correctly.  Acheiving optimal performance without compromising ergonomics or
correctness is a greater challenge.

## Issues With Today's String Design

### API Surface Area

What does this API do?  When should I use it?  Answers to these questions should
be plainly obvious for APIs of primary data types, but `String` fails that test.

To take a simple example, if you want to compare two strings for equality, there
are at least the following choices:

  1. `s1 == s2`
  2. `s1.compare(s2)`
  3. `s1.compare(s2, options: [], locale: nil)`
  4. `s1.compare(s2, options: [], locale: Locale(identifier: ""))`
  5. `s1.localizedStandardCompare(s2)`

This list does not exaggerate the problem.  The standard library itself
implements `==` with intended behavior approximately matching that of #4, but—due
to the complexity of the API and lack of specific documentation—ended up with
behavior matching #2 and #3.  Anyone processing human-readable text, though,
should use #5, which is why it is included.

String currently has 205 APIs. I've broken them down into Standard Library and
Foundation, simply to show that both contribute significantly to overall
complexity:

**Source** | `init` | `func` | `subscript` | `var`
---|---
Standard Library | 41 | 42 | 9 | 26
Foundation | 18 | 55 | 0 | 14

Method arity lines up as follows:

**Arity** | **Standard Library** | **Foundation**
---|---
0: `ƒ()` | 5 | 7 |
1: `ƒ(:)` | 19 | 48 |
2: `ƒ(::)` | 13 | 19 |
3: `ƒ(:::)` | 5 | 11 |
4: `ƒ(::::)` | 1 | 7 |
5: `ƒ(:::::)` | - | 2 |
6: `ƒ(::::::)` | - | 1 |

I consider this unacceptable. By contrast, `Int` in Swift 3.0 (*predating* the
new integers proposal that will eliminate much of its API surface area) has 80
APIs (none with more than two parameters).  Still, in my opinion, too many, but
much more manageable.

String processing is a complex domain, but that's all the *more
reason* we should present an API that's easy to grasp.

### Where's my API?

While `String` is available to all programs out-of-the-box, crucial APIs for
basic string processing tasks are still inaccessible until `Foundation` is
imported.  While it makes sense that `Foundation` is needed for domain-specific
jobs such as
[linguistic tagging](https://developer.apple.com/reference/foundation/nslinguistictagger),
one should not need to import anything to, for example, do case-insensitive
comparison.

### Strings Shouldn't Really Have Elements

A `String` does not act like a `Collection` of anything:

```swift
// Not a Collection of Characters
"e".characters.count               // 1
"\u{301}".characters.count         // 1 (COMBINING ACUTE ACCENT)
("e" + "\u{301}").characters.count // also 1

// Not a Collection of UnicodeScalars
"e\u{301}".unicodeScalars.count    // 2
"\u{e9}".unicodeScalars.count      // 1 (LATIN SMALL LETTER E WITH ACUTE)
"e\u{301}" == "\u{e9}"             // true
```

Although Swift 2.0 correctly changed `String` so that it does not conform to
`Collection` , `String`'s API still elevates `Character`s to special status:

```swift
var i = s.startIndex
s[i]                   // Equivalent to s.characters.first!
s.formIndex(after: &i) // Moves i past the first Character
```

Exposing fine-grained `Character` traversal at the top level of the `String` API
is conceptually wrong, and encourages users to do character-by-character string
analysis.  Because some strings that should be equal have unequal numbers of
characters,<sup id="a1">[1](#f1)</sup> processing one character at a time is almost always a bug, and the
fact that such cases are rare means the bug is unlikely to be caught in testing.

The right model for `String` is more like a roll of cookie dough with some rocks
hidden in it: a continuous thing that can be sliced into shorter things, but
only in certain places, and with no discernable unit of subdivision.

- Note: Today, `String` *may appear* to act like a `Collection` of `Character`s (but
  not a `RangeReplaceableCollection`) because of a bug (see the section
  titled
  [We Don't Implement Comparison Correctly](#we-dont-implement-comparison-correctly).

### String Lacks Swifty APIs For Scanning, Matching, and Tokenization

As though it wasn't bad enough that we expose and promote a tempting collection
of `Characters` that people will use to probe string contents, we also *fail* to
provide convenient alternative APIs that are correct for that purpose.  For
example, it should be easy to cleanly express, “if this string starts with
`"f"`, process the rest of the string as follows…”  Swift is well-suited to
expressing this common pattern beautifully; we just need to add the APIs.

### Slicing Operations are not Syntactically/Semantically Unified

Creating substrings is a basic part of String processing, but the slicing
operations that we have in Swift are all over the map: 

  * Slices with two explicit endpoints are done with subscript, and support in-place
    mutation:
    ```swift
        s[i..<j].mutate()
    ```

  * Slicing from an index to the end, or from the start to an index, is done
    with a method and does not support in-place mutation:
    ```swift
        s.prefix(upTo: i).readOnly()
    ```

### Index Translation is a Fact of Life

There are two aspects to this problem:

  1. `String` views cannot consume one anothers' indices without an explicit
    conversion step.  An index into a `String`'s `characters` must be translated
    before it can be used as a position in its `unicodeScalars`.  Although these
    translations are rarely needed, they add conceptual and API complexity.
  2. Many APIs in the core libraries and other frameworks still expose `String`
    positions as `Int`s and regions as `NSRange`s, which can only reference its
    `utf16` view and interoperate poorly with `String` itself.

### Regular Expressions Should Have Compile-Time Support

Literal regular expressions (as opposed to ones that are fully
dynamically-constructed from `String`s) should be syntax-checked at compile time
and should benefit from optimizations on that basis.  There are both language-
and library- level solutions to this problem, and while addressing it is out of
scope for Swift 4, it is important that we lay the foundations necessary to
support it.

### Printf-Style Formatting is Cryptic, Not Extensible, Not Statically Typesafe

`String.format` is designed on the `printf` model: it takes a format string with
textual placeholders for substitution, and an arbitrary list of other arguments.
The syntax and meaning of these placeholders has a long history in
C, but for anyone who doesn't use them regularly they are cryptic and complex,
as the `printf (3)` man page attests.

Aside from complexity, this style of API has two major problems: First, the
spelling of these placeholders must match up to the types of the arguments, in
the right order, or the behavior is undefined.  Some limited support for
compile-time checking of this correspondence could be implemented, but only for
the cases where the format string is a literal. Second, there's no reasonable
way to extend the formatting vocabulary to cover the needs of new types: you are
stuck with what's in the box.

### Foundation Formatters Unweildy, Verbose

The design pattern used by the core Foundation formatters demands more from
users than it should:
  * Matching the type of data being formatted to a formatter type
  * Creating an instance of that type
  * Setting stateful options (`currency`, `dateStyle`) on the type.  Note: the
    need for this step prevents the instance from being used and discarded in
    the same expression where it is created.
  * Overall, introduction of needless verbosity into source

These may seem like small issues, but the experience of Apple localization
experts is that the total drag of these factors on programmers is such that they
tend to reach for `String.format` instead.

### String Interpolation APIs are Too Limited

Swift string interpolation provides a beautiful alternative to printf's cryptic
domain-specific language (just write ordinary swift code!) and its type safety
problems (put the data right where it belongs!) but the following issues prevent
it from being useful for localized formatting (among other jobs).

  * [SR-2303](https://bugs.swift.org/browse/SR-2303) We are unable to restrict
    types used in string interpolation.
  * [SR-1260](https://bugs.swift.org/browse/SR-1260) String interpolation can't
    distinguish (fragments of) the base string from the string substitutions.

### C String Interop is Patchy

Our support for interoperation with C strings consists of the following `String` APIs

```swift
// These deal in arrays of CChar (a.k.a. Int8)
init(cString: UnsafePointer<CChar>)

init?(validatingUTF8 cString: UnsafePointer<CChar>)

func withCString<Result>(
    _ body: (UnsafePointer<Int8>) throws -> Result
  ) rethrows -> Result

var utf8CString: ContiguousArray<CChar>

// 
init(cString: UnsafePointer<UInt8>)

static func decodeCString<Encoding : UnicodeCodec>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true)
      -> (result: String, repairsMade: Bool)?
```

Because `UTF8.CodeUnit` is unsigned but `CChar`/`Int8` is signed, there is an
impedance mismatch.  We've been inconsistent about trying to resolve that
mismatch, and we need a policy.

* FIXME: I feel like there must be other problems that I'm forgetting.  Is that all?

### Correct Internationalization is Too Hard

There is strong evidence that developers cannot determine how to use our
international APIs correctly.  Most developers are not experts in localization,
and the current international APIs present an often bewildering catalogue of
choices.  Many `String` APIs that require special attention in an international
context take four or more parameters. For an audience of mostly non-experts, it
is especially important that naïve code is very likely to be correct if it
compiles, and that more sophisticated issues can be revealed progressively.

Among the most common internationalization errors is the unintentional
presentation to end-users of text that has not been localized.  Fortunately,
solving this problem is easy, and supports the goal of reduced API surface area.

### We Don't Implement Comparison Correctly

String comparison isn't implemented according to the Unicode standard.  All of
the following should evaluate as `true`, but currently do not:

**Expression** | **Notes**
---|---
`"a" < "J"`| Unicode interleaves case: a < A < b < B…
`"\u{2024}\u{2024}" == "\u{2025}"`| 2x ONE DOT LEADER, TWO DOT LEADER
`"\u{2024}\u{2025}" == "\u{2026}"` | ONE + TWO DOT LEADER, HORIZONTAL ELLIPSIS
`"\u{222c}\u{222c}\u{222c}" == "\u{222d}\u{222d}"` | 3x DOUBLE INTEGRAL, 2x TRIPLE INTEGRAL


Legend: 

  *  u2024 = ONE DOT LEADER
  *  u2025 = TWO DOT LEADER
  *  u2026 = HORIZONTAL ELLIPSIS
  *  u222c = DOUBLE INTEGRAL
  *  u222d = TRIPLE INTEGRAL

Strings in Swift are specifically intended to be—and are documented as—
Unicode-correct, so deviating from the standard in this way is not consistent
with our stated goals.

- Note: `CFString` (to which Swift's comparison implementation dispatches) and
  `NSString` behave the same as `String` in this regard. Ideally they would all
  have the same, Unicode-correct, behavior, but there may be legacy reasons that
  the non-native types must remain as they are.

### We Only Implement Unicode 8 Grapheme Breaking

Unicode 9 (and MacOS 10.11) brought us support for family emoji, which changes
the process of properly identifying `Character` boundaries from an O(1)
operation to worst-case O(N).  The change in complexity is not a great concern
because N is typically very small, but it reflects fundamental changes in how
the breaking algorithm works, and we need to update `String` to account for it.

### Substrings “Leak” Memory

Slicing a `String` produces a new `String` that keeps the entire original
alive. This arrangement has proven to be problematic in other programming
languages, because applications sometimes extract small strings from large ones
and keep those small strings long-term.

### String's Representation Doesn't Use Storage Well

For most strings in most applications, the big efficiency win comes from
efficient storage.  However,

* We fail to take advantage of the small string optimization (what Cocoa does
  with tagged pointers), which is both a storage and a performance optimization.
* `String`s whose characters fall outside ASCII but within Latin-1 waste one byte
  of storage per unicode scalar; Latin-1 strings can be stored as sequences of
  8-bit units.
* `MemoryLayout<String>.size` is 3 words when it should be at most 64 bits.  64
  bits is enough to store many strings inline and works well for out-of-line
  storage.

### No Way to Opt Out of Type Erasure

As noted above, many strings are short enough to store in 64 bits, many can be
stored using only 8 bits per unicode scalar, others are best encoded in UTF-16,
and some come to us already in some other encoding, such as UTF-8, that would be
costly to translate.  Supporting these formats while maintaining usability for
general-purpose APIs demands that a single String type can be backed by many
different representations, as it is today.

That said, the highest performance code always requires static knowledge of the
data structures on which it operates, and for this code, dynamic representation
selection comes at too high a cost.  Heavy-duty text processing demands a way to
opt out of dynamism and directly use known String encodings.  Having this
ability can also make it easy to cleanly specialize code that handles dynamic
cases for maximal efficiency on the most common representations.

### No Direct Support for Parsing ASCII Structure

Although many machine-readable formats support the inclusion of arbitrary
Unicode text, it is also common that their fundamental structure lies entirely
within the ASCII subset (JSON, YAML, many XML formats).  These formats are often
processed most efficiently by recognizing ASCII structural elements as ASCII,
and capturing the arbitrary sections between them in more-general strings.  The
current String API offers no way to efficiently recognize ASCII and skip past
everything else without the overhead of full decoding into unicode scalars .

## The Shape of a Solution

### Reducing API Surface Area and Conceptual Complexity

We can attack this problem on many fronts, often while dealing with other
issues:

  * **Drop `Character`** as a distinct data type.  There are no APIs whose
    correctness depends on operating on single grapheme clusters as opposed to
    longer strings.  A String's `.characters` can become a collection of
    substrings.  We can also drop the
    `ExpressibleByExtendedGraphemeClusterLiteral` protocol for the same reason.
  * Adopt
    [this proposal](https://github.com/apple/swift-evolution/blob/9cf2685293108ea3efcbebb7ee6a8618b83d4a90/proposals/0132-sequence-end-ops.md) and
    implement generic subscripting in the language. Removes the proliferation of
    `subscript` overloads and a bunch of `prefix` and `suffix` methods.
  * Adopt a version
    of
    [this proposal](https://github.com/pyrtsa/swift-evolution/blob/ca89e7b3a1dffc99baa695a03544fcba75afd0f3/proposals/NNNN-formalized-ordering.md) (implemented
    by [these changes](https://github.com/dabrahams/swift/pull/1)) that uses a
    `s1.compare(s2)` method (rather than a “spaceship operator” `<=>`, which has
    only a very weak advantage in that it can be defined in the library for
    tuples but is insufficient to let tuples conform to a protocol).  This will
    give us a platform to implement methods with additional, defaulted
    arguments, e.g. `s1.compare(s2, caseInsensitive: true)`, thereby unifying
    comparisons.
    
    
## Existing String API and Suggested Disposition

**Legend**:  ✅ = Keep, ❌ = Discard, ↗️ = Transplant,⛏️= Redesign, ❓= TBD
 
### C String Interop

**API** | **Suggested Disposition**
---|---
`init(cString: UnsafePointer<CChar>)` | ❓we should do something about CChar-vs-UInt8
`init(cString: UnsafePointer<UInt8>)` | ❓
`init?(validatingUTF8: UnsafePointer<CChar>)` | ❓probably OK
`static func decodeCString<Encoding : UnicodeCodec>(`<br/>`  _: UnsafePointer<Encoding.CodeUnit>?,`<br/>`  as: Encoding.Type,`<br/>`  repairingInvalidCodeUnits: Bool = default`<br/>`) -> (result: String, repairsMade: Bool)?` | ✅Would be an init except for repairsMade
`func withCString<Result>(`<br/>`  _: (UnsafePointer<Int8>) throws -> Result`<br/>`) rethrows -> Result` |❓
`var utf8CString: ContiguousArray<CChar>` | ❓Reevaluate all CString APIs
`func getCString(`<br/>`  _: inout [CChar],`<br/>`  maxLength: Int,`<br/>`  encoding: String.Encoding) -> Bool` |❓
`func cString(using: String.Encoding) -> [CChar]?` | ↗️e.cString(s)
`init?(`<br/>`  cString: UnsafePointer<CChar>,`<br/>`  encoding: String.Encoding)` |❓

### Miscellaneous

**API** | **Suggested Disposition**
:-------- | :-------
`init()` | ✅
`var customMirror: Mirror` |✅
`var customPlaygroundQuickLook: PlaygroundQuickLook` | ↗️Move CustomPlaygroundQuickLookable to PlaygroundSupport library
`init(_: String)` | ❓Should be resolved the same way as other "copy initializers" for value types
`func lowercased() -> String` |✅
`func uppercased() -> String` |✅
`init<T : LosslessStringConvertible>(_: T)` |✅
`init?(_: String)` |❓Why do we have this?

### `Character` Data Type Support

We are recommending removing this data type in favor of `String` and
`String.Slice` (a.k.a. `Substring`).

**API** | **Suggested Disposition**
:-------- | :-------
`init(_: Character)` | ❌
`mutating func append(_: Character)` | ❌
`init(extendedGraphemeClusterLiteral: String)` |❌
`typealias ExtendedGraphemeClusterLiteralType = String` |❌

### `TextOutputStreamable` and `TextOutputStream` Support

**API** | **Suggested Disposition**
---|---
`mutating func write(_: String)` | 
`func write<Target : TextOutputStream>(to: inout Target)` |❓

### Views

**API** | **Suggested Disposition**
---|---
`var utf8: UTF8View`|✅
`var utf16: UTF16View`|✅
`var unicodeScalars: UnicodeScalarView`|✅
`var characters: CharacterView` |✅
`struct UTF8View` | ✅
`struct UTF16View` |✅
`struct UnicodeScalarView` |✅
`struct CharacterView` | ✅
`typealias UTF16Index =`…  |❌Obsoleted by index unification
`typealias UTF8Index =`… |❌Obsoleted by index unification
`typealias UnicodeScalarIndex =`… |❌Obsoleted by index unification
`mutating func withMutableCharacters<R>(`<br/>`  _: (inout String.CharacterView) -> R) -> R` | ❌ Expected to be obsoleted by better `inout` support.
`init(_: String.CharacterView)` | ✅ Not strictly needed; we can assign to the `characters` of an empty string.<br/>We can also do `characters.joined(separator: "")` if the elements are `String`s.

### Comparison and Collation

**API** | **Suggested Disposition**
---|---
`static func <(lhs: String, rhs: String) -> Bool` |❓
`var hashValue: Int` |✅


### Formatting

**API** | **Suggested Disposition**
---|---
`var debugDescription: String` |❓
`var description: String` |❓

### Literal Convertibility

**API** | **Suggested Disposition**
---|---
`init(unicodeScalarLiteral: String)` |❓
`typealias UnicodeScalarLiteralType = String` |❓
`init(stringLiteral: String)` |❓
`typealias StringLiteralType = String` |❓

### Interpolation

**API** | **Suggested Disposition**
---|---
`init(stringInterpolation: String...)` | ? String interpolation needs a redesign
`init<T>(stringInterpolationSegment: T)` | ?
`init(stringInterpolationSegment: String)` | ❌The rest of these are part an optimization that relies on compiler magic, and should at *least* be hidden from users.
`init(stringInterpolationSegment: Character)` |  ❌
`init(stringInterpolationSegment: UnicodeScalar)` |  ❌
`init(stringInterpolationSegment: Bool)` |   ❌
`init(stringInterpolationSegment: Float32)` |   ❌
`init(stringInterpolationSegment: Float64)` |   ❌
`init(stringInterpolationSegment: UInt8)` |   ❌
`init(stringInterpolationSegment: Int8)` |   ❌
`init(stringInterpolationSegment: UInt16)` |   ❌
`init(stringInterpolationSegment: Int16)` |   ❌
`init(stringInterpolationSegment: UInt32)` |   ❌
`init(stringInterpolationSegment: Int32)` |   ❌
`init(stringInterpolationSegment: UInt64)` |   ❌
`init(stringInterpolationSegment: Int64)` |   ❌
`init(stringInterpolationSegment: UInt)` |   ❌
`init(stringInterpolationSegment: Int)` |   ❌

**API** | **Suggested Disposition**
---|---
`init(repeating: String, count: Int)` | ✅ Slightly useful
`typealias Index = String.CharacterView.Index` |❓
`var startIndex: String.Index` |❓
`var endIndex: String.Index` |❓

### Obsoleted Because String Is Not a Collection

**API** | **Suggested Disposition**
---|---
`typealias IndexDistance = ...` | ❌can't count elements on a roll of cookie dough
`func index(after: String.Index) -> String.Index` | ❌️The following should die
`func index(before: String.Index) -> String.Index` |❌
`func index(`<br/>`  _: String.Index,`<br/>`  offsetBy: String.IndexDistance) -> String.Index` |❌
`func index(`<br/>`  _: String.Index,`<br/>`  offsetBy: String.IndexDistance,`<br/>`  limitedBy: String.Index) -> String.Index?` |❌
`func distance(`<br/>`  from: String.Index, to: String.Index) -> String.IndexDistance` |❌
`subscript(i: String.Index) -> Character` |❌


**API** | **Suggested Disposition**
---|---
`subscript(bounds: Range<String.Index>) -> String` | ✅ Part of `Sliceable` Protocol, but generic on [`RangeExpression`](https://github.com/brentdax/swift/blob/incomplete-range/stdlib/public/core/RangeExpression.swift.gyb)
`subscript(bounds: ClosedRange<String.Index>) -> String` |❌Collapsed with the above
`init<S : Sequence>(_: S) where S.Iterator.Element == Character` |✅`init<S : StringProtocol>(_: S)`
`mutating func reserveCapacity(_: Int)` |⛏️At least rename this; it can only reserve ASCII capacity today.  Probably this should be on the UnicodeScalars.
`mutating func append<S : Sequence>(contentsOf: S) where S.Iterator.Element == Character` |✅`mutating func append<S: StringProtocol>(_: S)`
`mutating func replaceSubrange<C : Collection>(`<br/>`  _: Range<String.Index>, with: C) where C.Iterator.Element == Character` | ✅`mutating func replaceSubrange<R: RangeExpression, S: StringProtocol>(_: R, with: S) where R: Bound == String.Index`
`mutating func replaceSubrange(`<br/>`  _: Range<String.Index>, with: String)` |✅`mutating func replaceSubrange<S: StringProtocol>(`<br/>`  _: Range<String.Index>, with: S)`
`mutating func replaceSubrange<C : Collection>(_: ClosedRange<String.Index>, with: C) where C.Iterator.Element == Character` |❌
`mutating func replaceSubrange(`<br/>`  _: ClosedRange<String.Index>, with: String)` |❓
`mutating func insert(_: Character, at: String.Index)` |❓
`mutating func insert<S : Collection>(contentsOf: S, at: String.Index) where S.Iterator.Element == Character` |❓
`@discardableResult`<br/>`mutating func remove(at: String.Index) -> Character` | ❌️Drop this
`mutating func removeSubrange(_: Range<String.Index>)` | Cosolidate ranges under a protocol
`mutating func removeSubrange(`<br/>`  _: ClosedRange<String.Index>)` |❓
`mutating func removeAll(keepingCapacity: Bool = default)` |❓
`init<Subject>(describing: Subject)` |❓
`init<Subject>(reflecting: Subject)` |❓
`static var defaultCStringEncoding: String.Encoding` | ↗️Move these onto StringEncoding (UnicodeEncoding?)
`static func localizedName(of: String.Encoding) -> String` |❓
`static func localizedStringWithFormat(`<br/>`  _: String, _: CVarArg...) -> String` | ❌️Kill off printf-style interface
`init?(utf8String: UnsafePointer<CChar>)` | ❓Duplicates init(cString:)?
`func canBeConverted(to: String.Encoding) -> Bool` | ↗️Move to StringEncoding:  e.canRepresent(s)
`var capitalized: String` |❓
`var localizedCapitalized: String` | ❓Locales->Text
`func capitalized(with: Locale?) -> String` |❓
`func caseInsensitiveCompare(`<br/>`  _: String) -> ComparisonResult` |❓
`func commonPrefix(`<br/>`  with: String,`<br/>`  options: String.CompareOptions = default`<br/>`) -> String` | Replace with mismatch
`func compare(`<br/>`  _: String,`<br/>`  options: String.CompareOptions = default,`<br/>`  range: Range<String.Index>? = default,`<br/>`  locale: Locale? = default`<br/>`) -> ComparisonResult` |❓
`func completePath(`<br/>`  into: UnsafeMutablePointer<String>? = default,`<br/>`  caseSensitive: Bool,`<br/>`  matchesInto: UnsafeMutablePointer<[String]>? = default,`<br/>`  filterTypes: [String]? = default`<br/>`) -> Int` | ↗️Transplant
`func components(`<br/>`  separatedBy: CharacterSet`<br/>`) -> [String]` | Split
`func components(separatedBy: String) -> [String]` |❓
`func data(`<br/>`  using: String.Encoding,`<br/>`  allowLossyConversion: Bool = default`<br/>`) -> Data?` | ↗️Should be a failable `Data.init`
`var decomposedStringWithCanonicalMapping: String` | ❓Undecided
`var decomposedStringWithCompatibilityMapping: String` |❓
`var precomposedStringWithCanonicalMapping: String` |❓
`var precomposedStringWithCompatibilityMapping: String` |❓
`func enumerateLines(`<br/>`  invoking: @escaping (String, inout Bool) -> ())` |❓
`func getLineStart(`<br/>`  _: UnsafeMutablePointer<String.Index>,`<br/>`  end: UnsafeMutablePointer<String.Index>,`<br/>`  contentsEnd: UnsafeMutablePointer<String.Index>,`<br/>`  for: Range<String.Index>)` |❓
`func getParagraphStart(`<br/>`  _: UnsafeMutablePointer<String.Index>,`<br/>`  end: UnsafeMutablePointer<String.Index>,`<br/>`  contentsEnd: UnsafeMutablePointer<String.Index>,`<br/>`  for: Range<String.Index>)` |❓
`func enumerateSubstrings(`<br/>`  in: Range<String.Index>,`<br/>`  options: String.EnumerationOptions = default,`<br/>`  _: @escaping (`<br/>`    String?,`<br/>`  Range<String.Index>,`<br/>`  Range<String.Index>,`<br/>`  inout Bool) -> ())` | ❓Should be one or more collection/sequence properties
`func enumerateLinguisticTags(`<br/>`  in: Range<String.Index>,`<br/>`  scheme: String,`<br/>`  options: NSLinguisticTagger.Options = default,`<br/>`  orthography: NSOrthography? = default,`<br/>`  invoking: (String,`<br/>`  Range<String.Index>,`<br/>`  Range<String.Index>, inout Bool) -> ())` | ❓LinguisticTagger
 `var fastestEncoding: String.Encoding` | ↗️StringEncoding init
`func getBytes(`<br/>`  _: inout [UInt8],`<br/>`  maxLength: Int,`<br/>`  usedLength: UnsafeMutablePointer<Int>,`<br/>`  encoding: String.Encoding,`<br/>`  options: String.EncodingConversionOptions = default,`<br/>`  range: Range<String.Index>,`<br/>`  remaining: UnsafeMutablePointer<Range<String.Index>>`<br/>`) -> Bool` | ❓Encoding/Decoding
`init?<S : Sequence>(bytes: S, encoding: String.Encoding) where S.Iterator.Element == UInt8` |❓
`init?(`<br/>`  bytesNoCopy: UnsafeMutableRawPointer,`<br/>`  length: Int,`<br/>`  encoding: String.Encoding,`<br/>`  freeWhenDone: Bool)` |❓
`init(`<br/>`  utf16CodeUnits: UnsafePointer<unichar>,`<br/>`  count: Int)` |❓
`var hash: Int` | We should kill off the incorrect == behavior and associated hash
`init(`<br/>`  utf16CodeUnitsNoCopy: UnsafePointer<unichar>,`<br/>`  count: Int,`<br/>`  freeWhenDone: Bool)` |❓
`init(`<br/>`  contentsOfFile: String,`<br/>`  encoding: String.Encoding) throws` | ↗️Undecided, but definitely belongs elsewhere.  Why do we have path Strings
`init(`<br/>`  contentsOfFile: String, usedEncoding: inout String.Encoding`<br/>`) throws` |❓
`init(contentsOfFile: String) throws` |❓
`init(`<br/>`  contentsOf: URL,`<br/>`  encoding: String.Encoding) throws` |❓
`init(`<br/>`  contentsOf: URL,`<br/>`  usedEncoding: inout String.Encoding`<br/>`) throws` |❓
`init(contentsOf: URL) throws` |❓
`init?(data: Data, encoding: String.Encoding)` |❓
`init(format: String, _: CVarArg...)` |❓
`init(format: String, arguments: [CVarArg])` |❓
`init(`<br/>`  format: String,`<br/>`  locale: Locale?,`<br/>`  _: CVarArg...)` |❓
`init(`<br/>`  format: String,`<br/>`  locale: Locale?,`<br/>`  arguments: [CVarArg])` |❓
`func lengthOfBytes(using: String.Encoding) -> Int` |❓
`func lineRange(`<br/>`  for: Range<String.Index>`<br/>`) -> Range<String.Index>` |❓
`func linguisticTags(`<br/>`  in: Range<String.Index>,`<br/>`  scheme: String,`<br/>`  options: NSLinguisticTagger.Options = default,`<br/>`  orthography: NSOrthography? = default,`<br/>`  tokenRanges: UnsafeMutablePointer<[Range<String.Index>]>? = default`<br/>`) -> [String]` |❓
`func localizedCaseInsensitiveCompare(`<br/>`  _: String) -> ComparisonResult` |❓
`func localizedCompare(`<br/>`  _: String) -> ComparisonResult` |❓
`func localizedStandardCompare(`<br/>`  _: String) -> ComparisonResult` |❓
`var localizedLowercase: String` |❓
`func lowercased(with: Locale?) -> String` |❓
`func maximumLengthOfBytes(using: String.Encoding) -> Int` |❓
`func paragraphRange(`<br/>`  for: Range<String.Index>) -> Range<String.Index>` |❓
`func propertyList() -> Any` |❓
`func propertyListFromStringsFileFormat()`<br/>`  -> [String : String]` |❓
`func rangeOfCharacter(`<br/>`  from: CharacterSet,`<br/>`  options: String.CompareOptions = default,`<br/>`  range: Range<String.Index>? = default`<br/>`) -> Range<String.Index>?` |❓
`func rangeOfComposedCharacterSequence(`<br/>`  at: String.Index) -> Range<String.Index>` |❓
`func rangeOfComposedCharacterSequences(`<br/>`  for: Range<String.Index>) -> Range<String.Index>` |❓
`func range(`<br/>`  of: String,`<br/>`  options: String.CompareOptions = default,`<br/>`  range: Range<String.Index>? = default,`<br/>`  locale: Locale? = default`<br/>`) -> Range<String.Index>?` |❓
`func localizedStandardContains(_: String) -> Bool` |❓
`func localizedStandardRange(`<br/>`  of: String`<br/>`) -> Range<String.Index>?` |❓
`var smallestEncoding: String.Encoding` |❓
`func addingPercentEncoding(`<br/>`  withAllowedCharacters: CharacterSet) -> String?` |❓
`func appendingFormat(_: String, _: CVarArg...) -> String` |❓
`func appending(_: String) -> String` |❓
`func folding(`<br/>`  options: String.CompareOptions = default,`<br/>`  locale: Locale?) -> String` |❓
`func padding(`<br/>`  toLength: Int, withPad: String, startingAt: Int) -> String` |❓
`var removingPercentEncoding: String?` |❓
`func replacingCharacters(`<br/>`  in: Range<String.Index>, with: String) -> String` |❓
`func replacingOccurrences(`<br/>`  of: String, with: String,`<br/>`  options: String.CompareOptions = default,`<br/>`  range: Range<String.Index>? = default) -> String` |❓
`func trimmingCharacters(in: CharacterSet) -> String` |❓
`func substring(from: String.Index) -> String` |❓
`func substring(to: String.Index) -> String` |❓
`func substring(with: Range<String.Index>) -> String` |❓
`var localizedUppercase: String` |❓
`func uppercased(with: Locale?) -> String` |❓
`func write(`<br/>`  toFile: String,`<br/>`  atomically: Bool,`<br/>`  encoding: String.Encoding) throws` |❓
`func write(`<br/>`  to: URL,`<br/>`  atomically: Bool,`<br/>`  encoding: String.Encoding) throws` |❓
`func applyingTransform(`<br/>`  _: StringTransform, reverse: Bool) -> String?` |❓
`func contains(_: String) -> Bool` |❓
`func localizedCaseInsensitiveContains(_: String) -> Bool` |❓
`struct Encoding` |❓
`typealias EncodingConversionOptions =` ... |❓
`typealias EnumerationOptions =` ... |❓
`typealias CompareOptions =` ... |❓


## Open Questions

### `TextOutputStream` and `TextOutputStreamable`

Do we need the `TextOutputStream` API?  It is optimized for formatting
directly into a destination without the creation of intermediate strings
and/or string storage, and without the cost of type erasure imposed by
trafficking in string directly.  

If we have a `StringProtocol`, in principle we might avoid these APIs by
creating models of `StringProtocol` that store the things being formatted.  An
example of what I'm describing can be seen
in
[this example](https://github.com/apple/swift/blob/cc3947066f24d3abc32cfe4fc55ac05ba44db796/test/Prototypes/TextFormatting.swift/#L175)),
but of course that uses `TextOutputStreamable`'s API, which “pushes” text into a
`TextOutputStream`.  Modeling it as a `StringProtocol` instance would
effectively mean exposing a `Collection` of `UnicodeScalar`s that would be
“pulled” from.  I'm not sure whether that can be as effectively optimized,
especially while the language has no native inversion-of-control (e.g. `yield`)
mechanism.  In fact, we have no data on whether using `TextOutputStream` is even
currently a performance win, and I am not sure what experiments might be needed
in order to find out.

One thing is clear: if we do keep it, 

### `description` and `debugDescription`

* Should these be creating localized or non-localized representations?
* Is returning a `String` efficient enough?
* Is `debugDescription` pulling its weight?

### Naming, Again

`StringProtocol` and `TextProtocol` are not very inspiring names, but I can't
think of better ones.

### `StaticString`

`StaticString` was added as a byproduct of standard library developed and kept
around because it seemed useful, but it was never truly *designed* for client
programmers.  We need to decide what happens with it.

### Immutability

Can the basic `StringProtocol` include mutation, or do we also need to have a
`MutableStringProtocol`? This hinges on whether there are important models of
`StringProtocol` for which it would be costly to support mutation.  For example,
it might be hard to build a super-efficient model that could refer to string
constants in the data segment, if it also needed to support mutation.

---------------

<b id="f1">1</b> For example, ONE DOT LEADER (u2024) and TWO DOT LEADER (u2025)
are both complete grapheme clusters, but according to Unicode,
`"\u{2024}\u{2024}"` is equivalent to `"\u{2025}"`. [↩](#a1)


<!-- Local Variables: -->
<!-- eval: (buffer-face-mode 1) -->
<!-- End: -->
