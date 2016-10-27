# String Processing For Swift 4

The goal of re-evaluating Strings for Swift 4 has been fairly ill-defined thus
fair, with just this short blurb in
the
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

It's worth noting that ergonomics and correctness are mutually-reinforcing.  A
well-designed API is easy to use correctly and hard to use incorrectly.  An API
that is easy to use—but incorrectly—should not be considered an ergonomic
success.  Making it possible to acheive optimal performance without compromising
ergonomics or correctness is more of a challenge.

## Issues With Today's String Design

### API Surface Area

What does this API do?  When should I use it?  Answers to these questions should
be plainly obvious for APIs of primary data types.  In my opinion, `String`
fails that test.  It currently has 205 APIs, which I've broken them down into
Standard Library and Foundation, simply to show that both contribute
significantly to overall complexity:

| Source | `init` | `func` | `subscript` | `var` |
|--------------|----|----|---|----|
|**Standard Library** | 41 | 42 | 9 | 26 |
|**Foundation**| 18 | 55 | 0 | 14 |

method arity lines up as follows:

| Arity | Standard Library | Foundation |
|---|---|---|
| 0: `ƒ()` | 5 | 7 |
| 1: `ƒ(:)` | 19 | 48 |
| 2: `ƒ(::)` | 13 | 19 |
| 3: `ƒ(:::)` | 5 | 11 |
| 4: `ƒ(::::)` | 1 | 7 |
| 5: `ƒ(:::::)` | - | 2 |
| 6: `ƒ(::::::)` | - | 1 |

I consider this unacceptable. By contrast, `Int` in Swift 3.0 (*predating* the
new integers proposal that will eliminate much of its API surface area) has 80
APIs (none with more than two parameters).  Still, in my opinion, too many, but
much more manageable.

String processing is a complex domain, but that's all the *more
reason* we should present an API that's easy to grasp.

### Where's my API?

While `String` is available to all programs out-of-the-box, crucial APIs for
basic string processing tasks are still inaccessible until `Foundation` is
imported.  While it makes sense that `Foundation` is needed for esoteric jobs
such
as
[linguistic tagging](https://developer.apple.com/reference/foundation/nslinguistictagger),
one should not need to import anything to, for example, do case-insensitive
comparison.

### Strings Don't Really Have Elements

A `String` does not act like a `Collection` of anything:

```swift
// Not a Collection of Characters
"e".characters.count               // 1
"\u{301}".characters.count         // 1
("e" + "\u{301}").characters.count // also 1

// Not a Collection of UnicodeScalars
"e\u{301}".unicodeScalars.count    // 2
"\u{e9}".unicodeScalars.count      // 1
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
characters (e.g., `"\u{2024}\u{2024}"` and `"\u{2025}"`), processing one
character at a time is almost always a bug, and the fact that such cases are
rare means the bug is unlikely to be caught in testing.

- Note: Today, `String` *may appear* to act like a `Collection` of `Character`s (but
  not a `RangeReplaceableCollection`) because of a bug (see the section
  titled
  [We Don't Implement Comparison Correctly](#we-dont-implement-comparison-correctly).

### String Lacks APIs For Scanning, Matching, and Tokenization

As though it wasn't bad enough that we expose and promote a tempting collection
of `Characters` that people will use to probe string contents, we also *fail* to
provide convenient alternative APIs that are correct for that purpose.  For
example, it should be easy to cleanly express, “if this string starts with
`"f"`, process the rest of the string as follows…”  Swift is well-suited to
expressing this common pattern beautifully; we just need to add the APIs.

### Correct Internationalization is Too Hard

There is strong evidence that developers cannot determine how to use our
international APIs correctly.  Most developers are not experts in localization,
and the current international APIs present an often bewildering catalogue of
choices.  Many `String` APIs that require special attention in an international
context take four or more parameters. It is especially important for an audience
of mostly non-experts that, naïve code that compiles is very likely to be
correct, and that they can progressively learn more sophisticated techniques as
they are needed.

Among the most common internationalization errors is the unintentional
presentation to end-users of text that has not been localized.  Fortunately,
solving this problem is easy, and supports the goal of reduced API surface area.

### We Don't Implement Comparison Correctly

String comparison isn't implemented according to the Unicode standard.  All of
the following should evaluate as `true`, but currently do not:

```swift
"a" < "J" 
"\u{2024}\u{2024}" == "\u{2025}"
"\u{2024}\u{2025}" == "\u{2026}"
"\u{222c}\u{222c}\u{222c}" == "\u{222d}\u{222d}"
```

Strings in Swift are specifically intended to be—and documented as—being
Unicode-correct, so deviating from the standard in this way is not consistent
with our stated goals.

- Note: `CFString` (to which Swift's comparison implementation dispatches) and
  `NSString` behave the same as `String` in this regard. It would be
  best if they all had the same, Unicode-correct, behavior.

## Ergonomics and Correctness

### API Surface Area

## Correctness

### Unicode 9

### Emoji Support

## Performance

### Substrings Currently Leak Memory

### Opting Out of Type Erasure

### Fast Processing for ASCII-based Protocols

### Representational Optimizations

Today there is no way to efficiently process text with a known encoding and layout

<!-- Local Variables: -->
<!-- eval: (buffer-face-mode 1) -->
<!-- End: -->
