# PROVISIONAL C++ Interoperability Specification

This document condenses the many options discussed in the [C++ Interoperability
Manifesto][manifesto] into a single set of choices, with much less narrative.
It is not official in any way, but it provides one possible coherent view of
what C++ interoperability will look like.

## Goals and Tradeoffs

These are not the goals from the corresponding section of the
[manifesto][manifesto]; instead, they are the criteria by which we've chosen
what to include in this provisional specification.

- **API Accessibility**: all C++ APIs should be usable from Swift, and all Swift
  APIs usable from C++, without manual annotation or wrapping.
- The following take precedence over safety and ergonomics:
  - API Accessibility.
  - Avoiding any performance penalties at the API boundary.
- The ability to easily make an imported C++ API safe and ergonomic in Swift
  with manual intervention (annotation, wrapping, etc.) is a goal.
- Using a C++ API from Swift should not introduce any new, Swift-specific
  “gotchas,” and vice-versa.
  
## Access to entities across langauges

Keeping all APIs accessible means being able to uniquely name, in each language,
every distinct entity that can be named in the other language.  For example,
there must be a way distinguish `T*` from `T&` in Swift.

## Escaped Names

In Swift, the “escaped name” `@cplusplus(`*cpp-name*`)` refers to the C++ entity
identified by *cpp-name* in C++.  In C++, the escaped name
`@swift(`*swift-name*`)` refers to the Swift entity identified by *swift-name*
in Swift.

## Equivalences

Many entities are mapped to equivalent names in the other language. For example,
in Swift

- `@cplusplus(int)` is equivalent to `Int`.
- `@cplusplus(std::uncaught_exception)` is equivalent to `std.uncaught_exception`.
- `@cplusplus(T*)` is equivalent to `UnsafeMutablePointer<U>` when
  `@cplusplus(T)` is equivalent to `U`.

We'll write `@cplusplus(X) == Y` to mean `@cplusplus(X)` and `Y` are equivalent
in Swift.

## Similar Types

Some types without an exact equivalent across languages will nonetheless have a
close correspondence with a type in the other language.  For example, `T&` in
C++, where `T` is not *cv-qualified*, corresponds closely to
`UnsafeMutablePointer<U>` in Swift where `U == @cplusplus(T)`.  Similar types
should implicitly convert:

```swift
func f0(p: UnsafeMutablePointer<Int>, g: @cplusplus(int&(*)(int&))) 
  -> UnsafeMutablePointer<Int>
{
  g(p) // bidirectional implicit conversion
}
```

and when there is a unique similar type but no exact equivalent, should be
implicitly deduced in deduced contexts.  For example, if there is no Swift
equivalent for `T&` (e.g. `UnsafeMutableCPlusPlusReference<U>`), we would make
`UnsafeMutablePointer<U>` a unique similar type where `U == @cplusplus(T)`.
Then this function should compile without error:

```swift
func f(g: @cplusplus(int&(*)())) -> UnsafeMutablePointer<Int> {
  let p = g() // p deduced as UnsafeMutablePointer<Int>
  return p
}
```

We'll write `@cplusplus(T) ~ U` or `@cplusplus(T) ~= U` to mean that
`@cplusplus(T)` and `U` are similar or uniquely similar, respectively, in
Swift.

## Functions

There are various ways to annotate C++ and Swift functions to control how they
are exposed to the other language, specifics to be covered elsewhere in this
document.

#### Non-const pointer and reference parameters to C++ functions

Unless otherwise-annotated, pointer and reference to non-const `T` are exposed
to Swift as `UnsafeMutablePointer<T>` and `UnsafeMutableReference<T>`, where the
latter is defined as:

```swift
@propertyWrapper
struct UnsafeMutableReference<T> {
  public let address: UnsafeMutablePointer<T>
  var projectedValue: Self { self }
  init(_ address: UnsafeMutablePointer<T>) {
    self.address = address
  }
  var wrappedValue: T {
    get { address.pointee }
    nonmutating _modify { yield &address.pointee }
    nonmutating set { address.pointee = newValue }
  }
}
```

<details><summary>Rationale</summary>
  
For pointers, the precedent is already set by existing C interop.

Precedent aside, the following reasoning applies:

- in **call context**, Swift already suppors `inout` *syntax* (or implicit
  conversion from `inout`, if you like) for forming mutable pointer arguments,
  so the calling code works as-if the parameter had `inout` type.  Presenting
  the signature of such a C++ function as taking an `inout` parameter would
  simplify the user's view of the C++ API a bit, but also make the potential
  unsafety less explicit.

- in **implementation context**, a pointer to non-`const` parameter cannot be
  exposed as into `inout` because nothing on the C++ (calling) side upholds the
  exclusivity guarantees the compiler counts on for `inout` parameters.

Semantically, C++ lvalue references are exactly like C++ pointers except for
non-reseatability, implicit dereferencing, and the lack of pointer arithmetic.
The same rationales apply to choices about mapping.  A different mapping, to use
`inout` in call context, is the most we could hope for, but as noted above
provides only marginal benefits and makes the mapping asymmetric.

Mapping any two C++ types (e.g. `T*` and `T&`) to the same Swift type is a
non-starter per our goals, as it would make some C++ APIs inaccessible.  You
need to be able to access every template specialization and/or overload.

An [extension to property wrappers
](https://forums.swift.org/t/pitch-2-extend-property-wrappers-to-function-and-closure-parameters)
that seems likely to be accepted, and likely further directions, seem as though
they'll make `UnsafeMutableReference<T>` increasingly ergonomic, tipping the
balance in favor of creating this equivalent type instead of just mapping to
`@cplusplus(T&)`.

</details>

#### Const reference parameters

Unless otherwise-annotated, pointer and reference to const `T` are exposed
to Swift as `UnsafePointer<T>` and `UnsafeReference<T>`, where the
latter is defined as:

```swift
@propertyWrapper
struct UnsafeReference<T> {
  public let address: UnsafePointer<T>
  var projectedValue: Self { self }
  init(_ address: UnsafePointer<T>) {
    self.address = address
  }
  var wrappedValue: T {
    get { address.pointee }
  }
}
```

<details><summary>Rationale</summary>

The rationales mirror those in the previous section, with `shared` (from the
ownership manifesto) playing the role of `inout`.

</details>

#### Mapping overload sets

The corresponding [section](https://github.com/apple/swift/blob/master/docs/CppInteroperabilityManifesto.md#mapping-overload-sets)
of the manifesto
suggests that the `T const&` function in a `T&&`/`T const&` overload set should
not be imported at all.  Although that would be incompatible with the mandate to
keep the entire C++ API accessible from Swift, it's worth acknowledging that 
such an overload set typically corresponds to an owned-convention call in Swift.

#### Inline functions
### Namespaces and modules
#### The "std" namespace
### Structs and classes
#### Mapping the type itself and its special member functions
#### Special member functions with incorrect semantics
#### Special member functions that throw exceptions
#### Precise destruction semantics
#### Member functions that return inner pointers
#### Differences in move semantics between C++ and Swift
#### Move-only C++ classes
#### Move-only C++ classes: mapping in Swift 5
#### Move-only C++ classes: mapping to move-only Swift types (in future Swift versions)
#### Non-destructive moves
#### Non-movable C++ classes
#### Member functions
#### Const-qualified member functions in C++ classes
#### Volatile-qualified member functions
#### Ref-qualified member functions
#### Final functions
#### Getters and setters
##define SWIFT_PROPERTY(type, name) __attribute__((swift_property(#type, #name)))
#### Pointers to members
#### Virtual member functions
#### Inherited APIs
#### Conversions between derived and base classes
#### Subclassing C++ classes in Swift
#### Making C++ classes conform to Swift protocols
### Enums
### Templates
#### Function templates
#### Function templates: import as Swift generic functions
#### Function templates: allow to specify template arguments
#### Function templates: calls to specific specializations
#### Function templates: calls with generic type parameters
#### Function templates: importing as real generic functions
#### Class templates
#### Class templates: Importing full class template instantiations
#### Class templates: importing specific specializations
#### Class templates: using with generic type parameters
#### Class templates: using in generic code through a synthesized protocol
#### Class templates: importing as real generic structs
#### Mapping SFINAE
#### Variadic templates
#### Non-type template parameters
#### Template template parameters
### Overloaded operators
### <a name="operator-star-operator-arrow"></a> `operator*`, `operator->`
### <a name="operator-square-brackets"></a> `operator[]`
### <a name="operator-parentheses"></a> `operator()`
### Literal operators
### Exceptions
#### Background
#### Baseline functionality: Import functions as non-throwing, terminate on uncaught C++ exceptions
#### Extended functionality: Optionally propagate exceptions to Swift
#### Implementation
### Atomics
### Importing non-const pointer as extra return values
## Enhancing C++ API mapping into Swift with bridging
### Bridging data types with different memory layout
### Bridging: behind the scenes
### Bridging does not work in every context
### <a name="bridging-std-function"></a> Bridging `std::function`
### <a name="bridging-std-string"></a> Bridging `std::string`
### <a name="bridging-std-vector"></a> Bridging `std::vector`
### <a name="bridging-std-set-std-map"></a> Bridging `std::set`, `std::map`
### <a name="bridging-std-unordered-set-std-unordered-map"></a> Bridging `std::unordered_set`, `std::unordered_map`
### <a name="bridging-std-tuple-std-pair"></a> Bridging `std::tuple`, `std::pair`
### <a name="bridging-std-span"></a> Bridging `std::span`
### Standard C++ containers in general
### Custom C++ containers
### <a name="bridging-absl-hash-set-map"></a> Bridging `absl::flat_hash_{set,map}`, `absl::node_hash_{set,map}`
## Enhancing C++ API mapping into Swift with annotations
### Annotation workflow
### Tooling to infer nullability annotations
### APIs that take a pointer and count
## Current state of art: importing Swift code into C
## Importing Swift APIs into C++
### Resilient types
## Forum discussions


[manifesto]: https://github.com/apple/swift/blob/master/docs/CppInteroperabilityManifesto.md
