# PROVISIONAL C++ Interoperability Specification

This document condenses the many options discussed in the [C++ Interoperability
Manifesto][manifesto] into a single set of choices.  It is not official in any
way, but it provides one possible coherent view of what C++ interoperability
will look like.

## Goals
## Current state of art: importing C code into Swift
## Importing C++ APIs into Swift
### Names, identifiers and keywords
### Functions
#### Non-const pointer and reference parameters
#### Const reference parameters
#### Mapping overload sets
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
