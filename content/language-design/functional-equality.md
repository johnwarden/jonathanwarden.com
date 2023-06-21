---
title: "Functional Equality: When 2+2 does not equal 4.0"
slug: "functional-equality"
image: assets/images/Aristotle-1.jpg
date: "2015-05-16T22:22:00-05:00"
weight: 10
math: true
aliases:
- /2015/05/16/everything-is-the-same-everything-is-different/
- /everything-is-the-same-everything-is-different

---

TODO: implicit conversions

## Introduction

In this essay, I introduce the concept of **functional equality**, and discuss why  `2+2` may or may not equal `4.0`.


Obviously `2+2` represents the same number as `4.0`. And yet in many programming languages, these two expressions evaluate to **functionally** different values, because they can produce different results when input to the same function.

For example, consider this Scala program:

{{< highlight scala "linenos=false" >}}
var a = 2+2
var b = 4.0

// outputs "true"
print(x == y) 

// outputs "4"
print(a.toString()) 

// outputs "4.0"
print(a.toString()) 


{{< /highlight >}}

In the code above `a == b` is true, yet the variables `a` and `b` they are not functionally equal, because they have different string representations. But does this matter?

When comparing two values for equality, it is important to ask, *what do you mean by "equal"?* In some cases, programmers just want to know if two values **represent** the same underlying value, such as the same numeric value (regardless of type), the same moment in time (regardless of time zones), or the same physical quantity (regardless of unit of measurement).

But in other cases, what matters is whether the two values are **the exact same value**. Two values are exactly the same, for all intents and purposes, if they are **exchangeable** -- if you can replace one value with the other in your program and get the exact same output. This only happens if the two values are **functionally equal**.

In this essay I will explore the difference between functional equality and other types of equality, and explore how some common programming languages approach equality comparisons. I argue that the `==` operator should always respect functional equality, while making it easy to also test for other types of equality. I then recommend the use of [**representationless types**](/representationless-types) to more explicitly distinguish between a value and its representations.


## Definition of Functional Equality

In a functional language, the principle of functional equality says that if `a == b`, then there should exist no function `f` for which `f(a) != f(b)`. 

Formally, `a = b` iff:

$$
∀f ~ f(a) = f(b)
$$

## Why Functional Equality is Important

Some situations require less strict equality tests, such as numerical equality. But there are cases where strict functional equality tests are needed.

### Caching

When implementing a cache, two cache keys should probably not be considered equal unless they are functionally equal. Otherwise the cache can return a value for a functionally different key.  This would violate the basic principle of a cache, which should only effect program performance, never behavior.

### Debugging

Programmers expect "equal" inputs to produce equal outputs. 

Suppose a programmer has written a program that plots data on charts, placing labels with the numeric value next to each point. She may be encounter a situation where a label sometimes is displayed as "4.0" and sometimes is displayed as "4". While debugging she is perplexed to find that she gets different outputs for two different variables that the `==` operator says are equal!

Only after considerable frustration does she realize that the inputs, while passing the `==` test, are functionally different, because one uses floats and the other integers. She was testing for numerical equality, but what she really wanted to know if the two values were functionally equal.


### Functional Purity

A functionally pure language must respect the principle of <a href="http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)">referential transparency</a>. An expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program. But shouldn't we be able to replace any value with an **equal value** without changing the behavior of the program? Obviously yes. And so I would argue that a pure functional programming language must support a concept of functional equality.


## Different Representations of the Same Value

Programmers are often tempted to think of two different values as being equal when they are different **representations** of the same underlying value.

- Two timestamps corresponding to the same moment in time but with different time zones
- Two measurements representing the same length but using different units
- Two different but visually and semantically identical unicode strings (e.g. “ñ” (U+00F1) and "ñ" (U+006E U+0303) )
- Two complex data structures representing the same data at different locations in memory

But it is best to think of each of these cases as involving functionally **different** values that can be used to **represent** the **same underlying value**. They can be thought of as "equal" in one way, but because they behave differently (e.g. have different string representations) they are not functionally equal.

Mathematically, when we think of "values", we think of numbers, or other ideal abstractions such as sets. But in programming, values are data which **represent** something. In most languages values have a type, and *the type is part of the value*. And values of different types behave differently. This is why an integer variable and a float variable both representing the same *mathematical* value can be *functionally* different values.


## Common Approaches to Equality

Many languages enable testing for functional equality, while also making it easy to test for equality of underlying values.

### Separate Equality Operators

Javascript provides a separate equality operator. The strict equality operator `===` tests for functional equality, while the `==` operator tests for...[something else](https://algassert.com/visualization/2014/03/27/Better-JS-Equality-Table.html).

### Disallowing Comparison Across Types

In other languages, the `==` operator is reserved for strict functional equality. This means that (`int`) `4` and (`float`) `4.0` will never be equal. But when working with numbers, what developers care about is often numeric equality, and finding that `2+2 != 4.0` could surprise some developers, and thus cause bugs.




In Javascript, you can compare two values of two different types for strict functional equality, but this comparison is pointless, because values of different types can never be functionally equal. But in many typed languages the compiler doesn't even allow comparison across types. Programmers must explicitly convert values to a common type before comparing. For example in Go:

{{< highlight go "linenos=false" >}}

var a int64 = 2+2
var b float64 = 4.0

// compile error
// a == b

// true
float64(a) == b

{{< /highlight >}}


### Normal Forms

When there are multiple ways of representing the same value, one representation is often the **normal** representation (also "standard" or "canonical" form). To test if two values represent the same underlying value, the best approach is often to just compare their normal forms: for example, to see if two timestamps represent the same moment in time we can convert them to a Unix epoch time (which is always UTC).

### Operator Overloading


Because Go does not support operator overloading, it can still fail to respect strict functional equality. Many types support different **internal** representations of the same value. For example, a `Rational` data type might be implemented such that `1/2` and `2/4` are functionally equal. Except that the `==` test would fail, since the `==` operator compares internal representations.

Many languages solve this problem by allowing the `==` operator to be overloaded. The problem here of course is that programmers can implement `==` in a way that fails to respect functional equality. For example, `1/2` and `2/4` are not functionally equal if their string representations are different. 

A correct implementation of a type would require the `toString` method to display the normal form, and the `==` operator to ensure equality of normal forms.

## Normalizers

This suggests possible language-level support for different internal representations of the same value. A language could allow a private `normalForm` method to be defined for a type, and then then convert values to normal form before testing for equality or converting to a string. 

Programmers could still implement a type incorrectly such that two values with the same normal form are not functionally equal, but the `normalForm` method may be simpler and less error-prone than operator overloading.


## Representationless Types 

For some use cases, the representation of a value may be irrelevant. Whether a `Rational` is represented as `1/2` or `2/4` may not matter as long as the final result is correct. Or the time zone may be irrelevant in a database as long as the correct time is displayed to the user in their local time zone.

In [**Representationless Types**](/representationless-types), I suggest the use of completely separate types for the representations of a value (e.g. local timestamps with time zones) and the underlying value (e.g. a moment in time), and I discuss the challenges with this approach and explore the idea of representationless timestamps, pointers, and numbers.

## Summary: Everything is the Same, Everything is Different

If you think about it, any two things are the same in some ways, but different in other ways. So if you want to know whether two values are equal, you must first ask yourself *what do you mean by equal*?

The *functional equality* principle allows `a` and `b` to pass the equality test only if it is never true that `f(a) != f(b)` for any `f`.

I suggest that languages should reserve the `==` operator for functional equality, and then provide other ways of testing for other types of equality, such as separate numeric equality operators or deep equality operators. Pure functional programming languages in particular need a proper functional equality test to ensure **equal inputs always produce equal outputs**.


. If a type supports different internal representations of functionally equal values, there needs to be a way to overload the `==` operator, but then this can be implemented in a way that violates functional equality. Deep equality tests can be used 

In another essay, I suggest the idea of [**Representationless types**](/representationless-types) to represent values independently of their representations







## Multiple Internal Represent
ations

Often the user doesn't need different representations of a value, but there can be performance advantages to allowing different *internal* representations of the same functional value. For example, a user probably doesn't need multiple ways of representing the same rational number, so always storing rationals in standard form would make sense, except converting a rational to standard form involves finding a GCD, which can be computationally expensive, and doing so after every operation may be unnecessary.

A better approach would be to convert internal representations to normal form only when necessary: when testing for `==` or converting to a string. Two rational numbers could then have different internal representations and still be functionally equal.

A language could support this approach by allowing types to define a `NormalForm` method, and then converting values to normal form before testing for equality or converting to a string. Overloading the `==` operator would not be necessary in such a language.






Languages that allow overloading the `==` operator solve one problem while opening the door to others, because of course programmers can implement `==` incorrectly...or at least ways that violates functional equality.

If `==` is implemented such that two different representations of the same underlying value are equal, this robs the user of a functional equality test for the type. 

I suggest that the `==` operator should always test for functional equality, and that there should be other ways of testing for the equality of underlying values. 

### Normal Representations

When there are multiple ways of representing the same value, one representation is often the **normal** representation (also "standard" or "canonical" form). To test if two values represent the same underlying value, the best approach is often to just compare their normal representations: for example, to see if two timestamps represent the same moment in time we can convert them to a Unix epoch time (which is always UTC). Or we can convert two `Rational`s to standard form.





## Multiple Internal Representations

Often the user doesn't need different representations of a value, but there can be performance advantages to allowing different *internal* representations of the same functional value. For example, a user probably doesn't need multiple ways of representing the same rational number, so always storing rationals in standard form would make sense, except converting a rational to standard form involves finding a GCD, which can be computationally expensive, and doing so after every operation may be unnecessary.

A better approach would be to convert internal representations to normal form only when necessary: when testing for `==` or converting to a string. Two rational numbers could then have different internal representations and still be functionally equal.

A language could support this approach by allowing types to define a `NormalForm` method, and then converting values to normal form before testing for equality or converting to a string. Overloading the `==` operator would not be necessary in such a language.

## Representationless Types

A good approach to functional equality is to make `==` respect functional equality, disallow comparison across types, disable implicit conversions, and make it easy to convert values to a common type or normal form to compare for equality of underlying values.

For types that can represent the same value TODO

But I suggest an even more refined approach to equality: completely separate types for the representations of a value (e.g. local timestamps with time zones) and the underlying value (e.g. a moment in time). [**Representationless Types**](/representationless-types) would be independent of any particular representation: they would hide their internal representation, though they would use a default representation when converted to a string. I discuss the challenges with this approach and explore the idea of representationless timestamps, pointers, and numbers.


 and have no default string form: you must select a representation (e.g. a time zone) before you can display it as string. 

TODO: also default representation


----



  - Two see if two representations represent the same underlying value, convert them to a representationaless value first (e.g. convert `LocalTime` values to `UniversalTime` values).
  - Two represent (e.g. print out) representationless values, you must first choose a specific representation (a time zone, units, etc.).
- **Representationless numeric types** would be possible if the language required the precision of the output of numeric operations to be explicitly specified. This could be done at the level of a block scope.




### MISC




### Deep Equality Tests

In languages that use pointers, two complex data structures might represent the same underlying data, but if they include different pointer values, they are usually treated by the language as two separate values. Many languages provide a "deep equality" tests to determine if two different data structures represent the same underlying data.
For example, the elements of two equal sets may be stored in different orders. These differences mean the `==` test for equal sets will fail unless `==` is overloaded. But the sets may be functionally different: if, for example, the elements of the set are iterated over in different orders.


For example in Go, you can see if two  [`time.Time`](https://pkg.go.dev/time#Time) values represent the same moment in time by comparing the value of the `Unix()` method

## MIsc

### Reference Types, Pointers, and Mutability

Two complex data structures might represent the same underlying data but with different pointer values, which can result in functional differences. In most languages, a simple equality test will treat these structures as different, and a separate "deep equality" function must be used to ask if they represent the same underlying data.

For mutable data structures, the concept of "equality" gets even more complicated, but I will not get into that here.


### Functionally Equal Inputs Should be Equal

Treating functionally equal values as not equal can also be a problem. Two values of a Rational type might be functionally identical even though their internal representations differ. If a pair of otherwise-functionally identical values sometimes may or may not pass the `==` test, depending on how they happen to be represented internally, this can be a source of all sorts of bugs.

Complex data structures involving pointers might represent the same underlying data, but in most languages these won't pass the `==` test unless the `==` operator is overloaded. 



might represent the same underlying data, but because they have different internal representations, the language treats

 In most languages, a simple equality test will treat these structures as different, and a separate "deep equality" function must be used to ask if they represent the same underlying data.



 ---




In another article, I propose [Representationless Types](/representationless-types) that represent some underlying value independently of its representation, such as a moment in time without a time zone




I think Go has a better approach to functional equality than Haskell and Javascript. Making the `==` operator always perform a strict functional equality test prevents certain foot-guns: users will never accidentally test for equal underyling values when they meant to test for functional equality. If users want to test for whether two values represent the same underlying value, they must  convert the values to the same representation. 

Go helps to enforce this by disallowing equality comparison across types. But it is also possible for a single type to represent the same underlying value in different ways. For example [`time.Time`](https://pkg.go.dev/time#Time) can represent the same moment in time in different time zones. 

TODO: need to overload equality operator....


