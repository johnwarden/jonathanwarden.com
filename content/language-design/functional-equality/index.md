---
title: "Functional Equality"
slug: "functional-equality"
image: two-dogs-shadows.png
date: "2025-05-16T22:22:00-05:00"
weight: 10
math: true
aliases:
- /2015/05/16/everything-is-the-same-everything-is-different/
- /everything-is-the-same-everything-is-different

---

**When 2 + 2 Does Not Equal 4.0**


## Introduction: What Do You Mean by Equal?

What does it mean for two values to be equal? In designing programming languages or defining types, we have to consider this question. And the wrong answer can create problems. It can lead to counter-intuitive surprises like in JavaScript where `""` and `[0]` both are equal to `0` but not to each other. It can confuse programmers who expect two equal values to be the same, when they are actually quite different. 

In this essay, I will compare **functional equality** with **semantic equality** and survey equality testing semantics across different programming languages. 

### Leibniz's Law

Philosophers have long debated the question of what makes two things **identical**, and have come up with a criterion called **Leibniz's Law**, or "the identity of indiscernibles". This law says that two things are identical if they have all their properties in common. In other words, two things are the same *if there is no way to tell them apart*.

The formal expression of Leibniz's Law is:

$$
  x = y ⟺ ∀f (f(x) = f(y))
$$

### Functional Equality and Substitutability

This translates nicely to a definition of equality in a pure functional programming language: two values are indiscernible if they have all function results in common (i.e., no function produces different results when applied to them). Two such indiscernible values are **functionally equal**.


We can extend this definition of functional equality to languages that aren't purely functional by defining "function" to include all operators or procedures that can be applied to a value, and say that `f(x) = f(y)` also means that any effects of `f(x)` are equal to those of `f(y)`. 

Under this new definition, two values are functionally equal if we can **substitute** one for the other in our program without changing program behavior. 

### Semantic vs. Functional Equality

The criteria for functional equality is very strict. It generally means that two values can be functionally equal only if they have the exact same type -- otherwise for example `typeOf(x) != typeOf(y)`. In fact, if any function can inspect the binary representation of a value, then two values can be functionally equal only if they are identical down to the bit.


So in many programming languages, the `==` operator does not test for strict functional equality! For example, the integer `4` and the float `4.0` are often `==`, but not functionally equal, because for example integer division works differently than float division, or they have different string forms.

For example, in Python:

{{< highlight python "linenos=false" >}}
>>> 2+2 == 4.0
True
>>> str(2+2) == str(4.0)
False
{{< /highlight >}}

So functional equality is different from **numeric equality**. 

When comparing two numbers, it is usually numeric equality we are interested in. But not always. In certain scenarios such as unit tests, we truly care about substitutability. If a `float32` and `float64` behave differently in any way, then you can't always substitute one for the other without changing the program output. 

Numeric equality is a type of **semantic equality**. In this essay, I'll define and explain the idea of semantic equality. But first, I'll address some possible points of confusion in the definition of functional equality. Armed with an understanding of these two concepts, I'll compare example scenarios where either functional or semantic equality tests are needed. Finally I'll survey how the different types of equality tests are made in a variety of programming languages.

## Functional Equality

### Functional Equality vs. Identity

Functionally equal values don't have to be *identical* in the sense of Leibniz's Law, because they may have different **internal representations** as long as those representations are not exposed in the type's public API.

For example, a set type might be internally represented using a binary tree. Two binary trees holding the exact same values could have different structure (e.g. if the values were inserted in different order). But if all public functions/methods (such as `toString`) exposed set elements in sorted order, and no public functions exposed the internal tree structure, then two sets internally represented by different trees could be functionally equal.

So two values are functionally equal if they are substitutable. They don't have to be absolutely identical.

### Functional Equality and Functional Purity

Substitutability is an important concept in the theory of functional programming. A functionally pure language must respect the principle of [**referential transparency**](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)), which requires that you can substitute an expression with its value without changing program behavior. 

This almost looks identical to the definition of functional equality. But there is a subtle difference. Let's compare the criteria:


| |Referential Transparency|Functional Equality
|-|-|-
|**criteria**|you can substitute an expression with **its value** without changing program behavior|you can substitute an expression with **an equal value** without changing program behavior
|**example**|if `f(x)` is `2`, **and the value of `x` is `1.0`**, then you can replace `f(x)` with `f(1.0)` and nothing will change|if `f(x)` is `2`, **and `x` is equal to `1.0`**, then you can replace `f(x)` with `f(1.0)` and nothing will change

The difference is that referential transparency *does not require comparing two values for equality*. But in practice, a functional equality operator is critical for some functional programming techniques, such as caching/memoization, as I'll demonstrate in the cache example later in this essay.

### Equal Values vs. Equal Variables

One may be tempted to say that two values cannot be functionally equal if they have different memory addresses. But this confuses values with variables. Variables have addresses, but values have no address -- they just are. When a programmer writes `a == b`, they mean to test whether the value referred to by `a` is equal to the value referred to by `b`, not whether `a` and `b` are the same variable. 

This means that when comparing two *pointers or references* to mutable objects, functional equality requires that they point to the exact same address. Even if the values stored at two different addresses are equal, mutating one will not have the same effect on program behavior as mutating the other, so they are not substitutable. Even pointers to *immutable* copies of identical values are not functionally equal if the address of the pointer itself can be inspected (e.g. via `toString`).

## Semantic Equality

When comparing any two values, we usually don't care if they are 100% functionally equal. We just care if they are, well...the same, for all intents and purposes. 

But what are those intents and purposes? I argue that our intent is always to test whether two different values are **representations** of the same underlying **semantic value**. 

For numbers, the semantic value is its **numeric value** (with minor exceptions for IEEE floats, like NaNs or signed zeros). But there are many other types of semantic values that could have more than one representation.

- The same instant in time in different time zones
- Unicode strings with the same NFC form (e.g. “ñ” (U+00F1) and "ñ" (U+006E U+0303)) 
- Sets represented by different lists but with the same elements (e.g. `{1,2}` and `{2,1}`)
- The same length represented in different units (e.g. `1in` and `2.54cm`)
- Fractions with the same normal form (e.g. `2/4` and `1/2`)

In all these cases, there are two distinct entities: the semantic value, and its representation. These have a 1-to-many relationship.

<img id="chart1" src="multiple-representations-of-same-ideal-value.png"
     alt="Diagram showing one semantic value with multiple representations"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 500px" />

Programmers can't work with a value without representing it somehow. We need to choose a format/precision for numbers. We need to store the elements of a set in some order. So the actual values of variables programmers work with are always representations of some semantic value.

We can formally define **semantic equality** as:

$$
  x = y ⟺ SemanticValue(x) = SemanticValue(y)
$$


### The Relevant "Semantics"

The meaning of `SemanticValue` depends on your particular problem domain. For example, an application that processes timestamps in event logs probably should use **timestamp semantics**: two timestamps are equal if they represent the same instant in time, regardless of the location where the event occurred.

On the other hand, a calendar application may use **local time semantics**, where the actual time zone of an event is often relevant (a calendar event that starts at 9am New York Time behaves differently from a calendar event that starts at 3pm Madrid Time).

So there are therefore multiple possible ways that two times could be semantically equal. In such cases, it's important for the programmer to understand which semantics are relevant in order to perform the right kind of equality test.

### Canonical Representations

When there are multiple ways to represent the same semantic value, we can always designate one as the **canonical representation**. Examples of canonical representations:

- **Instants in Time**: Unix timestamps (seconds since the epoch, UTC)
- **Unicode Strings**: the NFC form
- **Sets**: an ordered list
- **Length**: length in meters
- **Rational numbers**: normal form (divide by GCD) 

Whatever the semantics, programmers can perform semantic equality tests by performing a **functional equality tests on the canonical representation**. 

***Examples (Javascript)***

{{< highlight javascript "linenos=false" >}}
// compare two timestamps using instant-in-time semantics
const a = Temporal.ZonedDateTime.from('2023-01-01T00:00:00[UTC]');
const b = Temporal.ZonedDateTime.from('2022-12-31T19:00:00[America/New_York]');
a.equals(b);  // False
a.toInstant().equals(b.toInstant());  // True
{{< /highlight >}}

{{< highlight javascript "linenos=false" >}}
// compare two Unicode strings using NFC semantics
const a = 'ñ';  // U+00F1
const b = 'n\u0303';  // U+006E U+0303
a === b;  // False
a.normalize('NFC') === b.normalize('NFC');  // True
{{< /highlight >}}

***Examples (Python)***


{{< highlight python "linenos=false" >}}
# compare two lists using set semantics
a = [1, 2]
b = [2, 1]
a == b  # False
set(a) == set(b)  # True
{{< /highlight >}}

{{< highlight python "linenos=false" >}}
import astropy.units as u
from astropy.units import imperial
u.add_enabled_units(imperial)

# compare two measurements using physical length semantics
a = 1 * imperial.inch
b = 2.54 * u.cm
a == b  # False
a.to(u.m) == b.to(u.m)  # True
{{< /highlight >}}


### Canonical Types

This naturally suggests the question: why do we even have non-canonical representations? If you always represent timestamps in event logs using epoch seconds, you never need to convert before comparing. If your rationals are always in normal form, or your set elements are always sorted, or your distances are always in meters, there is no possible confusion.

Many languages facilitate creation of **canonical types**: types that **only permit canonical representations**. 

For example, a canonical `Fraction` type only allows fractions in normal form. Python's `fractions.Fraction` is an example: the expression `Fraction(2, 4)` is automatically converted to normal form `Fraction(1, 2)`. Ruby's `Fractional`, Haskell's `Data.Ratio`, Julia's `Rational` all do the same.

<!--

Different semantics require different canonical types. For example, Javascript's new [temporal](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal) library offers two similar but distinct types for representing an historical "time":
  - [Temporal.Instant](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant): Timestamp semantics (no timezone, internally represented using epoch seconds)
  - [Temporal.ZonedDateTime](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime): Local time semantics (instant + timezone).

-->


With canonical types, there is a 1:1 relationship between the values of that type, and the semantic values they are supposed to represent. As a result, for these types **functional equality is equivalent to semantic equality**.



## When Functional Equality Matters

### Example: Caching

When implementing a cache, two cache keys should probably not be considered equal unless they are functionally equal. Otherwise the cache can return a value for a functionally different key. This would violate the basic principle of a cache, which should only affect program performance, never behavior.

For example, a polymorphic function `f(x)` might accept either a `float32` or a `float64` argument. It could be that the `float32` version of `f` and the `float64` version of `f` return numerically different values for **numerically equal inputs** -- for example the `float32` might overflow and return `+Inf` where the `float64` version doesn't.

If we put `f(x)` behind a generic cache, and that cache used **numeric equality** to check whether there was a cached result for some value of `x`, then `f(b)` might start returning `+Inf` for `float64` values that should not overflow. This would change program behavior and could introduce hard-to-find bugs.

### Example: Testing

Mistakenly using semantic equality checks in test code can cause confusing or incorrect test behavior.

Consider a unit test that looks like this (in no particular language). 

{{< highlight go "linenos=false" >}}
  result = foo()
  assert(result == 1)           // this passes
  // assert(f(result) == true)  // this fails for some reason
  assert(f(1) == true)          // but this passes

{{< /highlight >}}

The test originally failed on the assertion `f(result) == true`. But when the programmer commented that out the failed assertion and replaced it with the assertion `f(1) == true`, the test passed! But this is confusing, because the previous line of code *just* confirmed that `result == 1`.

But of course, the problem is that the programmer used the `==` operator, which is checking for semantic equality, not functional equality. `result` might, for example, be a float (`1.0`), while `1` is an int, and it may be that `f(1) != f(1.0)`.

In general, test code should use strict functional equality tests.


### Example: Programmer Intuition

Here's another example where a programmer's intuition might expect that equality implies substitutability:

{{< highlight go "linenos=false" >}}
  if result == Success {
    // return Success     // This breaks for some reason
    return result
  }
{{< /highlight >}}

If `result == Success`, why should it matter whether I return `result` or return `Success`? 

But if `==` is not a functional equality test, then it can matter (e.g. the result might have more information than just success/fail status code). If the programmer doesn't understand that, they can easily make the wrong choice.


## Common Approaches to Equality Testing

There are a few common ways that programming languages enable both functional and semantic equality tests.

By far, `==` is the most common choice for the equality operator in programming languages. And in most languages, `==` defaults to a functional equality test *when comparing two values of the same type*.

However, some languages allow customizing or overloading `==` so that it performs a semantic equality check instead.

And in some languages `==` can test for semantic equality between values of different types: most commonly between numeric types, but often between "truthy" types (e.g. `1 == True`) or "stringable" types (e.g. `13 == "13"`). This is generally done using implicit conversion or widening (e.g. convert both values to a float or a string before comparing). 

Languages such as Javascript/Typescript have a more complex [Abstract Equality Comparison algorithm](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Equality_comparisons_and_sameness#comparing_equality_methods) that even allows semantic equality between strings, numbers, and even arrays and objects (e.g. `[] == 0`).

For languages where `==` is not strict functional equality, `===` is often used as the strict functional equality operator. 

### Example: Go

In `Go`, `==` is a strict functional equality operator. The `==` operator can't be overloaded, and the compiler won't even allow values of different types to be compared.

To test for numeric equality across types, programmers must explicitly convert values to a common type:

{{< highlight go "linenos=false" >}}
var a int32 = 2+2
var b float64 = 4.0

// compile error
// a == b

// true
float64(a) == b

{{< /highlight >}}

### Example: Python

Python on the other hand allows equality comparison across values of any type. Numeric values are implicitly converted to a common type:

{{< highlight python "linenos=false" >}}
>>> a = 2+2
>>> type(a)
<class 'int'>
>>> b = 4.0
>>> type(b)
<class 'float'>
>>> a == b
True
{{< /highlight >}}

"Truthy" values of different types can also be equal:

{{< highlight python "linenos=false" >}}
>>> 1 == True
True
>>> 1 == "Blue"
False
{{< /highlight >}}

<!--

Some nuance. For `sync.Mutex`, a call to m.Lock() implicitly calls (&m).Lock(). And the mutex implemenation uses the address in the waiter queue. So two mutexes at different addresses are not functionally equal. But this is still a question of comparing objects (by address) vs. values. For any functions that use value-based semantics, of `a` and `b` are mutexes and contain the same bits, then the value-based functions produce the same result. So this is not a violation of functional equality, any more than the fact that two empty maps being non-equal is a violation of functional equality.

-->

### Normalization on Initialization

Many languages facilitate creation of canonical types via normalization of values on initialization. For example, in Scala you can define a custom `apply` function:

{{< highlight scala "linenos=false" >}}
case class Fraction(numerator: Int, denominator: Int)

object Fraction {
  def apply(n: Int, d: Int): Fraction = {
    // Assume normalization logic here (compute GCD, reduce, handle signs)
    val reducedN = /* normalized numerator */
    val reducedD = /* normalized denominator */
    Fraction(reducedN, reducedD)
  }
}

// Usage
val a = Fraction(2, 4)
val b = Fraction(1, 2)
println(a == b)  // True, as both normalize to the same values (e.g., 1/2)
{{< /highlight >}}

By enforcing that fractions are always in normal form, two functionally equal `Fraction` values will also be numerically equal, and vice versa.


### Canonical Number Types

Few languages have a single canonical number type. This is hard to achieve without sacrificing performance and constraining the range of numeric values that can be represented.

JavaScript, TypeScript, and pre-5.3 Lua have a single number type: double-precision floats. Perl represents numbers using a unified scalar interface with internal optimizations using different representations (ints, floats, and decimal strings). These languages trade simplicity for performance limitations, and constrain the range of numbers that can be represented exactly (e.g., separate types or pragmas are required for bigints or exact rationals).

The various number types differ not only in the range or precision of values that can be represented, but also in the semantics of arithmetic operations. For example, division produces different results under floating point, decimal float, exact rational, and integer division. 

Scheme, Common Lisp, Julia, and several others unify numbers under a "numeric tower" or hierarchy that includes ints, floats, exact rationals, complex, etc. These approximate canonicity through promotion rules but the different types still have differing arithmetic semantics and runtime checks that reveal the type.

A language that provided a single canonical interface for numbers, but still allowed use of different internal representations for efficiency, would require somehow decoupling internal representations from arithmetic semantics. This seems like a very difficult programming language design challenge.

### Comparison Across Languages

This table summarizes the rules for several popular programming languages
| Language | Cross-Type `==` Comparison | Functional Equality Test | Equality Operator Overloading | Normalization on Initialization | Canonical Numbers |
| - | - | - | - | - | - |
| Go | No | == | No | No | No |
| JavaScript | Abstract Equality Comparison algorithm | === | No | No | Yes (Number as doubles) |
| TypeScript | Abstract Equality Comparison algorithm | === | No | No | Yes (Number as doubles) |
| Python | Numbers and Truthy Values | type(x) == type(y) and x == y | Yes (__eq__) | Yes (__init__ for normalization) | No |
| Haskell | Yes (typeclass) | == (User-Defined via Eq) | Yes (typeclass) | Yes (newtypes for wrapping) | Partial (numeric tower) |
| Swift | No | == | Yes (Equatable protocol) | Yes (init for normalization) | No |
| Kotlin | Numbers | === | Yes (override equals()) | Yes (init for normalization) | No |
| Julia | Numbers | === | Yes (methods) | Yes (constructors) | Partial (numeric tower) |
| Java | No | == | No | Yes (constructors) | No |
| Scala | Numbers | x.getClass == y.getClass && x == y | Yes (override equals) | Yes (apply in companion object) | No |
| Rust | No | == (via PartialEq/Eq) | Yes (impl trait) | Yes (newtypes/structs) | No |
| C++ | No | operator== (User-Defined) | Yes (operator==) | Yes (constructors) | No |
| C# | No | == | Yes (operator ==) | Yes (constructors) | No |
| Ruby | Numbers | self.class == other.class && self == other | Yes (override ==) | Yes (initialize for normalization) | No |
| Perl | Numbers | $x == $y (numeric context) | No (== is built-in) | No | Yes (unified scalars) |
| Scheme | Numbers (via =) | eqv? | No | Yes (rationals only) | Partial (numeric tower) |
| Lua (pre-5.3) | N/A (single number type) | == | Yes (__eq metamethod) | No | Yes (uniform doubles) |



## Summary

If you think about it, any two things are the same in some ways, but different in other ways. So if you want to know whether two values are "equal," you must first ask yourself *what do you mean by equal*?

Two values are **functionally equal** iff substituting one for the other won't change program behavior—meaning it's never true that $f(x) \neq f(y)$ for any $f$.

Semantic equality is looser: the integer `4` and float `4.0` represent the same numeric value, but if `toString(4)` differs from `toString(4.0)`, then `2 + 2` isn't functionally equal to `4.0`.

A single **semantic value** can have multiple **representations**. **Functional equality** compares representations, **semantic equality** compares meaning. Choose a domain-tailored **canonical representation**—like sorted lists for sets or epoch seconds for times—and semantic equality tests reduce to functional equality tests on canonical representations. Make `==` a strict functional equality test to force programmers to be explicit about their intent, increasing clarity and killing bugs at the expense of convenience. Embrace **canonical types** to merge functional and semantic equality completely.


<!--







---------------









### A Canonical Numeric Type

What is the canonical representation of a number? float64? But you can't represent all numbers in a float64 -- you can't even represent all int64 values in a float64, let alone irrational and complex numbers.

It would have to be some arbitrary precision complex

Is it possible to design a language with *functional equality across numeric types.*

In such a language, numeric equality would be equivalent to functional equality. A float `4.0` and and int `4` would differ only in their internal representations. They would have the same string form ("4"), have the same `/` arithmetic (floating point). There would be no runtime `typeOf` function to distinguish them. The type checker would see int and floats as refinement types -- subsets of some canonical numeric uber-type. A function could be defined to work for any type of number, and the compiler would specialize it to work for specific numeric precisions. With carefully considered rules for handling overflow and widening, and modifying the semantics of floats (e.g. `NaN != NaN` but `+0.0 == -0.0`), I think this could be possible.






## Recommendations

I've explored various ways that functional and semantic equality tests are done in different languages. Now I'll make some recommendations.

### Start with a Functional Equality Operator

For almost any type, there is need for both semantic and functional equality tests. Since most languages can provide a functional equality operator by default (based on binary/structural equality), I think a language should *start* with a standard functional equality operator.

### Don't Allow Functional Equality Comparisons Across Types

Because in most languages, value of different types can't be functionally equal, the compiler should simply not allow functional equality tests across variables of two different concrete types. 

### Provide a Separate Cross-Type Numerical Equality Operator

Of course, programmers still need to conveniently test for semantic equality across types, especially numeric equality. They can always do this by explicitly converting values to their canonical representation before comparing (e.g. converting an into to a float before comparing it to a float, as required by Go). But a separate operator for numeric equality across types can be convenient. 

Although implicit conversion for numeric types is extremely common in modern languages, I think a separate numeric equality operator may be better. A language looses very little by introducing this, other than programmers having to learn about it, and makes it very hard for a programmer to accidentally perform the wrong type of equality check.

If the programmer tries to use the functional equality operator to compare an int and a float, the compiler will say something like "values of two different types can't be compared with `==`, did you mean to check for numeric equality?", and the programmer will make the change to an explicit numerical comparison if that's what they intended.

### Make Custom Equality Operators Test for Functional Equality

When defining a custom equality operator, make it test for functional equality, not semantic equality. 

Unless your language has multiple equality operators, overloading the equality operator to test for semantic equality robs your type of a way to test for functional equality! 

Semantic equality tests should be done by first explicitly converting values to canonical representations before comparing. 

I think the only exception should be numeric equality, where the semantics are very well defined and compatible with programmers intuition.

### No Equality Comparison Across Types, Except for Numbers

It follows that the only equality test that should be allowed across distinct types should be the numeric equality test.


### Support Canonical Types

Rather than overloading the quality operator to do semantic equality tests, use canonical types. For example, for applications with instant-in-time semantics, use a Unix timestamp, or a canonical `UTCTime` type that only allows timestamps in UTC. For applications with time-in-location semantics, use a type that can represent a timestamp in a specific timezone (like go's `time.Time`).

Similarly, instead of overloading the `==` operator for a `Rational` type so that it compares the normal forms of two rationals, keep `==` as the functional equality operator and make Rational a canonical type that only allows normal forms. 

In a language such as Haskell, where the `==` operator is always user defined, make it test for functional equality and provide other ways of testing for semantic equality.

## Topics for Exploration

### Functional Equality Across Numeric Types

I think it would be possible to define a language where values of different numeric types could be **functionally equal**.

Or more specifically, instead of different numeric types for different precisions, there would be a single rational number **uber-type**, and the different precisions would be different **internal implementations**. 

Implementations could include the various standard precivions of float and int types, ratio of ints, and custom types like decimal floats.

At the type system level, each of these implementations would correspond to a **refinement type**. And UInt8 would by a Rational constrained to integers between 0 and 255, for example.

Where the type checker saw a refinement type, the compiler would see an implementation hint.

    var x: UInt8 
    // Typechecker sees { var x : Rational | isInteger(x) && 0 <= x <= 255 } 
    // Compiler sees: I can implement this with an 8 bit unsinged int

Functions could be defined for the Rational uber-type, then specialized. For example, if you define `f(x)` for all Rational numbers:

    fn f(x: Rational): Rational {
      x ^ 2
    }

Then call it with a Uint8:

    var x: UInt8 = 2
    var y: UInt16 = f(x)

The compiler would compile a specialized version of `f` that took `UInt8` inputs and produced `UInt16` outputs.

The output has to be `UInt16` because the product of two 8-bit integers might require up to 16 bits. This raises the question of how "overflow" would be handled in such a language. I'm of the "panic on overflow" camp. So one solution would be to use an "assert" operator that panics if a value doesn't fit in range.

    // read a float64 value from somewhere
    var x: Float64 =
 
    // Ensure that the result of f(x) fits in a Float64 by panicing if it doesn't
    var y: Float64 = f(x) @ Float64

Without the explicit assert, the compiler would complain that the value returned by that call to `f(x)` can't fit in a Float64.

### Functional Equality for Floats

IEEE floats have some quirks. One of them is that `NaN == NaN` is false. So `==` doesn't test for functional equality in most languages.

But there's no reason a language couldn't tweak semantics for IEEE floats so that `NaN == NaN` was true, right? Is that too crazy?


### Lazy Canonical Types

Custom constructors are a good way to implement canonical types. But another possibility is *lazy* canonicalization, where some calculations are done on non-canonical values, and values are only normalized when nencessary. This could potentially have some performance benefits.

For example, a lazy-canonical `Fraction` type actually store values internally unnormalized. Then some operations, such as `*`, `/`, `toFloat`, could work with the unnormalized form. Normalization would only happen before calling functions that are only correct when working with the the canonical form, such as `toString`. 




---


SCRATCH/UNUSED SECTIONS BELOW



### Functional Equality and Identity

In some languages, it is easy to confuse a variable that holds a value and a variable that points to an (often mutable) object.

When comparing two object pointers or references, the `==` operator in most languages is said to test for the *identity*. That is, it tests that the variables point to the same object. This is still a functional equality test, because the actual values being compared are the references (e.g. memory addresses).


For example, a Unicode string could be encoded internally using utf8, utf16, or utf32. But it could do so such that strings using different encodings were functionally equal. 

This would require, of course, that no function could access the binary representation of the string. Further, if there was a function that returned the size of a string in bytes, and not in codepoints, functional equality would be violated. Similarly, writing the different strings to a file would result in different output, which would also violate functional equality.

But instead of a `sizeInBytes`, a Unicode string type could provide `sizeInUTF8`, `sizeInUTF16`, etc. And the encoding of a text file output stream could be specified as an argument upon opening the stream, and not depend on the internal encoding of the string written to it. Indeed there can be separate `UTF8String`, `UTF16String`, etc. types for cases where the actual encoding of the string is relevant to the programmer, allowing an encoding-agnostic `UnicodeString` type.







Two binary trees with the same values could therefore have different internal structure (if they were constructed with values in a different order). It would be therefore possible to have two sets that are functional equal and semantically equal but have **different encodings**.

For example, suppose the constructor `Set(3,2,1)` produces a value equal to `Set(1,2,3)`.

  > x = Set(3,2,1)
  > x
  Set(1,2,3)
  > x.members()
  [1,2,3]
  > y = Set(1,2,3)
  > y
  Set(1,2,3)
  > y.members()
  [1,2,3]
  > x == y
  True

However, internally, `x` and `y` are represented as binary trees constructed based on the order of values passed to their constructors.

Encoding of Set(3,2,1):

        3
       ↙
      2
     ↙
    1

Encoding of Set(1,2,3):

    1
     ↘
      2
       ↘
        3

But because the in-order traversal of these two trees is identical, functions such as `toString` and `members` will produce the same result. As long as there is *no* public function that exposes the tree structure itself, `x` and `y` are functionally equal. They use the same canonical representation, `Set(1,2,3)`, but different internal encodings.


# Ideas for Exploration

## Custom `canonical` Function

Custom constructors are a good way to implement canonical types. But another idea is a custom `canonical` functions. These can work like custom constructors but give the compiler more options. For example, the compiler can do a *lazy* canonicalization, allowing calculations to be done on non-canonical values and only canonicalizing when necessary.

For example, the constructor `Fraction(2,4)` could actually store a value in memory as `Fraction(2,4)`. It would then canonicalize in place when a function was called that needed to access the canonical form, such as `toString`. However, multiplying two *fractions* would not need to access the canonical form. I think the compiler could figure out a reasonable rule: for example if a function of a `Fraction` returned another `Fraction`, it could operate on non-canonical values. But if it returned a number (e.g. `.numerator()`) or a string (e.g. `toString()`) or any other type, it would canonicalize first.


## Overloading the `canonical` Function: Canonical Trait/Typeclass

Rather than overloading the equality operator, a language could allow a function to overload a default `canonical` function. The language could then provide a built-in semantic equality operator (e.g. `eq`), where `a eq b` is defined as `canonical(a) == canonical(b)`.

For example



### Canonical Numbers

But what is the canonical representation of a rational number? There is a standard scientific notation for *writing* rational numbers. Most languages don't have a convenient, efficient type that is capable of representing *any* rational number. An IEEE float cannot even precisely represent the number 1/3, even though this can be easily represented using a rational data type.

However, I think it is still possible to have something like a canonical number type. I hope to discuss this in a future essay.


### Canonical Floats

IEEE floats are also not canonical representation of floating point numbers. For example, they actually have multiple ways of representing the number 0: +0.0 and -0.0. 

But again, there's no reason a language couldn't tweak semantics for IEEE floats to normalize `-0.0` to `+0.0`.

Further, Floats admit values that aren't "numbers", such as NaN, +Inf, and -Inf.

A new language could slightly refine IEEE float semantics flight to create a "well-behaved float":

- Make `NaN == NaN`
- Normalize `-0.0` to `+0.0`

###

As another example, an `NFCString` type could automatically convert Unicode strings to NFC from on construction. So for example if we have two regular Unicode string variables `a="ñ”` and `b="ñ"`, which use the two different Unicode representations of the Spanish "eñe", then `a == b` would be false but `NFCString(a) == NFCString(b)` would be true.

Many types are inherently canonical including many/most numeric types. For example: all Int64 values are canonical representations: there is a 1:1 relationship between Int64 values, and the actual numbers they represent.  



#### Canonical Number Type

A single canonical number type is hard to implement without sacrificing performance or constraining the range of numeric values that can be represented.

Javascript, Typescript, and pre-5.3 Lua have a single number type (double precision floats), but this comes at the expense of performance. Perl represents numbers using a unified scalar interface with internal optimizations using different representation (ints, floats, and decimal strings). 

But these languages require a separate type for exact rationals or large ints.

Languages like Scheme, Common Lisp, Julia, and several others unify numbers under a "numeric tower" or hierarchy but they almost all have some runtime check that reveals the type and breaks functional equality. 

Numeric types differ not only in the range or precision of values that can be represented, but also in the actual semantics of arithmetic operations. A single canonical number type would need to somehow decouple representations from arithmetic, requiring programmers to specify the semantics of their arithmetic operations (integer, float, decimal float, or exact rational), along with precision and overflow rules, instead of lettings these to be determined by the type.







The ideal of a canonical number type is complicated by 

Also different problem domains may require different *behaviors* for the same numeric values: floating point, integer, and exact rational division all can produce numerically distinct results. A single canonical numeric type would need to somhow decouple representations from arithmetic, and allow programmers to specify what sort of arithmetic they want (integer, float, decimal float, exact rational), precision, and overflow rules.




 what sort of arithmetic they want (integer, float, decimal float, exact rational), precision, and overflow rules, independently of the internal representation of the number.



Also different problem domains may require different *behaviors* for the same numeric values: floating point, integer, and exact rational division all can produce numerically distinct results. A single canonical numeric type would need to somhow decouple representations from arithmetic, and allow programmers to specify what sort of arithmetic they want (integer, float, decimal float, exact rational), precision, and overflow rules.




A canonical number type for representing any number would be neat, but this doesn't exist in any language I am aware of. 

Only a few languages offer a canonical number type -- a unified interface for numeric values even though different types may be used internally for optimization. Javascript, Typescript, and pre-5.3 Lua have a single number type (double precision floats). So there is only a single way of representing any number. But this has performance drawbacks, and requires separate types for exact rationals and 


For example, Perl offers a unified scalar interface. There are internal optimizations using different representation of numbers (ints, floats, and decimal strings), but no runtime type check can distinguish between. 

Many languages unify numbers under some sort of class hierarchy, but in almost all cases there is some runtime check that reveals the type.


Scheme's numeric tower unifies all exact numerics under the 'rational' type, which includes both integers (as numerator with denominator 1) and ratios (fractions).

-->