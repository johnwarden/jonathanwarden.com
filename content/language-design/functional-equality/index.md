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

## Introduction

In this post, I'll argue that functional programming languages should provide an operator that tests for **functional equality**. 

If two values $a$ and $b$ are functionally equal, then there should exist no function $f$ for which $f(a)$ does not equal $f(b)$. Formally:

$$
a = b ⟺ ∀f ~ f(a) = f(b)
$$

In many programming languages, the `==` operator does not test for functional equality. For example, division often works differently for integers and floats, so the integer $4$ and the float $4.0$ can be functionally different, even though they are equal according to the `==` operator.

### Functional v. Ideal Equality

Much confusion arises from the fact that two functionally different values can be different **representations** of the same **ideal value** (the same point on the number line, the same moment in time). So when comparing two values for equality, it is important to ask, *what do you mean by "equal"?* In many cases, programmers just want to know if two values **represent** the same ideal value, such as the same point on the number line, the same moment in time, or the same physical quantity. I will call this **ideal equality**. But in other cases, what matters is whether the two values are, for all intents and purposes, **exchangeable** in code: you can replace one value with the other in your program and get the exact same output. This requires **functional equality**.

### The Representation Problem

Having more than one way to represent the same ideal value gives rise to the **representation problem**: the need to distinguish between two types of equality tests. 

Programmers expect the expression $2 + 2 == 4.0$ to evaluate to true. This is a strong instinct, because these values are *mathematically* equal. And many times, perhaps most of the time, the logic of a program depends on the mathematical, or *ideal*, values, and not the specific types or precisions used to represent them.

However, there are times when correct program behavior depends on comparing for functional equality. I'll explore some of these situations below, with the hope of demonstrating how solving the representation problem is necessary to avoid certain kinds of bugs.

Then, I discuss the ways common programming languages solve the representation problem. I argue that the `==` operator should always respect functional equality, and that ideal equality tests should be done by explicitly comparing canonical representations. Finally, I recommend the use of **canonical types** that only permit canonical representations.

## When Functional Equality is Important


### Caching

When implementing a cache, two cache keys should probably not be considered equal unless they are functionally equal. Otherwise the cache can return a value for a different key. This would violate the basic principle of a cache, which should only effect program performance, never behavior. And it's easy to see how subtle differences in behavior stemming from the hazard of cache hits could cause frustrating bugs.

### Debugging

Programmers expect "equal" inputs to produce "equal" outputs. 

Suppose a programmer has written a program that plots data on charts, placing labels with the numeric value next to each point. She may be encounter a situation where a label sometimes is displayed as "4.0" and sometimes is displayed as "4". While debugging she is perplexed to find that she gets different outputs for two variables that are "equal" according to the `==` operator.

Only after considerable frustration does she realize that the inputs, while passing the `==` test, are functionally different, because one uses floats and the other integers. She was testing for numerical equality, but what she really wanted was functional equality.

### Functional Purity

A functionally pure language must respect the principle of [**referential transparency**](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)). An expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program. But shouldn't we be able to replace any value with an **equal value** without changing the behavior of the program? Obviously yes. Certain functional programming techniques, such as memoization, require testing for functional equality. And so I would argue that a pure functional programming language must support a concept of functional equality.

## Values are Representations

There are many situations where the same ideal value can have different representations.

- Two timestamps corresponding to the same moment in time but with different time zones
- Two measurements representing the same length but using different units
- Two different semantically identical Unicode strings (e.g. “ñ” (U+00F1) and "ñ" (U+006E U+0303) )
- Two different lists representing the same set

Mathematically, when we think of "values", we often think of numbers, or other ideal abstractions such as sets. Of course, you can't work with an ideal value without representing it somehow. You need to choose a format/precision for numbers. You need to store the elements of a set in some order. So the actual values of variables programmers work with are representations with some concrete type. And values of different types can behave differently. Thus two **representations** of the same ideal value using different types can be two *functionally* different values.

It helps to think of an ideal value and its representations as different entities, with a 1-to-many relationship.

<img id="chart1" src="multiple-representations-of-same-ideal-value.png"
     alt="Illustration of multiple representations of same ideal value"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 500px" />

## Common Solutions to The Representation Problem

There are a few common ways that programming languages try to solve the representation problem (the need to have separate ways of comparing for ideal equality and functional equality).

### Implicit Conversion and Special `==` Logic

In many languages, the `==` operator implicitly converts numbers to the same type, so that for example an integer variable with value `4` will be `==` to a float variable with value `4.0` . 

And in languages such as Javascript, values of completely different types can sometimes be `==`. In Javascript `false`, `0`, `""`, `[]`, and [various other values are all `==` to `false`](https://algassert.com/visualization/2014/03/27/Better-JS-Equality-Table.html)!

### Multiple Equality Operators

Implicit conversions and other special logic for cross-type equality comparison effectively converts the `==` operator into an *ideal* equality test. But because a functional equality test is important for some applications, these languages must provide some other way to test for functional equality.

For example Javascript provides a separate `===` operator for strict functional equality.

### Disallowing Cross-Type Comparison

In other languages, the `==` operator is reserved for functional equality. 

Ssince values of different types can generally not be functionally equal, `==` comparisons across types don't make sense. A float can never `==` an int.

Since developers could be confused to find that `2+2 != 4.0`, in many typed languages, the compiler doesn't even allow comparison across types. To test for ideal equality, programmers must explicitly convert values to a common type. For example in Go:

{{< highlight go "linenos=false" >}}

var a int64 = 2+2
var b float64 = 4.0

// compile error
// a == b

// true
float64(a) == b

{{< /highlight >}}

### `==` Overloading for Same-Type Comparison

However, this does not completely solve the representation problem, for it's possible for two functionally different values of *the same type* to represent the same ideal value. For example in many languages, there is a type that represents a timestamp with a timezone. Two different values can represent the same moment in time in two different time zones.

Two provide an ideal equality test for such types, some languages allows `==` to be overloaded. But I think this is a mistake, because it robs the  type of a functional equality test.

Instead, the language or library should provide some other way of testing for ideal equality. 

### Canonical Representations

In Go, to test whether two values of type [`time.Time`](https://pkg.go.dev/time#Time) represent the same moment in time, you can convert them both to Unix epoch seconds using the [`Unix()`](https://pkg.go.dev/time#Time.Unix) method.

{{< highlight go "linenos=false" >}}

// t1 and t2 are the same time in two different time zones
t1, _ := time.Parse(time.RFC3339, "2016-06-16T19:20:30+00:00")
t2, _ := time.Parse(time.RFC3339, "2016-06-16T12:20:30-07:00")

fmt.Println(t1 == t2)
// Output: false

fmt.Println(t1.Unix() == t2.Unix())
// Output: true

{{< /highlight >}}

Epoch seconds is a **canonical** way of representing a moment in time in the past (epoch seconds don't have time zones, because the number of seconds that have passed since the epoch -- January 1, 1970 midnight UTC -- is the same no matter where you are on the planet).

Similarly, meters is a canonical way of measuring distance, NFC is a canonical way of representing Unicode strings, etc.

Comparing two values' canonical representations is more clear and explicit test of ideal equality than special `==` logic.

## Canonical Types

For many applications, you don't need more than one way to represent the ideal value. In such cases, it can be safest to use a **canonical type**, which is what I call a type that only permits canonical representations.

For example, many databases have separate types for timestamps with and without a timezone. An application that just needs to record *when* something happens, and not the timezone it happens in, should use a timestamp *without* a timezone, because this represents a moment in time, stored canonically using epoch seconds. Storing an additional, irrelevant timezone just invites bugs.

I think date/time libraries should also provide separate types for moments in time (e.g. `UniversalTime`) and timestamps with timezones (e.g. `LocalTime`). In languages that don't allow comparison across types, a `UniversalTime` could not be compared to a `LocalTime` value, even if the timezone of the `LocalTime` value happened to be UTC. But `LocalTime` would have a `UniversalTime()` method, so to see if two different `LocalTime` values in two different timezones represented the same actual moment in time, one could compare their `UniversalTime` values:

{{< highlight java "linenos=false" >}}
localtime1.UniversalTime() == localtime2.UniversalTime()
{{< /highlight >}}

The string form of a `UniversalTime` (e.g. the value of `toString()`) would default of course to UTC. `UniversalTime` would probably be seen by some less experienced developers as a kind of `LocalTime` that only supported UTC. But hopefully, most developers would recognize that `UniversalTime` is the proper timestamp type to use for most applications, where timestamps need to be represented in the user's local timezone only for display purposes.

Other examples of canonical types would be a canonical distance type that only uses meters, a NFC Unicode string type that only allows strings in NFC form, and a canonical set type that stores values in sorted order (so that a function that lists elements of a set behaves equally for equal sets).

### Canonical Numbers

But what is the canonical representation of a number? There is a standard scientific notation for *writing* rational numbers. But in a computer program, numeric values must be represented by some types with a finite precision. And there is no single type (yet) that is capable of representing *any* rational number. Even an arbitrarily large floating point type, for example, cannot precisely represent the number 1/3, which can be easily represented using a rational or decimal float.

However, I think it is still possible to have something like a canonical number type. I hope to discuss this in a future essay.

## Summary: Everything is the Same, Everything is Different

If you think about it, any two things are the same in some ways, but different in other ways. So if you want to know whether two values are equal, you must first ask yourself *what do you mean by equal*?

Two values are **functionally equal** iff it is never true that $f(a) \neq f(b)$ for any $f$. 

But often some **ideal value** can be represented in many different ways. So there is a difference between **functional equality tests** which compare **representations**, and **ideal equality** tests which compare ideal values. 

Having more than one way to represent the same ideal value gives rise to the **representation problem**: the need to support both types of equality tests.

I suggest that languages  and libraries solve this problem by reserving the `==` operator for functional equality, and allow users to test for ideal equality by comparing canonical representations. Using canonical types can be a good way to do this.

