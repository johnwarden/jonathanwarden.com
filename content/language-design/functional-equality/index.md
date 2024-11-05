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

In this post, I discuss the concept of **functional equality**.

If two values $a$ and $b$ are **functionally equal**, then there should exist no function $f$ for which $f(a)$ does not equal $f(b)$. Formally:

$$
a = b ⟺ ∀f ~ f(a) = f(b)
$$

In many programming languages, the `==` operator does not test for functional equality. For example, the integer `4` and the float `4.0` can be equal according to the `==` operator, and yet an integer and a float generally are not functionally equal. For example, their string representations will typically differ. If `toString(4) != toString(4.0)`, then `4` and `4.0` are not functionally equal by the definition above.

### Functional v. Ideal Equality

In many cases, the logic of a program depends on the mathematical, or *ideal*, values, and not the specific types or precisions used to represent them. In these cases, programmers want an equality test such that `2 + 2 == 4.0` evaluates to true. I call this type of equality test **ideal equality**. 

But in other cases, what matters is whether the two values are, for all intents and purposes, **exchangeable** in code: you can replace one value with the other in your program and get the exact same behavior. This requires **functional equality**.

Many bugs and much confusion stems from the fact that two functionally different values can be different **representations** of the same **ideal value**: the same point on the number line, the same moment in time, etc. So when comparing two values for equality, it is important to ask, *what do you mean by "equal"?*

## Values are Representations

There are many situations where the same ideal value can have different representations.

- Two different numeric types/precisions representing the same mathematical value
- Two timestamps corresponding to the same moment in time but with different time zones
- Two measurements representing the same length but using different units
- Two different semantically identical Unicode strings (e.g. “ñ” (U+00F1) and "ñ" (U+006E U+0303)) with the same NFC form
- Two different lists representing the same set

It helps to think of an ideal value and its representations as different entities, with a 1-to-many relationship.

<img id="chart1" src="multiple-representations-of-same-ideal-value.png"
     alt="Illustration of multiple representations of same ideal value"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 500px" />

Mathematically, when we think of "values", we often think of ideal abstractions such as numbers or sets. Of course, programmers can't work with an ideal value without representing it somehow. You need to choose a format/precision for numbers. You need to store the elements of a set in some order. Etc. So the **actual values of variables programmers work with are always representations**. And different representations -- especially if they have different types -- can behave differently, and thus be functionally different.

## When Functional Equality is Important

### Caching and Memoization

When implementing a cache, two cache keys should probably not be considered equal unless they are functionally equal. Otherwise the cache can return a value for a functionally different key. This would violate the basic principle of a cache, which should only effect program performance, never behavior. 

### Testing and Debugging

Programmers expect "equal" inputs to produce "equal" outputs. A programmer be frustrated to find that in production a certain function seems to be return the wrong value for a certain input, even though when they run unit tests, the function produces a *different* value **for the same input**, not recognizing that two inputs, while passing the `==` test, are actually functionally different values.

### Functional Purity

A functionally pure language must respect the principle of [**referential transparency**](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)). An expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program. But shouldn't we be able to replace any value with an **equal value** without changing the behavior of the program? Obviously yes. Certain functional programming techniques, such as memoization, require a respect for functional equality. And so I would argue that a pure functional programming language must support a concept of functional equality.

## Common Solutions to The Representation Problem

There are a few common ways that programming languages try to solve the representation problem (the need to have separate ways of comparing for ideal equality and functional equality).

### Separate Equality Operators

In many languages, the `==` operator implicitly converts numbers to the same type, so that for example an integer variable with value `4` will be `==` to a float variable with value `4.0` . 

And in languages such as Javascript, values of completely different types can sometimes be `==`. In Javascript `false`, `0`, `""`, `[]`, and [various other values are all `==` to `false`](https://algassert.com/visualization/2014/03/27/Better-JS-Equality-Table.html)!

Implicit conversions and other special logic for cross-type equality comparison effectively converts the `==` operator into an *ideal* equality test. 

But because a functional equality test is important for some applications, these languages often provide some other way to test for functional equality. For example Javascript provides a separate `===` operator for strict functional equality.

### Disallowing Cross-Type Comparison

In some languages, the `==` operator is reserved for functional equality. 

Since values of different types can generally not be functionally equal, `==` comparisons across types don't make sense. A float can never `==` an int.

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

However, this does not completely solve the representation problem, for it's possible for two functionally different values of *the same type* to represent the same ideal value. For example in many languages, there is a type that represents a timestamp with a timezone. Two different values of that type can represent the same moment in time in two different time zones.

Two provide an ideal equality test for such types, some languages allows `==` to be overloaded. But I think this is a mistake, because it robs the  type of a functional equality test.

Instead, the language or library should provide some other way of testing for ideal equality. 

### Canonical Representations

For example in Go, to test whether two values of type [`time.Time`](https://pkg.go.dev/time#Time) represent the same moment in time, you can convert them both to Unix epoch seconds using the [`Unix()`](https://pkg.go.dev/time#Time.Unix) method.

{{< highlight go "linenos=false" >}}

// t1 and t2 are the same time in two different time zones
t1, _ := time.Parse(time.RFC3339, "2016-06-16T19:20:30+00:00")
t2, _ := time.Parse(time.RFC3339, "2016-06-16T12:20:30-07:00")

// t1 and t2 are not functionally equal
fmt.Println(t1 == t2)
// Output: false

// but they are ideally equal
fmt.Println(t1.Unix() == t2.Unix())
// Output: true

{{< /highlight >}}

Epoch seconds is a **canonical** way of representing a moment in time in the past (epoch seconds don't have time zones, because the number of seconds that have passed since the epoch -- January 1, 1970 midnight UTC -- is the same no matter where you are on the planet). There is only **one** canonical way of representing each ideal moment in time. Therefore, comparing the canonical representation is equivalent to an ideal equality test.

## Canonical Types

A **canonical type** is a type that **only permits canonical representations**. Or in other words, there is a 1:1 relationship between the values of that type, and the ideal values they are supposed to represent.

Examples of canonical types might include `UTCTimestamp`, `NFCString`, or `OrderedSet`. 

A `UTCTimestamp` would correspond 1:1 with a moment in time in the past (e.g. a unix timestamp). 

An `NFCString` would only permit NFC forms of Unicode strings. Comparing two regular Unicode strings would compare their representations. To compare for semantic equality of two strings, convert them to `NFCStrings` first. So for example if we have two regular Unicode string variables `a="ñ”` and `b="ñ"`, which use two different Unicode representations of the Spanish "eñe", then `a == b` would be false but `a.NFC() == b.NFC()` would be true.


Two different sets that are ideally equal may not be functionally equal if the *order* that items are traversed or displayed in the set depends on how they are stored. If `a.toString() == "{1,2}"` and `b.toString() == "{2,1}", then `a` and `b` are not functionally equal. An `OrderedSet` type, that sorted the set members internally using any deterministic ordering function, would not permit two different representations of the same ideal set. 

### Canonical Numbers

But what is the canonical representation of a rational number? There is a standard scientific notation for *writing* rational numbers. But in a computer program, numeric values must be represented by some types with a finite precision. And there is no single type (yet) that is capable of representing *any* rational number. Even an arbitrarily large binary floating point, for example, cannot precisely represent the number 1/3, even though this can be easily represented using a rational or decimal float.

However, I think it is still possible to have something like a canonical number type. I hope to discuss this in a future essay.

## Summary: Everything is the Same, Everything is Different

If you think about it, any two things are the same in some ways, but different in other ways. So if you want to know whether two values are equal, you must first ask yourself *what do you mean by equal*?

Two values are **functionally equal** iff it is never true that $f(a) \neq f(b)$ for any $f$. 

But often some **ideal value** can be represented in many different ways. So there is a difference between **functional equality tests** which compare **representations**, and **ideal equality** tests which compare ideal values. 

Having more than one way to represent the same ideal value gives rise to the **representation problem**: the need to support both types of equality tests.

I suggest that languages  and libraries solve this problem by reserving the `==` operator for functional equality, and allow users to test for ideal equality by comparing canonical representations. Using canonical types can be a good way to do this.

