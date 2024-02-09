---
title: "Functional Equality: Why 2+2 does not equal 4.0"
slug: "functional-equality"
image: assets/images/Aristotle-1.jpg
date: "2015-05-16T22:22:00-05:00"
weight: 10
math: true
aliases:
- /2015/05/16/everything-is-the-same-everything-is-different/
- /everything-is-the-same-everything-is-different

---


## Abstract

In this post, I'll argue that functional programming languages should respect the principle of **functional equality**. This principle states that if `a` equals `b`, then there should exist no function `f` for which `f(a)` does not equal `f(b)`.

Many functional programming languages violate this principle. For example, in many languages division works differently for integers and floats, so the integer `4` and the float `4.0` can be functionally different, even though they are numerically equal. In this essay I will show how values that are "equal" but behave differently can be a source of confusion and bugs for programmers.

The root of the problem is the fact that two functionally different values are different **representations** of the same **underlying value** (the same point on the number line, the same moment in time). They are thus "equal" in some ways but different in other ways. To minimize confusion, languages should allow programmers to specify what they mean by "equal" when comparing values for equality: equal representations, or equal underlying values.

This can be done either by providing different equality operators (e.g. `==` vs `===` in Javascript), or by requiring values of different types to be converted to the same type before being compared (as enforced by languages such as Go and Haskell), or using what I call **representationless types**. 

## It's Easy to Violate Functional Equality

Many functional programming languages violate the principle of functional equality. 

In Scala, to pick an arbitrary language, the `toString` method may evaluate to different values for `a` and `b` even when the `==` operator says they are equal.

{{< highlight scala "linenos=false" >}}
var a = 2+2
var b = 4.0

// outputs true
print(a == b && a.toString() != b.toString())
{{< /highlight >}}

So Scala violates the principle of functional equality. `a == b`, even though the two values are not functionally equal.

## Violating Functional Equality Creates Problems

But do these differences matter?

I propose that what matters to programmers is whether two values are **interchangeable**. If `a` is really equal to `b`, you should be able to replace `a` with `b` anywhere in your code, and nothing in your output would change at all. 

Suppose a developer writes a program that plots data on charts, placing labels with the numeric value next to each point. She may be encounter a situation where the same chart sometimes includes a decimal point in the labels, and sometimes doesn't. While debugging she is perplexed to find situations where *equal inputs produce different outputs*, which her intuition says should not be possible in a functional programming language. Only after considerable frustration does she realize that the inputs, while passing the `==` test, are actually different, because one uses floats and the other integers. She'll then realize that she needs to rethink her definition of "equality".


## Functional Purity and Functional Equality

The principle of functional equality is related to the functional programming principle of <a href="http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)">referential transparency</a>. An expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program. But shouldn't we be able to replace any value with an **equal value** without changing the behavior of the program? Obviously yes. And so I would argue that a pure functional programming language must respect the principle of functional equality. 


## So 2+2 does not equal 4.0?

So should `2+2 == 4.0` evaluate to false?

That could be a problem as well. It would certainly be non-intuitive for many programmers, and could easily lead to very frustrating debugging sessions.

Languages such as Javascript address this problem by providing multiple equality operators. The `==` operator represents something akin to numeric equality, where the strict equality operator `===` respects functional equality.

In other languages, equality is not even *defined* across types. In Haskell, for example, an `Int` can't even be compared to a `Float`. The following program won't compile, because `a` and `b` are different, non-comparable types.

{{< highlight haskell "linenos=false" >}}
main :: IO ()
main = do
  let a :: Int = 2
  let b :: Float = 4.0
  putStrLn $ show $ (a == b) 
{{< /highlight >}}

To compare an `Int` to a `Float` in Haskell, you need to first convert them to the same type. This in a sense forces the programmer to acknowledge that the values are different, even if they represent the same numerical value. (Note Haskell still allows the programmer to violate functional equality by overloading the `==` operator using the `Eq` typeclass).

Go is a language that arguably respects functional equality. Two values are equal only if they are the exact same value -- represented by the exact same bytes in memory. (Though in the case of data structures with pointers, one might argue that Go will sometimes treat two functionally equal values as not equal).

## Everything is the Same, Everything is Different

The problem is that the concept of equality is fuzzy. `2+2` is obviously numerically equal to `4.0`. They represent the same point on the number line. But in other ways, `2+2` obviously is not the same as `4.0`. They are two different expressions. They may have different types (i.e. `float` and `int`). In most typed programming languages, *the type is part of the value*, and values of different types behave differently (e.g. the `/` may perform integer division for `int`s, and float-point division for `float`s). So two equal numerical values of different types may be functionally different.

And if you think about it, any two values are the same in some ways, but different in other ways. So if you want to know whether two values are equal, you must first ask yourself *what do you mean by equal*?

## Different Representations of the Same Value

Programmers are often tempted to think of two **different** values as being equal when they are different **representations** of the same underlying value.

- Two timestamps corresponding to the same moment in time but with different time zones
- Two measurements representing the same length but using different units
- Two unicode strings with the same NFC normalization but different codepoints

But it is best to think of each of these as two functionally **different** values that can be used to **represent** the **same underlying value**. They can be thought of as "equal" in that they represent the same underlying value, but they behave differently (e.g. have different string representations) and therefore they are not functionally equal.

In fact **the only time two values are functionally equal is when they are the same value**.

## Representationless Types

I suggest that language and library designers might define **representationless types** for situations where there are multiple ways of representating the same underlying value. For example, a `UniversalTime` type without a timezone would be different from the `LocalTime` type with a timezone. 

A `UniversalTime` value could not be compared to a `LocalTime` value, even if the timezone of the `LocalTime` value happened to be GMT. But `LocalTime` would have a method that returns the corresponding `UniversalTime`. So to see if two different `LocalTime` values in two different timezones represented the same actual moment in time, just compare their `UniversalTime` values:

{{< highlight java "linenos=false" >}}
localtime1.UniversalTime() == localtime2.UniversalTime()
{{< /highlight >}}


To represent a `UniversalTime` as a string, you would need to first convert it to a localtime by specifying a timezone:

{{< highlight java "linenos=false" >}}
localtime = universaltime.LocalTime("America/Los_Angeles")
println(localtime.Format(ISO8601))
{{< /highlight >}}

Similarly I would propose, a unitless Distance type, a NFC Unicode string type, etc.

Now for most languages, every type needs to have a string representation (e.g. `toString`, for debugging, etc). I suggest for representationless types, there be a default representation, based when appropriate on locale (local time zone, units of measurement, etc).

## Representationless Number Type

A **representationless** `Number` type would be tricky, but possible. In practice, the result of numerical operations needs to have a finite precision; otherwise numerical values would tend to grow in size indefinitely. In fact, a precise representation of numbers such as `1/3` as a float would require an infinite amount of memory (`0.3333333...` and so on forever). At some point you have to round.

So you would not be able to do a simple division on a representationless numeric value unless you explicitly specified the precision of the division function to use.

For example, suppose a language had a representationless `Number` type which acted as a kind of interface type that hides the specifics of its internal representation. A value of a specific numeric type could be assigned to a variable of `Number` type, and two different `Number` values could be compared for equality even if they used a different internal representations. But operations on Numeric values would need to specify a type/precision.



{{< highlight java "linenos=false" >}}
Number a = (int64) 2+2 // a is internally represented by an int64
Number b = (float64) 4.0 // b is internally represented by a float64

// true, because they represent the same point on the number line
a == b 

// Divide by three using three different precisions
Number result1 = 1 /(float64) 3
Number result2 = 1 /(float32) 3
Number result3 = 1 /(int64) 3

// The result is three different values
println(result1) // Prints 0.3333333333333333
println(result2) // Prints 0.33333334
println(result2) // Prints 0


{{< /highlight >}}


Alternatively, instead of specifying prevision for every mathematical operation, the language could allow precision to be set for a scope. For example in the following code, the function `betaMean` is defined in terms of the representationless `Number` type, and does not specify the precision of mathematical operations it performs. Instead, the caller specifies the precision.


{{< highlight java "linenos=false" >}}

// Return the mean of a beta distribution with the given α and β parameters
function betaMean(Number α, Number β) Number
	return α / (α + β)
}

function main() {
	Number α = 1
	Number β  = 2
	set NumericPrecision=float64 {
		println(mean(α,β)) // Prints 0.3333333333333333
	}
	set NumericPrecision=float32 {
		println(mean(α,β)) // Prints 0.33333334
	}

}

{{< /highlight >}}

One example of a kind of representationless Numeric type are Go numeric constants. Though they only exist at the compiler level -- once assigned to a variable they must be represented some finite-precision numeric type.

## Summary

- The *functional equality* principle allows `a` and `b` to pass the equality test only if it is never true that `f(a) != f(b)` for any `f`. 
- A pure functional programming language should respect the principle of functional equality.
- Since in many langauges, values of different types behave differently, they should not be comparable without converting to the same type
- Two different representations of the same underlying value are not functionally equal (the same time in two different time zones, etc)
- **Representationless types** can represent values independently of their representations (a timestamp without a specific time zone, a distance without specific units). 
  - Two see if two representations represent the same underlying value, convert them to a representationaless value first (e.g. convert `LocalTime` values to `UniversalTime` values).
  - Two represent (e.g. print out) representationless values, you must first choose a specific representation (a time zone, units, etc.).
- **Representationless numeric types** would be possible if the language required the precision of the output of numeric operations to be explicitly specified. This could be done at the level of a block scope.


A language that enforces functional equality will force programmers to more concretely understand and define what exactly they mean by equality, and to write more clear and correct code.
