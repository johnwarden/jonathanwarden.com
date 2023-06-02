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

In this post, I'll argue that functional programming languages should respect the principle of **functional equality**, which states that if two values are equal, then all functions of those values should also be equal. 

More precisely, if `a == b`, then there should exist no function `f` for which `f(a) != f(b)`. So this statement should never be true.

{{< highlight scala "linenos=false" >}}
(a == b) && (f(a) != f(b))
{{< /highlight >}}

Or more formally:

$$
	\forall a, b \in \mathbb{R}, a = b \implies \forall f \in \mathbb{F}, f(a) = f(b)
$$


Many functional programming languages violate this principle, which can cause problems for programmers. I propose that variables of two different types should not even be comparable, and suggest **representationaless types** for situations where there are different representations of the same underlying value (e.g. timestamps representing the same time in different time zones). A **representationless numeric type** is possible if the precision of the output of numeric operations is explicitly specified.


## It's Easy to Violate Functional Equality

Many functional programming languages violate the principle of functional equality. 

In Scala, for example, the toString() method evaluates to different values for `a` and `b`, even though the `==` operator says they are equal.

{{< highlight scala "linenos=false" >}}
var a = 2+2
var b = 4.0
print(a == b && a.toString() != b.toString())
{{< /highlight >}}

## Does 2+2 = 4.0?

The answer to this apparently trivial question really depends on *what do you mean by equal!?*

If the question is whether `2+2` and `4.0` evaluate to equal points on the number line, then the answer is yes. But in other ways, `2+2` obviously is not the same as `4.0`. They are two different expressions. When evaluated they may have different types (i.e. `float` and `int`). In most programming languages, *the type is part of the value*, and values of different types behave differently (e.g. the `/` may perform integer division for `int`s, and float-point division for `float`s), even when they hold the same **mathematical** value. So two numerical values of different types may be functionally different.

## Violating Functional Equality Creates Problems

But do these differences matter?

I propose that what matters to programmers is whether two values are **interchangeable**. If `a` is really equal to `b`, you should be able to replace `a` with `b` anywhere in your code, and nothing in your output would change at all. 

Suppose a developer writes a program that plots data on charts, placing labels with the numeric value next to each point.

She may be encounter a situation where the same chart sometimes includes a decimal point in the labels, and sometimes doesn't. While debugging she is perplexed to find situations where *equal inputs produce different outputs*, which her intuition says should not be possible in a functional programming language. Only after considerable frustration does she realize that the inputs, while passing the `==` test, are actually different, because one uses floats and the other integers. She'll then realize that she needs to rethink her definition of "equality".


## Functional Purity and Functional Equality

This idea is related to the functional programming concept of <a href="http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)">referential transparency</a>. An expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program. But shouldn't we be able to replace any value with an **equal value** without changing the behavior of the program? Obviously yes. And so I would argue that a pure functional programming language must respect the principle of functional equality. 


## So 2+2 does not equal 4.0?

So should `2+2 == 4.0` evaluate to false?

That could be a problem as well. It would certainly be non-intuitive for many programmers, and could easily lead to very frustrating debugging sessions.

Languages such as Javascript address this problem by providing multiple equality operators. A strict equality operator `===` respects functional equality, where the `==` operator represents something akin to numeric equality. 

This is a decent solution, however I would propose that in a typed functional language, functional equality should not even be defined across types. The expression `2+2 == 4.0` should produce a compile-time error. You should be required instead to convert the values to the same type (e.g. `(float) 2+2 == 4.0`) before comparing.

In Haskell, for example, an `Int` can't even be compared to a `Float`. The following program won't compile, because `a` and `b` are different, non-comparable types.

{{< highlight haskell "linenos=false" >}}

main :: IO ()
main = do
  let a :: Int = 2
  let b :: Float = 4.0
  putStrLn $ show $ (a == b)
  
{{< /highlight >}}

Haskell, however, still allows types to be defined that violate functional equality by overloading the `==` operator using the `Eq` typeclass.




## Different Representations of the Same Value

Programmers are often tempted to think of two **different** values as being equal when they are different **representations** of the same underlying value.

- Two timestamps corresponding to the same moment in time but with different time zones
- Two measurements representing the same length but using different units
- Two unicode strings with the same NFC normalization but different codepoints

But it is best to think of each of these as **different** values that can be used to **represent** the **same underlying value**.

## Representationless Types

I suggest that language designers and library values define **representationless types** for these cases. For example, a `UniversalTime` type without a timezone would be different from the `LocalTime` type with a timezone. A `UniversalTime` value could not be compared to a `LocalTime` value, even if the timezone of the `LocalTime` value happened to be GMT. 

A `LocalTime` value could of course have a method that returns the corresponding `UniversalTime` value. So to see if two different `LocalTime` values in two different timezones represented the same actual moment in time, just compare their `UniversalTime` values:

{{< highlight java "linenos=false" >}}
	localtime1.UniversalTime() == localtime2.UniversalTime()
{{< /highlight >}}


To represent a `UniversalTime` as a string, you would need to first convert it to a localtime by specifying a timezone:

{{< highlight java "linenos=false" >}}
	localtime = universaltime.LocalTime("America/Los_Angeles")
	println(localtime.Format(ISO8601))
{{< /highlight >}}
	

Similarly I would propose, a unitless Distance type, a NFC Unicode string type, etc.


## Representationless Number Type

A **representationless** `Number` type would be tricky, but possible. In practice, the result of numerical operations needs to have a finite precision, otherwise numerical values would tend to grow indefinitely as mathematical operations are applied to the output of other mathematical operations -- especially division operations. And different ways of representing numeric values imply different logic for mathematical operations: division using floating point, decimal floating point, rational, and integer precision all produce different results.

So you would not be able to do a simple division on a representationless numeric value unless you explicitly specified the division function to use.

For example, suppose a language had a representationless `Number` type which acted as a kind of interface type that hides the specifics of its internal representation. A value of a specific numeric type could be assigned to a variable of `Number` type, and two different `Number` values could be compared for equality even if they used a different internal representations. But operations on Numeric values would need to specify a type/precision.



{{< highlight java "linenos=false" >}}
	Number a = (int64) 2+2 // a is internally represented by an int64
	Number b = (float64) 4.0 // b is internally represented by a float64

	// true, because they represent the same point on the number line
	a == b 

	// Divide by three using three different precisions
	Number result1 = a /(float64) 3
	Number result2 = a /(float32) 3
	Number result3 = a /(int64) 3

	// The result is three different values
	println(result1) // Prints 0.3333333333333333
	println(result2) // Prints 0.33333334
	println(result2) // Prints 0


{{< /highlight >}}


Alternatively, instead of specifying prevision for every mathematical operation, the language could allow precision to be set for a specific scope. For example in the following code, the function `betaMean` is defined in terms of the representationless `Number` type, and does not specify the precision of mathematical operations it performs. Instead, the caller specifies the precision.


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

## Summary

- The *functional equality* principle allows `a` and `b` to pass the equality test only if it is never true that `f(a) != f(b)` for any `f`. Formally:

$$
	\forall a, b \in \mathbb{R}, a = b \implies \forall f \in \mathbb{F}, f(a) = f(b)
$$

- A pure functional programming language should respect the principle of functional equality.
- Since in many langauges, values of different types behave differently, they should not be comparable without converting to the same type
- Two different representations of the same underlying value are not functionally equal (the same time in two different time zones, etc)
- **Representationless types** can represent values independently of their representations (a timestamp without a specific time zone, a distance without specific units). 
  - Two see if two representations represent the same underlying value, convert them to a representationaless value first (e.g. convert `LocalTime` values to `UniversalTime` values).
  - Two represent (e.g. print out) representationless values, you must first choose a specific representation (a time zone, units, etc.).
- **Representationless numeric types** would be possible if the language required the precision of the output of numeric operations to be explicitly specified. This could be done at the level of a block scope.


A language that enforces functional equality will force programmers to more concretely understand and define what exactly they mean by equality, and to write more clear and correct code.
