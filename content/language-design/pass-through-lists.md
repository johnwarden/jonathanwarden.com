---
title: "Pass-Through Lists"
slug: "pass-through-lists"
image: assets/images/return-value-code-graphic.jpg
alias: http://jonathanwarden.com/2014/06/19/pass-through-lists/
date: "2014-06-19T18:40:00-05:00"
weight: 20
---

The *pass-through list* is a programming-language feature intended to makes it easier for programmers to modify functions to return additional values without breaking backwards compatibility, in the same way it is easy to modify functions to take additional parameters without breaking backwards compatibility.  This is done by, in a sense, unifying the semantics of passing parameters to functions and returning values from functions.

## Problem

Suppose I have an `inverse` function that takes one number and returns one number, in some hypothetical untyped language:

{{< highlight java "linenos=false" >}}

let inverse(x) = 1/x
2 * inverse(4)

{{< /highlight >}}


I cannot change this function to return a second value, without breaking existing code:

{{< highlight java "linenos=false" >}}

// return a list with a number and a string
let inverse(x) = (1/x, "the inverse of " ++ x)

// Doesn't work any more!  
// inverse(4) returns a list, not a number.
2 * inverse(4)

{{< /highlight >}}


On the other hand, I can easily modify the function to take an optional second parameter:

{{< highlight java "linenos=false" >}}

let inverse(x, verbose=false) = // 'verbose' is optional
	if(verbose) println("Calculating the inverse of " ++ x)
	1/x // return 1/x
{{< /highlight >}}


And code that depended on the old version of the function still works with the new:

{{< highlight java "linenos=false" >}}

2 * inverse(4) // Still works!

{{< /highlight >}}


## Solution


### 1. Functions Always Return Lists

In most languages, functions accept lists of arguments, but typically return single values.  But if functions always returned lists, we wouldn't have this problem.

This unfortunately would require a lot of extra code for extracting values from lists.  But we could reduce that burden with a little syntactic sugar: allowing calling code to accept the values returned by a function using a **deconstructing assignment**:

{{< highlight java "linenos=false" >}}

// function that returns a list containing one number
let inverse(x) = (1/x)

// deconstructing assignment 
// assigns the first element of the return list to y
let (y) = inverse(4)
2 * y
{{< /highlight >}}


Now we can modify a function to return multiple values, but the caller can choose to ignore extra values:

{{< highlight java "linenos=false" >}}

let inverse(x) = (1/x, "the inverse of " ++ x)

// ignore the second return value
let (y) = inverse(4)

// or use it if we want
let (z, explanation) = inverse(4)
{{< /highlight >}}


### 2. Implicit Deconstruction

But this doesn't completely solve the problem: we still have to use a deconstructing assignment to receive the values returned by every function call, so simple code such as `2*inverse(4)` needs to be rewritten as:

{{< highlight java "linenos=false" >}}

let (result) = inverse(4)
2 * result
{{< /highlight >}}


But this can be solved with an **implicit deconstruction** rule: if a list returned by a function is not explicitly deconstructed with a deconstructing assignment, then it is *implicitly deconstructed*, with all but the first value being ignored.

So `inverse` can now return a second value, but we can still call it as if it returned just one:

{{< highlight java "linenos=false" >}}

// return a list with a number and a string
let inverse(x) = (1/x, "the inverse of " ++ x)

// implicit deconstruction: just use the first value from inverse
2 * inverse(4) 

{{< /highlight >}}


And now we have a language where it is easy to modify a function to return additional values, without breaking backwards compatibility.

### 3. Implicit Construction

Now, it's a little inconvenient for our language to force every function to explicitly return a list.

But we can solve this problem with an **implicit construction** rule, and say that a list is implicitly created in places where it is expected. So:

{{< highlight java "linenos=false" >}}

let inverse(x) = 1/x
{{< /highlight >}}


Is syntactically equivalent to:

{{< highlight java "linenos=false" >}}

let inverse(x) = (1/x)
{{< /highlight >}}


In other words, the parentheses around return lists are implicit.  You only need to explicitly include them when returning more than one value.

### 4. Implicit Construction in Function Calls

Let's say that, in our language, functions expect lists of arguments.  The implicit construction rule says that, if you don't explicitly construct a list where they are expected, one is implicitly constructed.  So:

{{< highlight java "linenos=false" >}}

inverse 4

// must be the same as
inverse(4)
{{< /highlight >}}


Implicit construction can be looked at just an optional parentheses rule.

### 5. Implicit Deconstruction in Function Definitions

Functions in our language always accept and return a *list* of values.  Now in many functional languages, functions always accept and return a *single* value.  Let's say that in our language, both of these are true: functions always accept and return a single value -- a list -- and in cases where that list only contains 1 item, implicit construction/de-construction simplifies the syntax by allowing you to construct and deconstruct the list implicitly, without parentheses.

Let's say our language supports the `->` operator for defining anonymous functions.

{{< highlight java "linenos=false" >}}

let product = (x,y) -> x * y

{{< /highlight >}}


Now, we said that functions only take a single argument, but it looks like product is taking two. But let's say that this is just syntactic sugar for deconstructing the function arguments to a function, that is equivalent to:

{{< highlight java "linenos=false" >}}

let product = args ->
	let (x,y) = args
	x * y
{{< /highlight >}}


### 6. Pass-Through Lists are Not Regular Lists

Implicit construction/deconstruction means that these lists cannot be referenced, making them into a kind ephemeral type, whose lifespan is mainly limited to passing values to or returning values from functions.  I'll call them **Pass-Through Lists** -- reflecting their use for "passing values through" to/from functions, and the idea that implicit deconstruction allows values to "pass through" the parentheses as if they weren't there.

Pass-through lists could possibly be used as a convenient notation for assigning a list of variables to a list of values like so:

{{< highlight java "linenos=false" >}}

let(firstName, lastName) = ("Albert", "Einstein");
{{< /highlight >}}


Nesting pass-through lists would also be pointless, given consistent application of the implicit deconstruction rule:

{{< highlight java "linenos=false" >}}

let a = ((1,2),(3,4))

// given implicit deconstruction produces the same result as
let a = (1,2)

// which produces the same result as
let a = 1
{{< /highlight >}}


### Pass-Through Lists and Parentheses

The implicit deconstruction rule makes the use of parentheses to construct pass-through lists consistent with the use of parentheses for grouping to override default operator precedence and associativity rules. For example, in the expression `a*(b+c)`, we use parentheses to override the default arithmetic operator precedence. We can look at this as actually creating a single-item pass-through list containing the value of `b+c`, which is then extracted with an implicit deconstruction and multiplied by `a`.

### Pass-Through Lists and Regular Lists

Since pass-through lists can't be referenced, our language probably needs an additional list type, perhaps constructed using the [] operator.

{{< highlight java "linenos=false" >}}

let fruit = ["applies","oranges","cherries"]
{{< /highlight >}}


## Synopsis of Pass-Through List Rules

Pass-through lists are created by placing one or more comma-separated values in parentheses.  All functions take a single pass-through list as their argument, and return a single pass-through list.

The values of pass-through lists can be accessed via deconstructing assignments, for example:

{{< highlight java "linenos=false" >}}

let (a,b) = ("a","b")
{{< /highlight >}}


If a pass-through list contains more values than are listed on the left-hand side of the deconstructing assignment, extra values are ignored.

{{< highlight java "linenos=false" >}}

let (a) = ("a","b")
{{< /highlight >}}


### Implicit Construction

Pass-through lists are implicitly constructed -- in other words, parentheses are assumed -- in any context where a pass-through list is expected, including...

...when passing arguments to functions, so:

{{< highlight java "linenos=false" >}}

f x

// is the same as
f(x)
{{< /highlight >}}


...when returning values from functions, so:

{{< highlight java "linenos=false" >}}

let inverse(x) = 1/x

// is the same as
let inverse(x) = (1/x)

{{< /highlight >}}


...and in any deconstructing assignments, so:

{{< highlight java "linenos=false" >}}

let(x) = 5

// is the same as
let(x) = (5)
{{< /highlight >}}


### Implicit Deconstruction

Implicit deconstruction happens wherever a pass-through list is accessed without an explicit deconstructing assignment, including...

...when constructing pass-through lists by enclosing values  in parentheses for overriding operator precedence associativity rules, so:

{{< highlight java "linenos=false" >}}

a*(b+c)

// is the same as
let(temp) = (b+c)
a * temp
{{< /highlight >}}


...when accessing function return values, so:

{{< highlight java "linenos=false" >}}

let y = f(x)

// is the same as
let (y) = f(x)
{{< /highlight >}}


...when pass-through lists of arguments are passed to functions.  So:

{{< highlight java "linenos=false" >}}

let inverse = x -> 1/x

// is the same as
let inverse = (x) -> 1/x
{{< /highlight >}}


## Summary

By requiring functions to return lists, we've made code more robust with respect to changes to function signatures.  The implicit construction and deconstruction rules remove the syntactic overhead from this.

These rules make pass-through lists into a kind of ephemeral type that cannot be referenced, requiring support of a more conventional list type in any language that uses pass-through lists.

Using the same semantics for passing/returning values to/from functions will allows a language designer to implement some useful new language features consistently on both sides of the interface to a function: named parameters (named return values), optional and default values (optional and default return values), type constraints, variable-length argument (and return value) lists, pattern matching, and nested deconstructing assignments.  I hope to explore some of these features in future posts.

Finally in another future post, I hope to discuss how adding partial application and a new feature I call *folded application* to a language, along with the implicit construction and deconstruction rules, result in a language where curried and un-curried versions of functions are functionally identical.