---
title: "Pass-Through Lists"
slug: "pass-through-lists"
image: assets/images/return-value-code-graphic.jpg
date: "2014-06-19T18:40:00-05:00"
weight: 20
aliases:
- /2014/06/19/pass-through-lists/

---

The *pass-through list* is a programming-language feature intended to make it easier for programmers to modify functions to return additional values without breaking backwards compatibility, in the same way it is easy to modify functions to take additional parameters without breaking backwards compatibility.  This is done by, in a sense, unifying the semantics of passing parameters to functions and returning values from functions.

## Problem

Suppose I have an `inverse` function that takes one number and returns one number, in some hypothetical untyped language:

{{< highlight java "linenos=false" >}}

func inverse(x) 
	return 1/x

println(2 * inverse(4))

{{< /highlight >}}


I cannot change this function to return a second value, without breaking existing code:

{{< highlight java "linenos=false" >}}

func inverse(x) 
	return (1/x, "the inverse of " ++ x)

// This doesn't compile!  
// inverse(4) returns a list, not a number.
println(2 * inverse(4))

{{< /highlight >}}


On the other hand, I can easily modify the function to *take* an optional second parameter:

{{< highlight java "linenos=false" >}}

func inverse(x, verbose=false) // 'verbose' is optional
	if(verbose) println("Calculating the inverse of " ++ x)
	return 1/x // return 1/x
{{< /highlight >}}


And code that depended on the old version of the function would not need to be modified:

{{< highlight java "linenos=false" >}}

println(2 * inverse(4)) // Still works!

{{< /highlight >}}

It's nice that I can modify `inverse` to *take* additional parameters without breaking existing code. And it would be nice if I could also modify it to *return* additional parameters without breaking existing code. This would promote backwards compatibility: the ability for old code to work with newer versions of libraries -- ignoring additional return values that it doesn't use.



## Proposed Solution

Here's a proposal for a language feature that makes additional return values backwards compatible. This proposal avoids adding additional syntactic complexity to the language using something I call implicit construction/deconstruction.

### 1. Functions Always Return Lists

In most languages, functions accept lists of arguments, but typically return single values.  But if functions always returned lists, additional values could always be added to the end of the list. Unfortunately, this would require extra code on the caller side to extract returned values from lists.  

But we could reduce that burden with a little syntactic sugar: allowing calling code to accept the values returned by a function using a **deconstructing assignment**:

{{< highlight java "linenos=false" >}}

// function that returns a list containing one number
func inverse(x)
	return (1/x) // return a list with 1 item

// receive the value from `inverse` using deconstructing assignment 
// the first element of the returned list is assigned to y
let (y) = inverse(4)
println(2 * y)
{{< /highlight >}}


Now we can modify a function to return multiple values, but the caller can choose to ignore extra values:

{{< highlight java "linenos=false" >}}

func inverse(x)
	return (1/x, "the inverse of " ++ x)

// ignore the second return value
let (y) = inverse(4)

// or use it if we want
let (z, explanation) = inverse(4)
{{< /highlight >}}


### 2. Implicit Deconstruction

But this doesn't completely eliminate the extra syntax burden: we still have to use a deconstructing assignment to receive the values returned by every function call. So a simple expression such as `2 * inverse(4)` would not be possible. We'd need to write:

{{< highlight java "linenos=false" >}}

let (result) = inverse(4)
println(2 * result)
{{< /highlight >}}


But this can be solved with an **implicit deconstruction** rule: if a list returned by a function is not explicitly deconstructed with a deconstructing assignment, then it is *implicitly deconstructed*, with all but the first value being ignored.

So we can now call `inverse` as if it returned a single value, and not a list:

{{< highlight java "linenos=false" >}}

// return a list with a number and a string
func inverse(x)
	(1/x, "the inverse of " ++ x)

// implicit deconstruction: just use the first value from inverse
println(2 * inverse(4))

{{< /highlight >}}


And now we have a language where it is easy to modify a function to return additional values, without breaking backwards compatibility.

### 3. Implicit Construction

Now, it's a tiny bit inconvenient for our language to force every function to explicitly return a list.

But we can solve this problem with an **implicit construction** rule, and say that a list is implicitly created in places where it is expected. So:

{{< highlight java "linenos=false" >}}

func inverse(x)
	return 1/x
{{< /highlight >}}


Is syntactically equivalent to:

{{< highlight java "linenos=false" >}}

func inverse(x)
	return (1/x)
{{< /highlight >}}


In other words, the parentheses around return lists are implicit.

### 4. Implicit Construction in Function Calls

Let's say that, in our language, functions always require a list of arguments, and parentheses are used for constructing lists. So the expression

{{< highlight java "linenos=false" >}}
inverse(4)
{{< /highlight >}}

actually constructs a list containing the value `4` and passes this as the argument to `inverse`.

But the implicit construction rule says a list is implicitly constructed where it is required. So we could make parentheses optional in function calls if there is only one argument:

{{< highlight java "linenos=false" >}}

inverse 4 // same as inverse(4)

{{< /highlight >}}


### 5. Implicit Deconstruction in Function Definitions

So functions in our language always accept and return a list of values. Now in many functional languages, functions always accept and return a *single* value.  Let's say that in our language, both of these are true: functions always accept and return a single value -- a list. In cases where that list only contains 1 item, implicit construction/de-construction simplifies the syntax by allowing you to construct and deconstruct the list implicitly, without parentheses.

Previously, we defined an inverse function that takes two arguments:

{{< highlight java "linenos=false" >}}

func inverse(x, verbose=false) // 'verbose' is optional
	...function body
{{< /highlight >}}

But we just said that in this language, functions only take a single list as an argument. So the above is actually syntactic sugar: the argument list is treated as a deconstructing assignment. The above is equivalent to the following:

{{< highlight java "linenos=false" >}}

func inverse(args)
	let (x, verbose) = args
	...function body
{{< /highlight >}}


## Pass-Through Lists are Not Regular Lists

Implicit construction/deconstruction means if we try to assign a list to a variable, we will only end up assigning the first value to the variable:

{{< highlight java "linenos=false" >}}

let myList = (1,2,3)
myList // evaluates to 1

{{< /highlight >}}


But rather than creating syntax for taking a reference to a list, we will instead define **pass-through lists** as a kind of ephemeral type that only exists in the compiler. It is a syntactic construct use for "passing" values to or returning values from functions. The name **pass-through list** also reflects the idea that values "pass through" the parentheses as if the parentheses weren't there.


Nesting pass-through lists would also be pointless, given consistent application of the implicit deconstruction rule:

{{< highlight java "linenos=false" >}}

let a = ((1,2),(3,4))

// given implicit deconstruction produces the same result as
let a = (1,2)

// which produces the same result as
let a = 1
{{< /highlight >}}


## Pass-Through Lists and Parentheses

Using parentheses as the syntax for constructing a pass-through lists just happens to make parentheses do other things we expect from them.

For example, we can now use parentheses for specifying the order of operations. The expression `a*(b+c)` creates a single-item pass-through list containing the value of `b+c`, which is then extracted with an implicit deconstruction and multiplied by `a`, resulting in the intended order of operations.

Implicit construction/destruction also provides a convenient notation for assigning a list of variables to a list of values like so:

{{< highlight java "linenos=false" >}}

let(firstName, lastName) = ("Albert", "Einstein");
{{< /highlight >}}


## Pass-Through Lists and Regular Lists

Since pass-through lists can't be referenced, a language that uses them probably needs an actual list type, perhaps constructed using square brackets instead of parentheses.

{{< highlight java "linenos=false" >}}

let fruit = ["applies","oranges","cherries"]
let x = fruit
x // evaluates to ["applies","oranges","cherries"]
{{< /highlight >}}

### Further Possibilities

Using the same semantics for passing/returning values to/from functions could further enable some useful new language features consistently on both sides of the interface to a function: named parameters (named return values), optional and default values (optional and default return values), type constraints, variable-length argument (and return value) lists, pattern matching, and nested deconstructing assignments.  I hope to explore some of these features in future posts.


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

func inverse(x)
	return 1/x

// is the same as

func inverse(x)
	return (1/x)

{{< /highlight >}}


...and in any deconstructing assignments, so:

{{< highlight java "linenos=false" >}}

let(x) = 5

// is the same as

let(x) = (5)
{{< /highlight >}}


### Implicit Deconstruction

Implicit deconstruction happens wherever a pass-through list is accessed without an explicit deconstructing assignment, including...

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

### Behavior of Parentheses

These rules mean parentheses work as expected for specifying order of operations...

{{< highlight java "linenos=false" >}}

a*(b+c)

{{< /highlight >}}

...or for multi-value assignments

{{< highlight java "linenos=false" >}}

let (a,b) = ("a","b")
{{< /highlight >}}



## Summary

By requiring functions to return lists, we've made code more robust with respect to changes to function signatures.  The implicit construction and deconstruction rules remove the syntactic overhead from this.

These rules make pass-through lists into a kind of ephemeral type that cannot be referenced, requiring support of a more conventional list type.

Using the same semantics for passing/returning values to/from functions could enable some interesting language features, such as named and optional parameters, to be implemented consistently on both sides of the function interface.


In my [next post](/implicit-currying-and-folded-application), I discuss how adding partial application and a new feature I call *folded application* to a language, along with the implicit construction and deconstruction rules, result in a language where curried and un-curried versions of functions are functionally identical.