---
title: "Implicit Currying and Folded Application"
slug: "implicit-currying-and-folded-application"
image: assets/images/currypufffold003.jpg
alias: http://jonathanwarden.com/2015/05/25/implicit-currying-and-folded-application/
date: "2015-05-25T21:05:00-05:00"
weight: 50

---

*Implicit currying and folded application* are language feature that render moot the distinction between curried and un-curried functions, allowing functions written in curried style to be called as un-curried functions and vice-versa.

## Quick Background: Currying

<a href="http://en.wikipedia.org/wiki/Currying">Currying</a> is the technique of translating a function that takes multiple arguments, or a tuple of arguments (an n-ary function) into a sequence of functions, each with a single argument.  For example:

{{< highlight clojure "linenos=false" >}}
// binary function (uncurried form)
let product = (x,y) -> x*y

// curried form of same function
let product = x -> y -> x*y
{{< /highlight >}}

The curried form of the function has to be invoked differently than the uncurried form:

{{< highlight clojure "linenos=false" >}}
(product 2) 4
// which, given left-associativity, is the same as
product 2 4
{{< /highlight >}}

Instead of

{{< highlight clojure "linenos=false" >}}
product(2,4)
{{< /highlight >}}


### Partial Application vs Implicit Currying

*Partial application* is the ability to pass only the first argument to a function taking two or more arguments, and get back a function taking the remainder of the arguments.  So you can write your function as an n-ary function, but call it as if it were curried.

Partial application is non-applicable in a language, like Haskell, where n-ary functions are not supported.  Functions with multiple arguments have to be defined in curried form.

The advantages to languages like Haskell, where all functions take only a single argument, have been thoroughly explored elsewhere. But these advantages would not be lost in a language that allowed functions to be defined *as if* they were n-ary.  It could be left to the compiler or interpreter to translate them automatically to curried form.  So:

{{< highlight clojure "linenos=false" >}}
let product = (x,y) -> x*y
{{< /highlight >}}

is syntactically equivalent to:

{{< highlight clojure "linenos=false" >}}
let product = x -> y -> x*y
{{< /highlight >}}

This is different from partial application, in that n-ary functions still don't exist.  You still can't pass multiple arguments to functions.

{{< highlight clojure "linenos=false" >}}
// won't work
product(x,y)
{{< /highlight >}}

You have to pass arguments one at a time as you do in Haskell:

{{< highlight clojure "linenos=false" >}}
product x y
{{< /highlight >}}


### Folded Application

So now we have a language where all functions take a single argument, and multiple-argument functions are either explicitly or implicitly re-written in curried form.  Now let's also say that, as a syntactic convenience, we allowed curried function to be invoked *as if* they actually allowed multiple arguments.  So:

{{< highlight clojure "linenos=false" >}}
 product(2,4)
 // is the same as
 (product 2) 4
{{< /highlight >}}

I call this *folded application*, because it involves implicitly doing a fold (or reduce) operation on the invisible *apply* operator, applying the function to the first argument, then applying the result to the second argument, and so on.  So:

{{< highlight clojure "linenos=false" >}}
product(2,4)
// is implicitly translated to
reduce apply [product,2,4]
{{< /highlight >}}


## Curried/Uncurried Equivalence

There are languages, like Haskell, where all functions take just one argument (which has advantages), and other languages where functions can take multiple arguments (which also has advantages).

Implicit currying + folded application together allows you to mix paradigms.  If you want to think of your functions as always taking single arguments, as in Haskell, you can, regardless of how the function is defined.  If you prefer to think of the same functions as taking a list of arguments, you can.  The distinction between a curried functions and it's equivalent n-ary function becomes moot.  So if you define:

{{< highlight clojure "linenos=false" >}}
let product1 = x -> y -> x*y
let product2 = (x,y) -> x*y
{{< /highlight >}}

Then all of the following are equivalent:

{{< highlight clojure "linenos=false" >}}
(product1 2) 4
(product2 2) 4
product1(2,4)
product2(2,4)
{{< /highlight >}}


## Difference from Partial Application

A language that allows n-ary functions and partial applications doesn't quite give you curried/un-curried equivalence.  It allows n-ary functions to be invoked as if they were curried.  But it does not allow curried functions to be invoked as if they were n-ary.  This asymmetry requires programmers to make a choice when defining their functions, and once the choice is made their options for using the function is limited.  Curried/un-curried equivalence eliminates this dilemma.

## Conclusion

Should languages mix paradigms?  I could understand if your instinct is that you should choose a paradigm and stick with it.  On the other hand, I think that there are times when both paradigms are useful.  The n-ary function paradigm can be a more familiar and intuitive for some software developers, and for certain problems.  Indeed I think it is more natural to think of a `product` as&nbsp;a function that takes two arguments, not as a function that returns a function.  On the other hand, there is great power in a language where, at a fundamental mathematical level underneath the syntactic sugar, all functions ultimately take a single argument.  A language that allows both paradigms offers the best of both worlds.