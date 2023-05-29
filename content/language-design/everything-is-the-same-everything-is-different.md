---
title: "Functional Equality: Everything is the Same, Everything is Different"
slug: "everything-is-the-same-everything-is-different"
image: assets/images/Aristotle-1.jpg
date: "2015-05-16T22:22:00-05:00"
weight: 10
math: true
aliases:
- /2015/05/16/everything-is-the-same-everything-is-different/
- /everything-is-the-same-everything-is-different

---

or

# When 2+2 is not 4

In this post, I'll argue the merits of a *functional programming* language where the equality operator respects the principle of *functional equality*.

This principles states that, if `a` and `b` are equal, then there should exist no function `f` for which `f(a) != f(b)`.

Many, if not most, FP languages violate this principle.

### Does 2+2 = 4?

The answer to this quintessentially trivial question really depends on *what do you mean by equal!?*

Let's say I define `a` and `b` in a Haskell program like so:

{{< highlight java "linenos=false" >}}
let a = 2+2
let b = 4.0
{{< /highlight >}}

Does `a` equal `b`? If the question is whether they evaluate to equal points on the number line, then the answer is yes. But in other ways, `2+2` obviously is not the same as `4.0`. One is an int, the other is a float.

But do these differences matter?

I propose that, when defining equality, what matters to programmers is whether two values are *interchangeable*. If `a` is really equal to `b`, you should be able to replace `a` with `b` anywhere in your code and nothing in your output would change at all. In other words, this statement should never be true:

{{< highlight java "linenos=false" >}}
(a == b) && (f(a) != f(b))
{{< /highlight >}}

This concept is related to the FP concept of <a href="http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)">referential transparency</a>: an expression is referentially transparent if it can be replaced with its **value** without changing the behavior of the program.

But shouldn't we also expect that any value can be replaced with an **equal value** without changing the behavior of the program?

For an FP language to really respect the spirit of referential transparency, it should respect functional equality.

### It's Easy to Get Equality Wrong

The strict requirements of functional equality can be easily broken. In the Haskell case, the following expression is true, in violation of the principle of referential transparency:

{{< highlight java "linenos=false" >}}
(a == b) && (show(a) != show(b))
{{< /highlight >}}

Because `show(a)` will be "4", and `show(b)` will be "4.0", the two values are not functionally equal.

### Violating Functional Equality Creates Problems

The user of your class my assume that two objects that are equal behave in the same way and are therefore totally interchangeable, when they are in fact not.

Suppose a developer writes a program that plots data on charts, placing labels with the numeric value next to each point.

She may be encounter a situation where the same chart sometimes includes a decimal point in the labels, and sometimes doesn't.  She is perplexed when she discovers that *equal inputs produce different outputs*, which her intuition says should not be possible in a functional programming lgnguage.  Only after spending considerable time debugging does she realize that the inputs, while passing the `==` test, are actually different, because one contains floats and the other contains integers.  She'll then realize that she needs to rethink her definition of "equality".

### Equality Across Types

So `2+2 == 4.0` should evaluate to false?

That would be a problem as well.  It would certainly be non-intuitive for many programmers, and could easily lead to very frustrating debugging sessions.

But I would say that, if you want a language where ints and floats act differently, then they should not even be comparable.  The programmer should be required to explicitly convert them into a common type before comparing.  The expression `2+2 == 4.0` should produce a compile-time error.  You should be required to write something like `(float) 2+2 == 4.0`, making it very clear what you mean by "equal".

I would also argue that different types for ints and floats may not be necessary in modern languages -- and indeed most dynamic languages these days make no distinction.

### Summary

The *functional equality* principle allows `a` and `b` to pass the equality test only if it is never true that `f(a) != f(b)` for any f.

With a language that enforces functional equality, programmers will still sometimes fail to really think through what they mean by "equality", and be surprised when two values they expect to be equal are not.  But I think this is okay -- it will force programers to more concretely understand and define what exactly they mean by equality, and to write more clear and correct code.
