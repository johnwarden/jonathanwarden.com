---
title: "Effects without Side-Effects"
slug: effects-without-side-effects
image: assets/images/9d80511470b05582fdb13329de734bda_400x400.png
date: "2015-06-11T20:32:00-05:00"
series: ["Procedures in a Pure Language"]
weight: 30
aliases:
- /2015/06/11/effects-without-side-effects/
---

In my post on [Procedures in a Pure Language](/procedures-in-a-pure-language), I discuss how even pure functional languages can be used to create procedures that have effects, and how that is how things should be. I propose a little language where these impure procedures can coexist with pure functions in a way that makes the line between pure and impure very clear.

In this post, I propose adding a stricture to this language that ensures that, while procedures can have effects, they cannot have *side-effects*.

Here's a quick review of a simple program in this language.

{{< highlight java "linenos=false" >}}

main = (console) -> procedure
	let greet = console.println("Hello, Bill")
	greet!

{{< /highlight >}}


`println` and `main` are functions which, given a `console` argument, return procedures, which can be executed with the `!` operator.  Let's call such procedures, which are bound to the object on which they operate, **methods**.

## Containing Effects

Now let's say add this rule to our language: procedures can only execute methods on objects passed to them as arguments.

So procedures have no ability to *directly* reference or execute any other procedure: there are no built-in procedures like `println`, no global objects like Javascript's `window`, and no ability to directly import services or objects like Java's `System.out`.

Whoever runs the `main` procedure above can be certain it will have no effects outside of those that can be achieved by invoking methods on `console`. Since the caller has complete control over these methods, any effects of `main` are completely *contained*.

So these procedures can have *effects*, but since those affects are contained, they cannot have *side-effects*.

## "Impure" Inversion of Control

So a program can't actually *do* anything unless it is provided with an object on which it can invoke methods.  To OO programmers this sounds like dependency injection or inversion of control.

We can use [functional dependency-injection](/functional-dependency-injection) to achieve inversion of control in this language without the syntactic overhead of manual dependency injection.

{{< highlight java "linenos=false" >}}

given console
main = procedure
	console.println!("Hello, Bill")
{{< /highlight >}}

## Procedures Inside Functions

Since effects are contained, a function can be pure and still use impure functions in its implementation! For example:

{{< highlight java "linenos=false" >}}


greet = (name) ->
	mutable output = []
	output.push! "Hello, "
	output.push! name
	return output.join("")
{{< /highlight >}}

`greet` creates a locally-scoped mutable object, `output`, and manipulates it -- thereby producing effects. But those effects are contained to the local variable.

A functions may be implemented using temporary internal stateful computations like this and still be pure if these states cannot affect the caller or the outside world.

## Sandboxing

Since any effects of executing procedures are contained to objects passed to those procedures, we can sandbox their effects.

Presumably, when we run the above program in our hypothetical language, the interpreter will by default pass the a real `console` object that will actually print to standard output. But let's say we have the ability to create simple mutable objects that look someting like this:

{{< highlight java "linenos=false" >}}

mutable mockConsole = object
	output: [] # empty list
	println: (message) -> procedure
		output.push!(message)

{{< /highlight >}}

Now we can pass a mock console to `main`:

{{< highlight java "linenos=false" >}}

main = (console) -> procedure
	console.println!("Hello, Bob")
main!(mockConsole)
mockConsole.output // ["Hello, Bob"]
{{< /highlight >}}

Since all services that the `main` function might use to have effects (Network, Filesystem, etc.) must be passed to it, it makes commandline scripts written our language easy to test.

## Conclusion

Requiring inversion of control for all dependencies on services that can be used to have effects, gives the caller of the procedure complete control over its effects: effects without side-effects.