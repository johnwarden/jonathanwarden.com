---
title: "Procedures in a Pure Language"
slug: "procedures-in-a-pure-language"
series: ["Procedures in a Pure Language"]
image: assets/images/Pure.jpg
date: "2015-05-28T00:15:00-05:00"
series: ["Procedures in a Pure Language"]
weight: 32
aliases:
- /2015/05/28/procedures-in-a-pure-language/
---

## The Problem

The fact that you can write procedures, which produce side-effects, in Haskell, which is supposed to be a pure language, can be confusing.

I think the key to clearing up the confusion is to understand that most Haskell programs are actually programs that *produce* programs -- like preprocessors in CPP.  Conal Elliott's post <a href="http://conal.net/blog/posts/the-c-language-is-purely-functional">The C language is purely functional</a> explores this idea.

The Haskell language itself is pure.  When *evaluated*, Haskell expressions have no side effects, and are referentially transparent.

But the value returned by `main` in many Haskell programs is an `IO something` -- which is a *procedure* that can be executed and may have side effects.

If the GHC didn't have the built-in ability to take an `IO something` and compile it into an executable procedure, then a Haskell program could evaluate expressions all day long, but the resulting values would just disappear into the ether because the program could never output the results to STDOUT, because that is a side-effect.

Haskell has advantages over impure languages not because it <strong>removes</strong> the ability to write impure procedures, but because it <strong>adds</strong> the ability to write pure functions, guaranteed to have no side effects and to be referentially transparent. Many programs can and should be written with pure functions, which are easier to maintain, understand, and debug.  Often you only need impure procedures for a small part of the program, perhaps only the parts that actually writes output to STDOUT or a database.

Unfortunately, when you need to write impure-procedures in Haskell, there is a great deal of syntactic overhead that I don't think has to be necessary.

The benefits of Haskell's IO to me are are:

 - I can write pure functions.
 - I can also write procedures.
 - I can treat procedures as values.
 - I clearly see in my code when I am defining a procedure and when I am defining a pure function.
 - I clearly see in my code when a procedure is executed.


I'd like to see a language with those benefits, but with additional benefits:

- I can write procedures without the *syntactic overhead* of the IO Monad required in Haskell.
- I can *contain* procedures to only operate on specific objects, so that I can limit their effects to a *sandbox*.

## Proposed Solution

Suppose we start with a simple untyped language with an `->` operator for defining anonymous functions, and a `++` operator for concatenating strings.

{{< highlight java "linenos=false" >}}

f = (name) -> "Hello, " ++ name
f("Bill")
{{< /highlight >}}

Since this program has no procedures, it doesn't do anything other than produce the value "Hello, Bill" when evaluated.

Now let's add procedures:

{{< highlight java "linenos=false" >}}

main = (console) -> procedure
	console.println!("Hello, Bill")
{{< /highlight >}}

I have defined a function, `main`, which takes an argument named `console`, and returns a *procedure*.

The body of a `procedure` is a sequence of *imperatives*.  In this example there is a single imperative, `console.println!("Hello, Bill")`.  An imperative is to an expression what a function is to a procedure: both return values, but an imperative doesn't have to be a pure function call.

`console.println`, like `main`, is a function that returns a procedure.  The `!` operator says that this procedure should actually be *executed*, not just returned, at this point in the code.  Otherwise, the result of evaluating `main` would be a procedure that, when executed, just returns another procedure.

### Methods

`console.println` looks like what you'd call a method in OO. I could easily have written this as `println console`, but I like the `.` syntax here.  I'll call functions like `println` that can be called with `.` syntax `methods`.
 
### The "Apply and Execute" Operator

The `!` binary operator could be thought of as "apply and execute", because it applies a function to its arguments, and then execute the procedure that is returned.

You can also apply a function that returns a procedure to it's arguments without executing the procedure:

{{< highlight java "linenos=false" >}}

let greet = console.println("Hello, Bill")
{{< /highlight >}}

The `!` operator can also be used as a unary, postfix operator, which simply executes a procedure

{{< highlight java "linenos=false" >}}

greet!
{{< /highlight >}}

### Operations

Methods like `println`, that *return* procedures are called *operations*.

The `!` binary operator is used to *invoke* an operation by applying arguments to a function and then executing the procedure.

### Summary of Proposal

{{< highlight java "linenos=false" >}}

main = (console) -> procedure
	let greet = console.println("Hello, Bill")
	greet!

	// another way of doing the above.
	console.println!("Hello, Bill") 
{{< /highlight >}}

So `main` is a *pure function* that returns a *procedure*.  `println` is an *operation* -- a *method* that returns a *procedure*.  `println`, like all methods, is also a pure function, because simply *applying* it has no side effects.

`greet` is a procedure, the result of applying `println` to its arguments in the expression `console.println("Hello, Bill")`.

`greet!`, because of the presence of the `!` operator, is an *imperative*.

`console.println!("Hello, Bill")` is likewise an imperative.

### Summary of Definitions


- **Function**: takes arguments, never has effects.
- **Procedure**: takes no arguments, has effects when executed.
- **Method**: functions attached to objects (enabling polymorphism).
- **Operation**: method that produces a procedure.
- **Expression**: has no effects, consistent.
- **Imperative**: may have effects or be inconsistent.

## Conclusion

We have defined a language that, like Haskell, allows us to define pure functions, which themselves can produce procedures that can be executed.  The body of any function containing imperatives that execute procedures must be a procedure, just as in Haskell any function that uses a `bind` operation or `do` on an `IO something` must itself return an `IO something`.  But our language has first-class procedures instead of the `IO` monad, and the `!` operator instead of `do` or any of the `bind` operators.

Also just as in Haskell, "evaluating" the program has no side-effects.  It just produces a procedure which you can then execute.

Our language doesn't treat procedures as Monadic value as does Haskell.  After a program is evaluated there is no need for something that can be bound, `fmaped` over, or stuck in a `do` block, since all that you will ever do with this procedure is execute it.

Also by treating procedures differently from monadic values, it is even easier to see exactly when you are invoking procedures.  This will be helpful to a programmer striving to minimize unnecessary use of impure code


