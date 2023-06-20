---
title: "Functional Dependency Injection"
slug: functional-dependency-injection
image: assets/images/Syringe_052712.jpg
date: "2015-06-09T19:18:00-05:00"
weight: 40
aliases:
- /2015/06/09/functional-dependency-injection/

---

In this post I'll talk about dependency injection and IoC in functional programming languages, and propose a solution for achieving IoC with `given/inject` preprocessing.


The terms ***Dependency Injection*** and ***Inversion of Control*** tend to be used in OOP circles, though these concepts are applicable to virtually any language. 



## Dependency Injection for Dummies

A good summary of the definitions of these concepts is the first answer by Garret Hall on the stack overflow question <a href="http://stackoverflow.com/questions/6550700/inversion-of-control-vs-dependency-injection">Inversion of Control vs Dependency Injection</a>. <a href="http://www.warski.org/blog/2015/02/in-todays-post-oo-world-is-dependency-injection-still-relevant/">Adam Warski</a> has another good blog post on dependency injection in a post OO world.

But here's my simple definition:

**Dependency Injection** is when instead of hard-coding a dependency, you make the dependency a parameter. 

For example, when instead of doing something like this:

{{< highlight clojure "linenos=false" >}}
import someLibrary.makeRect
def makeSquare(sideLength)
	 makeRect(sideLength, sideLength)
{{< /highlight >}}

You do this:

{{< highlight clojure "linenos=false" >}}
def makeSquare(sideLength, makeRect)
	makeRect(sideLength, sideLength)
{{< /highlight >}}

I'm not using any particular programming language here.  Just trying to illustrate concepts.

Instead of calling this little technique *passing-of-dependencies-as-arguments*, we call it *dependency-injection*.  There you go.

## Too Many Arguments

If you are a consistent dependency-injector, you inject all dependencies, deferring all decisions about concrete implementations to the highest level of your program -- say the `main` function.  So:

{{< highlight clojure "linenos=false" >}}
import someLibrary.makeRect

def main()
  makeSomeSquares(makeRect)

def makeSquare(sideLength, makeRect)
	 makeRect(sideLength, sideLength)

def makeSomeSquares(makeRect) 
	[makeSquare(2, makeRect), makeSquare(4, makeRect)]
{{< /highlight >}}


So your have littered your code with `makeRect`s, passing it from main all the way down the call stack. This is ugly. Most people don't actually do this.

## Other Means of Inversion of Control

**Inversion of Control** is the more general principle we are striving for here.  IoC just means not hard-coding specific implementations for dependencies, but rather just specifying what is needed ("something that makes a rectangle"), and letting something higher up make a decision about specific implementations

Passing dependencies as parameters like this is just one way of achieving IoC. In the OO world, there are also IoC frameworks, service locators, the Template design pattern, and more.  But what about the FP world?

## `Given` and `Inject` Keywords

In a functional programming language where code is data, we don't need containers or design patterns or anything like that. We can just create something that modifies our code to do the dependency injection for us.

Let's define the `given` keyword, which is like `import`, but where you don't hard-code a specific implementation.

{{< highlight clojure "linenos=false" >}}
given makeRect // instead of: import someLibrary.makeRect

def makeSquare(sideLength)
	makeRect(sideLength, sideLength)

def makeSomeSquares
	[makeSquare(2), makeSquare(4)]
{{< /highlight >}}

In a typed language we'd want to specify the type of `makeRect`, but let's not worry about that now.

So we no longer pass `makeRect` explicitly to `makeSquare` or `makeSomeSquares`. Instead let's define an `inject` keyword for binding `given` dependencies to actual implementations.

{{< highlight clojure "linenos=false" >}}
def main =
  inject makeRect = someLibrary.makeRect
  makeSomeSquares
{{< /highlight >}}




Now we have two simple, complementary keywords -- `given` and `inject` -- for achieving inversion of control, without the burden of "manually" injecting dependencies into every function call all the way down the stack.

## `Given` and `Inject` Macros

`given` and `inject` can be thought of as pre-processor directives or macros, that cause your program to be transformed before it is evaluated.  For example in a Lisp-like language, we could define `GIVEN` and `INJECT` macros, and write our program like this:

{{< highlight clojure "linenos=false" >}}

(GIVEN [makeRect]
	def makeSomeSquares ..etc...)

(def main
	(INJECT [makeRect someLibrary.makeRect]
		makeSomeSquares))

{{< /highlight >}}

After the macro evaluation stage, we'd have a program where makeRect is explicitly passed as a parameter to `makeSquare` and `makeSomeSquares`, but we'd never have to touch that awkward code.

The `given` and `inject` syntax is just an alternative syntax for achieving the same thing.

## Conclusion

I don't need to explain the merits of inversion of control -- it's one of the pillars of good OO design.  However, it's often dismissed as non-applicable in a language with higher-order functions.  But as demonstrated above, manually injecting dependencies by passing functions as arguments can be cumbersome.

`given/inject` preprocessing allows you to inject your dependencies to achieve inversion of control without syntactic overhead or complexity, while respecting the principles of pure functional programming.