---
title: "Functional Structural Types"
slug: "functional-structural-types"
image: /assets/images/Pres12_WTA.png
alias: http://jonathanwarden.com/2013/07/07/functional-structural-types/
draft: true

---

<span style="font-size: 13px; line-height: 19px;">In this post I'll introduce the idea of a **Functional Structural Type**, which max properties of Type Classes and Structural Types.</span>

Structural types and Typeclasses are similar in that they both allow you to define arbitrary new *supertypes* for existing types. They are both defined very pragmatically, in terms of what you can *do* with them -- as long as certain members or functions are defined for the type it can be an instance of the new supertype.

Below I'll show how the same problem would be solved either with a Structural Type in Scala, or a Typeclas in Haskell. I will then introduce the Functional Structural Type, and show how it would work in both languages.

### Duck Example

Suppose I have types CartoonDuck and RubberDuck, which both have 'quack' methods. But due to a design oversight, they don't implement a common "Duck" supertype.

<a href="http://jonathanwarden.com/wp-content/uploads/2013/07/duck-library-11.png"><img class="alignnone size-medium wp-image-242" alt="duck library 1" src="http://jonathanwarden.com/wp-content/uploads/2013/07/duck-library-11-300x121.png" width="300" height="121"></a>

If I can't or don't want to modify the implementaitons for these types, then I can use either Structural Types or Typeclasses to go ahead and create a "Duck" type "after the fact". I can make both CartoonDuck and RubberDuck implement this type, without modifying either of their definitions! Cool!

<a href="http://jonathanwarden.com/wp-content/uploads/2013/07/duck-library-21.png"><img class="alignnone size-medium wp-image-243" alt="duck library 2" src="http://jonathanwarden.com/wp-content/uploads/2013/07/duck-library-21-300x172.png" width="300" height="172"></a>

## Structural Type Example

Here's how that would look in scala.

### Listing 1

:scala:
/* Create a Duck interface after the fact, using a structural type definition */
type Duck = {def quack: String}

/* Write a function that operates on ducks */
def describeDuck(duck: Duck) = "A duck that says " + duck.quack

/* I can call that function on cartoon ducks and rubber ducks! Holy crap! */
describeDuck(new CartoonDuck())
describeDuck(new RubberDuck())

## Typeclass Example

It's somewhat more complicated with Type Classes in Haskell

### Listing 2

:haskell:
/* Create a Duck interface after the fact, using a typeclass */
class Duck d where
quack' :: d -&gt; String

/* Make some things that can quack instances of Duck */
instance Duck CartoonDuck where
quack' = quack

instance Duck RubberDuck where
quack' = quack

/* Write a function that operates on ducks */
describeDuck :: Duck d =&gt; d -&gt; String
describeDuck d = "A duck that says " ++ quack' d

/* I can call that function on cartoon ducks and rubber ducks! Holy crap! */
main = do
print $ describeDuck CartoonDuck
print $ describeDuck RubberDuck

## A Hybrid

Structural types are defined in terms of *what methods are defined for them*. Typeclasses are defined in terms of *what functions are defined for them*

Structural types are more flexible than typeclasses in a way. Typeclasses require you to explicitly declare specific types to be instances of the typeclass before you can use it.  With a structural type you can just use it.

A "Functional Structural Type" would combine the two. Like a typeclass, it would be defined in terms of what functions were defined over it. But you wouldn't need to explicitly create instances.

### Functional Structural Types in Haskell

This is how a functional structural type might look in Haskell:

### Listing 3

:haskell:
/* Create a Duck interface after the fact, using a typeclass */
structural class Duck d where
quack :: d -&gt; String

/* Write a function that operates on ducks */
describeDuck :: Duck d =&gt; d -&gt; String
describeDuck d = "A duck that says " ++ quack d

/* I can call that function on cartoon ducks and rubber ducks! Holy crap! */
main = do
print $ describeDuck CartoonDuck
print $ describeDuck RubberDuck

Notice that, in addition to dropping the instance declarations, I can refer to  `duck`, instead of `duck'`, since a structural type just re-uses existing functions instead of defining new functions.

### Functional Structural Types in Scala

A Functional Structural Type in Scala would be useful if, when I created my CartoonDuck and RubberDuck libraries, I implemented `quack` as a function, not a method. It would allow me to treat anything that has a `quack` function as an instance of Duck.

Definitions of functional structural types would look almost exactly like definitions for regular structrural types. The code below looks almost exactly like Listing 1, except that I use the `typeclass` keyword instead of the `type` keyword, and I invoke `quack` as a function call instead of a method call.

### Listing 4

:scala:
/* Create a Duck interface after the fact, using a structural type definition */
typeclass Duck = {def quack: String}

/* Write a function that operates on ducks */
def describeDuck(duck: Duck) = "A duck that says " + quack(duck)

/* I can call that function on cartoon ducks and rubber ducks! Holy crap! */
describeDuck(new CartoonDuck())
describeDuck(new RubberDuck())
typeclass Duck = { def quack: String }

### One Step Further

It seems that Functional Structural Types offer all the benefits of typeclasses, and could replace typeclasses in a functional programming language.

The **Uniform Access Principle** states that "All services offered by a module should be available through a uniform notation."  I think that OO languages tend to violate this principle by using separate notation for method access and function calls.  E.g. the following have different semantics:

:haskell:
duck.quack /* Method Invocation */
quack(duck) /* Function Application */

Functional languages such as Haskell offer one notation -- function application.

But I there's no reason that a OO-functional hybrid language like Scala couldn't also unify these two modes of access.  `duck.quack` and `quack(duck)` would both indicate function calls, but any functions that are not defined in the current scope would have a `default` implementation that would invoke the method of the same name.  The truck would be making all of this typesafe.  But if you could solve that problem, then you would have an object-functional language with true Uniform Access, and you could replace structural types with functional structural types.