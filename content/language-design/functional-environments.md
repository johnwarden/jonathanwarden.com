---
title: "Functional Environment Variables"
slug: functional-environments
image: assets/images/bea6047db6.jpg
alias: http://jonathanwarden.com/2015/06/11/functional-environments/
date: "2015-06-11T15:19:00-05:00"
weight: 40
aliases:
- /2015/06/11/functional-environments/
---

Global environment variables violate a core principle of functional programming.  For example, this is not very acceptable in the FP world:

{{< highlight java "linenos=false" >}}
def hello = 
  if(locale == 'en.US') 
    "hello"
  elsif(locale == 'fr.FR')
    "bonjour"
{{< /highlight >}}


`locale` shouldn't just *be* there as a global.  In an pure functional language it should be passed as a parameter, according to the the principle of <a href="http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)">referential transparency</a>.


{{< highlight java "linenos=false" >}}
def hello(locale) = 
  if(locale == 'en.US') 
    "hello"
  elsif(locale == 'fr.FR')
    "bonjour"
{{< /highlight >}}

But this means you also have to pass `locale` to every function that calls `hello`.

{{< highlight java "linenos=false" >}}
def greet(name, locale) =
   hello(locale) ++ ", " ++ name
{{< /highlight >}}

Which means you have to pass `locale` to every function that calls `greet`, and so on. Turtles all the way down.


## `Given/Inject` Preprocessing


In my post on <a href="http://jonathanwarden.com/2015/06/09/functional-dependency-injection/">functional dependency injection and inversion of control</a>, I discuss a solution for passing dependencies without this syntactic overhead, using `given/inject` statements and code pre-processing.

We usually think of "dependencies" as libraries or services.  But anything your code depends on can be thought of as a dependency.  In any case, we can use `given` to thread `locale` through our code without the semantic overhead.

{{< highlight java "linenos=false" >}}

given locale

def greet(name) =
   hello() ++ ", " ++ name

def hello() = 
  if(locale == 'en.US') 
    "hello"
  elsif(locale == 'fr.FR')
    "bonjour"

{{< /highlight >}}

The `given` statement causes:

<ul>
 	<li>`greet` and `hello` to be rewritten in the pre-processing stage to accept an invisible `locale` parameter</li>
 	<li>`greet` to pass the `locale` parameter in its call to `hello`.</li>
</ul>

Now other functions can `inject` a value for `locale`. The `inject` statement causes locale to be passed as a parameter to every subsequent function call in the code block that needs it.

{{< highlight java "linenos=false" >}}
def main(environment) = 
    inject locale = environment.locale
    greet("Everybody")
{{< /highlight >}}

Now we have the convenience of what kind of looks like a global variable.  But it is implemented in such a way that the core principles of FP -- no side-effects and referential transparency -- are respected.


## Referential Transparency


You might argue that, because you don't actually see these parameters literally being passed in your code, `given/inject` violates referential transparency.

But the referential transparency rule simply requires that an expression can be replaced with its value without changing the output of the program.  The value of an expression involving a `given` simply depends on that variable being bound to a value. Once bound, the expression can be evaluated, and it will be interchangeable with its value.

Referential transparency is not about syntax: it's about having well-defined, reproducible behavior that is determined entirely by inputs.  There is nothing non-FP about using macros and pre-processors to save some keystrokes.

## Clarity

Some programmers might fear that bugs and confusion may arise when programmers don't realize that their program behavior depends on an environment variable.  But if even indirect dependencies on environment variables must be declared with `given` statements, it should be just as clear to a programmer reading the code what is going on.

## Preprocessing vs Monads

Another purely functional approach to environment variables is the use of the Reader Monad. But this requires a great deal more syntactic overhead than `given/inject`.  The signature of every function must be modified to return a Reader, and every function call involves a bind.  Code pre-processing with given/inject does this work for you.

## Conclusion

`given/inject` preprocessing lets you use environment variables in a purely functional way without the work and complexity of threading them as parameters throughout your code.