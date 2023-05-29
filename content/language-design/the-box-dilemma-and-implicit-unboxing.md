---
title: 'The "Box Dilemma" and Implicit Unboxing'
slug: "the-box-dilemma-and-implicit-unboxing"
image: assets/images/images.jpeg
alias: http://jonathanwarden.com/2013/06/28/the-box-dilemma-and-implicit-unboxing/
date: "2013-06-28T18:12:00-05:00"
aliases:
- /2013/06/28/the-box-dilemma-and-implicit-unboxing/
draft: true
---

The "**Box Dilemma**" is the name I've given to the situation where modifying an interface to provide additional information requires making a non-backwards-compatible change.

"**Implicit Unboxing**" is a possible solution to this problem. The compiler or interpreter would let you work with an object (the "box") containing some value you are interested in wrapped up with some other stuff you want to ignore, and write code as if the box and the other stuff didn't exist.

## When a Box is a Problem

Imagine you walk into a jewelry store that offers watch polishing services. The sign says "give us a watch, and we will give you back a polished watch." Fantastic. You hand the man behind the counter your fancy watch, in its original box.

**Jeweler**: <em>What's this?</em>

**You**: <em>A watch.</em>

**Jeweler**: <em>This isn't a watch. It's a box.</em>

**You**: <em>Uh…the watch is **in** the box.</em>

**Jeweler**: <em>I told you I polish watches, not boxes.</em>

**You**: <em>Are you kidding me?</em>

You expect human beings to have some common sense when dealing with boxes. <em>Just open the box!</em> But, for some reason, we don't have the same expectations for software. If you are using some sort of interface -- a library, a web service, a function, etc. -- and it expects a watch, and you pass it a box with a watch inside of it, it won't work. For example:

{{< highlight java "linenos=false" >}}

/* Polish interace takes a watch */
Watch polish(Watch watch)

/* Try to polish a box with a watch in it */
jeweler.polish(new Box(new Watch))

{{< /highlight >}}


If it's a strongly typed language, the compiler will respond as that jeweler did, with a type error, something like:

> 'polish' expects an argument of type Watch. But you passed it an argument of type Box[Watch] (you moron).

If the language is not strongly typed, it will act even less intelligently, and throw a disasterous runtime error only after the polishing wheel has ripped your cardboard box to shreds.  The same problem occurs if the Jeweler returns the watch in a box containing, but your code wasn't written to expect a box.

## Interface Breaking Changes

As a less allegorical example, suppose I have a function that uses some statistics to guess the language of a natural language string. The fairly unsophisticated function just returns a string with the name of the language, which is all I need. It's not always 100% confident of its guess, so it also returns a number representing a confidence percentage. The interface looks like this (in no particular programming language):

### Listing 1

{{< highlight java "linenos=false" >}}

interface LanguageGuess
  String language
  Float confidence

/* guessLanguage signature returns a LanguageGuess */
LanguageGuess guessLanguage(String)

{{< /highlight >}}


But I am not interested in the <em>confidence</em> field now. I wish the interface wasn't so complicated and I could just treat the return value as a string, and write code like this:

### Listing 2

{{< highlight java "linenos=false" >}}

String languageGuess = guessLanguage("oh la la")

println 
		languageGuess 
		+ ": " 
		+ (if(languageGuess == "French")
			"that's what I thought"
		  else
			"what the?!?!")

{{< /highlight >}}


Instead of like this:

### Listing 3

{{< highlight java "linenos=false" >}}

LanguageGuess languageGuess = guessLanguage("oh la la")

println languageGuess 
	+ ": " 
	+ (if(languageGuess.language == "French")
		"that's what I thought"
	   else
		"what the?!?!")

{{< /highlight >}}


You may be thinking <em>what an incredibly lazy programmer!</em>  You only need to add `.language` after `languageGuess` to "open the box" and access the string.  Is this so much work?

Well, no.  But my motivation is not saving keystrokes. Rather, it's that I don't want to break existing code.  You see the previous version of this function used to return just a string, until I realized some words exist in many languages, and that my previous thinking about this language identification problem was over-simple.  So I changed the interface to return an object containing both a language and a confidence %.  But now all the many people who wrote code against my old interface have to change it.  This is a real problem.

## The "Core Value" and the "Box"

With many data structures, it's easy to identify the **core** value. It's the payload, the meat, the fancy watch, the T type parameter, the thing that the "container" exists to contain.

Changing an interface that used to return a value of type T, to return some sort of object **containing** a value of type T, is one of the most common and expensive changes coders make, one I beleive accounts for measureless software development costs annually.

But this sort of change is a necessary part of the natural evolution of code. First of all, it's easier to start thinking about your program logic in terms of these core functions and types, and initially ignore ancilliary stuff like logging, error handling, etc. Second, you like all of us have a limited IQ and simply won't think of all the aspects that should be worked into your design up front (and gosh darnit I'm sick of languages making my feel like I should). And finally, requirements and designs change!

## A Box Monad?

Many of the millions of attempts to explain what a Monad is use a Box as an analogy. Now, Monads go beyond simply a value contained in a box. They can deal with future values, or multiple values, or possible or probable values, and so on.

Rather, the "Box Dilemma" problem is just one problem that Monads may help solve.

One of the things that gets functional programmers excited about Monads, is that they kind of promise to let you write code in terms of core values, and let the Monad handle everything else (in this case, the box) orthogonally. But you have to write your code just a little differently so the compiler or interpreter knows to work the Monad magic. For example if I made the type of objects returned by `guessLanguage` into Monad instances, I could use 'do' syntax (a la Haskell) or for comprehensions (a la Scala), like this:

### Listing 4 (Scala-like for comprehension)


{{< highlight java "linenos=false" >}}

for
 language <- guessLanguage("oh la la")
 yield println language + ": " + (if(language == "French")
  "that's what I thought"
 else
  "what the?!?!")

{{< /highlight >}}

This is, uhh, great. It makes my code more complicted, and it hasn't helped me achieve my goal of not changing Listing 2.

On the bright side, <em>this</em> code is now more robust, because now its possible to add additional aspects to the objects returned by guessLanguage, such as possible errors, or multiple values, or I could even make it run asynchronously, without changing the code in Listing 4 (although combining all these aspects into one object would be challenging). Now that is kind of neat, and part of why Monads are all the rage right now.

## Implicit Unboxing

But, I'm not satisfied. I don't want to change Listing 2 one bit. My core logic was expressed perfectly clearly and succinctly in listing 2. I want to **completely** de-couple my core logic from ancilliary aspects of my program like confidence and possible error and logging.

Why can't the compiler look at my code in listing 1, and automatically just do the right thing? It could automatically convert listing 1 to listing 2 (if we wanted to use a Monad for the implementation). Or it could simply rewrite the code to extract the core value explicitly.

Now, one reason I might not want this, is that in some cases I **do** want access to the other stuff in the box. So I'd need a way to indicate these cases to the compiler. One possibly solution may be to use type annotations (assuming a typed language) to indicate either the simpler type or the whole box. If I want to access the whole box, my code might look like this (the declaration `LanguageEstimate languageEstimate` on line 1 is the key).

### Listing 5

{{< highlight java "linenos=false" >}}

/* Treat the return value as a language estimate */
LanguageEstimate languageEstimate = guessLanguage("quiero taco bell")

println "The language is " + languageEstimate.language + " with confidence " + languageEstimate.confidence

{{< /highlight >}}


Otherwise, I would have declared `String languageEstimate` and my code would look more like listing 2.

## Creating Boxes

Suppose in our language, we could create objects using syntax like this:

{{< highlight java "linenos=false" >}}

(core: "French", confidence: 30)

{{< /highlight >}}


And to save some keystrokes, lets say that we can indicate the "core" value just by listing it first.

{{< highlight java "linenos=false" >}}

("French", confidence: 30)

{{< /highlight >}}


The compiler would ignore everything but the core value by default

{{< highlight java "linenos=false" >}}

String language = ("French", confidence: 30)
println language

{{< /highlight >}}

**Output**
	
	French

But if I use a type annotation compatible with the box, it would give me access to the box and all the extra stuff in it.

{{< highlight java "linenos=false" >}}

LanguageEstimate estimate = ("French", confidence: 30)
println "Language is " + estimate.language + " with confidence " + estimate.confidence

{{< /highlight >}}


Structural typing and a syntax for pattern matching / deconstructing assignments could come in handy here.

{{< highlight java "linenos=false" >}}

(language, confidence:) = ("French", confidence: 30)
println "Language is " + language + " with confidence " + confidence

{{< /highlight >}}


Now, if I originally wrote my function to only return a string:

{{< highlight java "linenos=false" >}}

estimateLanguage = string ->
	String language = /* do some analysis */
	Float confidence = /* do some analysis */
	return language
language

{{< /highlight >}}


I could modify it return confidence as well without breaking any existing code, just by returning an object with a core value.

{{< highlight java "linenos=false" >}}

LanguageEstimate = type(String, confidence:Float)
estimateLanguage = s ->
	String language = /* do some analysis */
	Float confidence = /* do some analysis */
	/* return language and confidence*/
(language, confidence: confidence)

{{< /highlight >}}


And again, this change wouldn't break any existing code: listing 2 would work for either version of this function.

## Possible Applications

Since clearly defining the "Box Dilemma" in my own mind, and understanding what seems like a plausible solution, I have noticed countless instances where this would be immensely useful.

For example, it would provide an interesting way of supporting macros in a language. In an expression such as f(g(x)), the result of evaluating g(x) would normally be passed to f. What if, instead, f received a box that contained the value of g(x) as the core (evaluated lazily), but also contained a data structure representing the parsed expression g(x) itself? If you are writing a regular function, you would ignore the box and just process the value. If you are writing a macro, you could access the box and manipulate the expression itself.

As another example, implicit unboxing can allow you to sneak in "extra" information in all sorts of places where normally it wouldn't fit. For example, suppose I have a `println` function defined for strings, which are just lists of characters. And suppose I have defined constants for text formatting such as color and style. I could create strings that have format annotations, and pass those strings to functions that don't know anything about format annotations, without breaking anything.

{{< highlight java "linenos=false" >}}

regularString = ['a','b','c']
colorfulString = [
	('a', color: red)
	('b')
	('c', color: blue, bold: true)
]
println regularString
println colorfulString

{{< /highlight >}}

**Output**

	abc
	abc

But other functions that know about text formatting could deal with them.

{{< highlight java "linenos=false" >}}

println colorfulString map c ->
	(char, color:) = c
	if(defined color) "(" + color.name + " " + char + ")" else char

{{< /highlight >}}

**Output**

	(red a)b(blue c)

## Conclusion

A language that supported implicit unboxing would open up all sorts of possibilities, where new information could be added to interfaces were previously we wouldn't have dreamed of it because of how much complexity it would add to the interface, and how much existing code it would break. It would allow functions to return error, debug, type and contract, annotations, and all sorts of other information that can be ignored by code that doesn't need it. But most importantly, as you change your interface due to the natural evolution of your understanding of the problem and your requirements, you are much less likely to break existing code.