---
title: "Procedures in a Pure Language"
slug: "procedures-in-a-pure-language"
series: ["Procedures in a Pure Language"]
image: assets/images/Pure.jpg
alias: http://jonathanwarden.com/2015/05/28/procedures-in-a-pure-language/
date: "2015-05-28T00:15:00-05:00"
series: ["Procedures in a Pure Language"]
weight: 32
aliases:
- /2015/05/28/procedures-in-a-pure-language/
---
The fact that you can write procedures, which produce side-effects, in Haskell, which is supposed to be a pure language, can be confusing.

I think the key to clearing up the confusion is to understand that most Haskell programs are actually programs that <em>produce</em> programs -- like preprocessors in CPP.  Conal Elliott's post <a href="http://conal.net/blog/posts/the-c-language-is-purely-functional">The C language is purely functional</a> explores this idea.

The Haskell language itself is pure.  When <em>evaluated</em>, Haskell expressions have no side effects, and are referentially transparent.

But the value returned by <code>main</code> in many Haskell programs is an <code>IO something</code> -- which is a <em>procedure</em> that can be executed and may have side effects.

If the GHC didn't have the built-in ability to take an <code>IO something</code> and compile it into an executable procedure, then a Haskell program could evaluate expressions all day long, but the resulting values would just disappear into the ether because the program could never output the results to STDOUT, because that is a side-effect.

Haskell has advantages over impure languages not because it <strong>removes</strong> the ability to write impure procedures, but because it <strong>adds</strong> the ability to write pure functions, guaranteed to have no side effects and to be referentially transparent.  The majority of many programs can and should be written with pure functions, which are easier to maintain, understand, and debug.  Often you only need impure procedures for a small part of the program, perhaps only the parts that actually writes output to STDOUT or a database.

Unfortunately, when you need to write impure-procedures in Haskell, there is a great deal of syntactic overhead that I don't think has to be necessary.

The benefits of Haskell's IO to me are are:
<ul>
 	<li>I can write pure functions.</li>
 	<li>I can also write procedures.</li>
 	<li>I can treat procedures as values.</li>
 	<li>I clearly see in my code when I am defining a procedure and when I am defining a pure function.</li>
 	<li>I clearly see in my code when a procedure is executed.</li>
</ul>
I'd like to see a language with those benefits, but with additional benefits:
<ul>
 	<li>I can write procedures without the <em>syntactic overhead</em> of the IO Monad required in Haskell.</li>
 	<li>I can <em>contain</em> procedures to only operate on specific objects, so that I can limit their effects to a <em>sandbox</em>.</li>
</ul>
I think such a language is possible.  Anyone want to help me create it it?