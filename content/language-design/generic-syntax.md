---
title: "Generic Syntax"
slug: "generic-syntax"
image: /assets/images/generic-syntax5.png
alias: http://jonathanwarden.com/2013/06/28/generic-syntax/
date: "2013-06-28T21:50:00-05:00"
---
Is it really necessary to invent a new syntax, complete with a new grammar and parser, for every new programming language?  Is there a generic syntax that can conveniently express the semantics of many languages?  Since all languages, whatever the syntax, are eventually parsed into an abstract syntax tree, couldn't we just use a general language for expressing abstract syntax trees?
<h1>XML and LISP</h1>
XML was designed to be generic enough to express data and concepts from virtually any domain.  So the AST of, say, a Java program could be represented in XML:
<h3>Java</h3>
[ruby]
public Integer sum(Integer x, Integer y) {
return x+y
}

[/ruby]
<h3>XML</h3>
[xml]
&lt;method name='sum' returnType='Integer' public='true'&gt;
&lt;parameters&gt;
&lt;parameter name='x' type='Integer'/&gt;
&lt;parameter name='y' type='Integer'/&gt;
&lt;/parameters&gt;
&lt;body&gt;
&lt;return&gt;&lt;plus arg1='x' arg2='y'/&gt;&lt;/return&gt;
&lt;/body&gt;
&lt;/method&gt;

[/xml]

Obviously, nobody would actually want to write their Java programs in XML -- but bear with me.  Once you have a data structure representing your Java program, your program is now data that can be manipulated before it is finally converted to bytecode, for example via an XSLT transformation.  You can now extend Java semantics with <strong>macros</strong> -- functions that act on your program itself.  Macros are one of the Lisp programmer's secret weapons, and part of the reason Lisp programmers are always pointing and laughing at the rest of us.

You could also represent a Java AST in JSON, but it would be even more unwieldy than XML:

[javascript]
{
head: "method",
name: "sum",
returnType: "Integer",
public: true,
parameters: [
{
head: "parameter",
name: "x",
type: "Integer"
},
{
head: "parameter",
name: "y",
type: "Integer"
}
],
body: [
{
head: "return",
body: {
head: "+",
arguments: ["x","y"]
}
}
]
}

[/javascript]

You could also represent your Java AST in Lisp, since Lisp is actually built on a generic syntax for expressing nested data structures, which is what an AST is.  This is why there are so many domain-specific languages in the Lisp world.  Your Java program, if represented as a Lisp expression, would look something like this:

[java]
'(method Integer sum :public true [(Integer arg1) (Integer arg2)] (
(return (+ arg1 arg2))
))

[/java]

Since this is quoted data, there are no pre-defined semantics.  It can represent anything -- in our case a Java program.
<h1>Improving on LISP</h1>
The above Lisp code is a lot more terse than the XML or JSON.  But even so, but I imagine few people will want to write their Java programs in Lisp.
<h2>Cleaner, More Generic</h2>
But what if we could further simplify the syntax and take Lisp out of the equation?  We could:
<ul>
 	<li>First, eliminate the quote operator, by dropping the assumption that every expression represents a functional program by default.  This makes the syntax truly generic, more like XML.</li>
 	<li>Second, eliminate unnecessary parentheses.  Use whitespace indent/outdent to indicate open/close parentheses.</li>
 	<li>Third, use infix notation for symbols like +</li>
</ul>
Our syntax now looks like this:

method Integer sum :public true [Integer x, Integer y]
return (x+y)

This is  pretty terse and readable.  In fact, I think it compares in brevity and clarity with the original Java:

[java]
public Integer sum(Integer x, Integer y) {
return x+y
}

[/java]
<h2>The Parsed Data Structure</h2>
Here's how a generic syntax expression might be parsed into JSON:
<ul>
 	<li>Comma-separated lists are converted to arrays.</li>
 	<li>Other sequence are converted to maps containing:
<ul>
 	<li>a <code>root</code> (the first element)</li>
 	<li>any key-value pairs (<code>public</code> in our example)</li>
 	<li>a <code>body</code> (containing the rest of the list)</li>
</ul>
</li>
</ul>
So this expression:

[java]
method Integer sum :public true [Integer x, Integer y]
return (x+y)

[/java]

Would be parsed into this JSON structure:

[javascript]
{
"head": "method"
"public": true
"body": [
"Integer",
"sum"
[
{
"head": "String"
"body": ["x"]
},
{
"head": "String"
"body": ["y"]
}
],
[
{
"head": "return",
"body": [
{
"head": "+",
"body": [
"x",
"y"
]
}

]
}
]
]
}

[/javascript]

Your compiler/interpreter could take this object and interpret it in a way that makes sense.  For example, it would be quite straightforward to write an interpreter that applied some transformations or some macros and then converted this data structure into Java.
<h1>Other Language Examples</h1>
Before showing examples of using generic syntax to represent the ASTs of programs in other languages, I'll add one more element to the syntax.  Sandwiching a token between colons like <code>:this:</code> makes that token an infix operator.  And I'll say all infix operators are right-associative with equal precedence.

Notice how natural the below programs look using generic syntax.
<h3>Generic C</h3>
[c]
include "iostream"

function int main []
cout &lt;&lt; "Hello, World!"
return 0

[/c]
<h3>Generic Haskell</h3>
[c]
quicksort :: (Ord a) =&gt; (List a) -&gt; (List a)
(quicksort List) = List
quicksort (p : xs) =
(quicksort lesser) ++ (List p) ++ (quicksort greater)
:where:
lesser  = filter (&lt; p) xs
greater = filter (&gt;= p) xs

[/c]
<h1>Conclusion</h1>
And a bit more work needs to be done for this language to be complete and practical of course.

Many generic data interchange languages exist, such as XML, YAML, and JSON.  But none have been defined to have the versatile terseness required for a programming language.  Such a syntax could have interesting uses:
<ul>
 	<li>It could make it easy to create a new domain-specific languages or new programming language without defining a parser.</li>
 	<li>You could write code from your favorite language, say Java or PHP, using this new syntax, and then define macros that transform your program before it is finally converted to Java or PHP.</li>
</ul>
In another post, I hope to develop this generic syntax more thoroughly, show some examples of how you might replace your current syntax with generic syntax, so that you can use Macros to transform your code.