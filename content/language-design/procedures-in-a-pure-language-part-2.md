---
title: "Procedures in a Pure Language - Part 2"
slug: "procedures-in-a-pure-language-part-2"
series: ["Procedures in a Pure Language"]
image: assets/images/Pure.jpg
alias: http://jonathanwarden.com/2015/05/31/procedures-in-a-pure-language-part-2/
series: ["Procedures in a Pure Language"]
date: 2015-05-31T01:22:00-05:00
weight: 33
---

In my last post on <a href="http://jonathanwarden.com/2015/05/28/procedures-in-a-pure-language/">Procedures in a Pure Language</a>, I discussed how even a "purely functional" programming language such as Haskell actually allows you to create procedures that have side effects, and how that is the way things should be.

I also defined the following wish list for a programming language where:
<ul>
 	<li>I can write pure functions.</li>
 	<li>I can also write procedures.</li>
 	<li>I can treat procedures as values.</li>
 	<li>I clearly see in my code when I am defining a procedure and when I am defining a pure function.</li>
 	<li>I clearly see in my code when a procedure is executed.</li>
 	<li>I can write procedures without the <em>syntactic overhead</em> of the IO Monad required in Haskell.</li>
 	<li>I can <em>contain</em> procedures to only operate on specific objects, so that I can limit their effects to a <em>sandbox</em>.</li>
</ul>
<h2>Proposed Solution</h2>
Suppose we start with a simple untyped language with an <code>-></code> operator for defining anonymous functions, and a <code>++</code> operator for concatenating strings.

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

I have defined a function, <code>main</code>, which takes an argument named <code>console</code>, and returns a <em>procedure</em>.

The body of a <code>procedure</code> is a sequence of <em>imperatives</em>.  In this example there is a single imperative, <code>console.println!("Hello, Bill")</code>.  An imperative is to an expression what a function is to a procedure: both return values, but an imperative doesn't have to be pure functions.

<code>console.println</code>, like <code>main</code>, is a function that returns a procedure.  The <code>!</code> operator says that this procedure should actually be <em>executed</em>, on not just returned, at this point in the code.  Otherwise, the result of evaluating <code>main</code> would be a procedure that, when executed, just returns another procedure.
<h2>Methods</h2>
<code>console.println</code> looks like what you'd call a method in OO.  I'm not thinking&nbsp;we're defining an OO language here, mind you.  We could easily have written this as <code>println console</code>, but I like the <code>.</code> syntax here.  Either way, <code>println</code> is a function that is somehow attached to the <code>console</code> value -- or more specifically <code>console</code> is polymorphic: <code>console</code> itself supplies the definition of <code>println</code>.  We don't need to go into detail of exactly how this works (types? classes? typeclasses?).  I'll just say that functions like <code>println</code> that are attached to objects are called <code>methods</code>.
<h2>The "Apply and Execute" Operator</h2>
The <code>!</code> binary operator could be thought of as "apply and execute", because it applies a function to its arguments, and then execute the procedure that is returned.

You can also apply a function to it's arguments without executing it:

{{< highlight java "linenos=false" >}}

let greet = console.println("Hello, Bill")
{{< /highlight >}}

The <code>!</code> operator can also be used as a unary, postfix operator, which simply executes a procedure (instead of calling a function and executing the resulting procedure).

{{< highlight java "linenos=false" >}}

greet!
{{< /highlight >}}
<h2>Operations</h2>
Methods like <code>println</code>, that <em>return</em> procedures are called <em>operations</em>.

The <code>!</code> binary operator is used to <em>invoke</em> an operation by applying arguments to a function and then executing the procedure.
<h2>Summary</h2>

{{< highlight java "linenos=false" >}}

main = (console) -> procedure
	let greet = console.println("Hello, Bill")
	greet!
console.println!("Hello, Bill") // another way of doing the above.
{{< /highlight >}}

So <code>main</code> is a <em>pure function</em> that returns a <em>procedure</em>.  <code>println</code> is an <em>operation</em> -- a <em>method</em> that returns a <em>procedure</em>.  <code>println</code>, like all methods, is also a pure function, because simply <em>applying</em> it has no side effects.

<code>greet</code> is a procedure, the result of applying <code>println</code> to its arguments in the expression <code>console.println("Hello, Bill")</code>.

<code>greet!</code>, because of the presence of the <code>!</code> operator, is an <em>imperative</em>.

<code>console.println!("Hello, Bill")</code> is likewise an imperative.
<h3>Summary of Definitions</h3>
<ul>
 	<li><em>Function</em>: takes arguments, never has effects.</li>
 	<li><em>Procedure</em>: takes no arguments, has effects when executed.</li>
 	<li><em>Method</em>: functions attached to objects (enabling polymorphism).</li>
 	<li><em>Operation</em>: method that produces a procedure.</li>
 	<li><em>Expression</em>: has no effects, consistent.</li>
 	<li><em>Imperative</em>: may have effects or be inconsistent.</li>
</ul>
<h2>Conclusion</h2>
We have defined a language that, like Haskell, allows us to define pure functions, which themselves can produce procedures that can be executed.  The body of any function containing imperatives that execute procedures must be a procedure, just as in Haskell any function that uses a <code>bind</code> operation or <code>do</code> on an <code>IO something</code> must itself return an <code>IO something</code>.  But our language has first-class procedures instead of the <code>IO</code> monad, and the <code>!</code> operator instead of <code>do</code> or any of the <code>bind</code> operators.

Also just as in Haskell, "evaluating" the program has no side-effects.  It just produces a procedure which you can then execute.

Our language doesn't treat procedures as Monadic value as does Haskell.  After a program is evaluated there is no need for something that can be bound, <code>fmaped</code> over, or stuck in a <code>do</code> block, since all that you will ever do with this procedure is execute it.

Also by treating procedures differently from monadic values, it is even easier to see exactly when you are invoking procedures.  This will be helpful to a programmer striving to minimize unnecessary use of impure code


