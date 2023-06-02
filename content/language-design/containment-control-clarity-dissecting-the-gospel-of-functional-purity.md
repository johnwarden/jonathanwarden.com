---
title: "Containment, Control, Clarity: Dissecting the Gospel of Functional Purity"
slug: "containment-control-clarity-dissecting-the-gospel-of-functional-purity"
image: assets/images/functional-purity.png
date: "2013-07-03T17:54:00-05:00"
draft: true
aliases:
- /2013/07/03/containment-control-clarity-dissecting-the-gospel-of-functional-purity/

---

In this post, I'll try to classify the goals of functional purity for the benefit of those who already believe in it, hopefully providing a useful structure for conversations around language design and best practices.

Many coders feel the critical importance of pure functions to the very marrow of our bones. And yet we struggle when trying to construct arguments to convince the "non-believers". Converts to functional purity tend to come to it through their own experience, not through logic. It is a belief akin to a religious value, or probably more so to a moral value. It is arrived at not so much through a reproducible process of deduction, but through the aggregate subconscious processing of countless experiences (largely debugging sessions) that emerge in an intuitive <em>knowing</em> that is as certain as a logical proof.

We may offer examples where values being modified when the programmer doesn't expect it will cause bugs and make someone's life harder. Or we speak in abstractions, about complexity and the the ability to reason about our programs. We might even point to studies. But many coders are so used to mutability and side-effects that complaining about them is like complaining about gravity.

The moral authority of functional purists is weakened in the eyes of non-believers when we make apparent compromises. We use monads and do blocks that appear to let us create side-effects while allowing us to say we aren't. Our code becomes incomprehensible for the less mathematically inclined. Monads proliferate, IO becomes part of the type signature of every function, do blocks are everywhere, and we further alienate the uninitiated with an attitude that suggests this is all a virtue, as if repeatedly acknowledging their presence were penitence for the sin of using functions that just <em>look</em> like they produce side effects. We spread hundreds of "IO"s throughout our code like hundreds of Hail Mary's.

Below I'll try to catalog and articulate the reasons many people do believe in functional purity, not necessarily to convince the "non-believers", but to help "believers" put a microscope to exactly what we are trying to accomplish, and to question how well we are achieving it.
<h3>1. Containment</h3>
There's a big difference between a Haskell function that could possibly perform a problematic IO operation, and a Java method that could do anything.

The "effects" of functions that return IO monads are contained to a limited, well-defined set of IO operations. They can't modify any values or variables in memory. They can't launch new threads or terminate the process or modify global variables. Their effects are <em>contained</em>.

A Java method, on the other hand, is totally un-contained. It <em>could</em> do anything. Subtle, insidious little things that create infuriating irreproducible errors. Even if you trust the guy who wrote the Java method, you know that accidents happen. Lack of containment is risky. Containment ensured by the language is better.
<h3>2. Control</h3>
Very similar to containment, but one step beyond, is control. A function that returns an IO monad doesn't actually do anything -- it just returns some IO operations that will be performed when bound to Main.main. &nbsp;They won't be performed at all if you don't do this binding.  You have control.

As another example, if you have a function that does some stateful operation with a State monad, not only are you sure that effects are contained to the output of that function, but you have complete control over the start and end state. You can pass in mock states, say for testing, and you can examine, modify, or reject the end state.

Or, if a function uses Writer for logging, the caller has complete control of what to do with those logs. It can ignore them, modify them, or just pass them on up the call stack.

This is a sort of inversion of control, where a function may perform pseudo-side-effects, but the caller controls exactly if and how these side-effects occur.
<h3>3. Clarity</h3>
Finally, clarity. This is what we mean when we talk about being able to reason about our code. There's nothing going on that we can't see right in the editor. You can point and say "there, all the effects of that function are held in that value there", there's no trying to remember if this function modifies that value or writes to some database, or if it depends on when exactly you run it or what environment variables you've set. Even pseudo-side effecting code with IO and State and Logging -- they are still just functions that return values, and we can see those values being returned and the flow of those state changes. Once you know how to read and write code like this, code is clearer and vast classes of bugs are preempted.
<h3>Applying the 3 C's</h3>
These three goals capture all the reasons for pure functions that I can think of at the moment (but please comment if I've missed some). &nbsp;And I think these three 'C's can provide a good framework when thinking about language design and best practices.
<h4>Sandboxes</h4>
It's interesting to see how these same goals can be met by other means. &nbsp;For example, containment can be achieved using "sandboxes". &nbsp;Sandboxes are why we allow our web browsers to run un-trusted Javascript code. &nbsp;It produces side effects, but they are contained.
<h4>Implicit Parameters</h4>
<span style="font-size: 13px; line-height: 19px;">Some see implicit parameters in Scala as dangerous. But where does the danger lie? Containment, Control, or Clarity?</span>

Implicit parameters are contained. They can't cause side effects, and there is a limited, well-defined mechanism for passing them to functions.

I believe there is no sacrifice to control: as the caller you can override them, control what functions they are passed to or not passed to, and make them explicit when you want to.

The danger lies, I believe, in clarity: it may not be clear that this invisible parameter is being passed around, and this could cause frustrating bugs. An argument can be made that nothing should happen in your code implicitly.

But knowing that the danger lies in clarity, not containment and control, we can focus our discussion on striking a balance between two aspects of clarity: the danger of values getting passed around that are not clearly visible in the code, and the benefits of reduction in code noise.