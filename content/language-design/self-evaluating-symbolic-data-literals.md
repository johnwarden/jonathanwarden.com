---
title: "Self-Evaluating Data in LISP"
slug: self-evaluating-data-in-lisp
image: /assets/images/2013-09-27-bcfd3b8.png
alias: http://jonathanwarden.com/2016/03/31/self-evaluating-symbolic-data-literals/
date: TODO
tags: ["language-design"]
---


## The Problem

Why were keywords and vectors added to LISP, a language that was already based on symbols and lists?

The answer: because keywords and vectors are self-evaluating. Like string and number literals, they are what they are. Unlike symbols and lists, they are never treated as code that is replaced with some other value when evaluated.

You might argue that if you don't want to symbol or a list to be treated as code, just don't evaluate it!  But this gets complicated if you are evaluating code that has non-code symbolic data embedded in it.

For example, it's clear that the code below outputs "2".

{{< highlight clojure "linenos=false" >}}

(def function 'print)
(def arg 2)
(eval (list function arg))
; prints 2

{{< /highlight >}}


But this code produces an error.

{{< highlight clojure "linenos=false" >}}

(def function 'print)
(def shape '(rect 2 4))
(eval (list function shape))
; ERROR: rect is not defined

{{< /highlight >}}

Even though we used the quote operator when we first defined `shape` to make sure its value was treated as data and not code, this value gets evaluated <em>again</em> during the evaluation of the generated code.

It can be hard to keep track of exactly when symbols and lists might be evaluated and thus need to be quoted. This is one reason LISP programmers prefer vectors and keywords for symbolic data that is not code. If data is not meant to be interpreted as code, it's better if it can't ever be interpreted as code -- just as it's better if the number 2 can't ever be interpreted as anything but the number 2. You can eval vectors, keywords, and numbers all day long and they will never change.

{{< highlight clojure "linenos=false" >}}

(def function 'print)
(def shape [:rect 2 4])
(eval (list 'print arg))
; prints [:rect 2 4]

{{< /highlight >}}

There is more good discussion about <a href="http://arcanesentiment.blogspot.com/2011/08/why-use-keywords-as-symbols.html">why we use keywords as symbols</a> in the Arcane Sentiment blog.

## An Alternative for Self-Evaluating Data

So a way to embed self-evaluating, non-code symbolic data literals inside of LISP code is valuable. Keywords and vectors make this possible.

In this essay I explore another possibility: a new quote operator for self-evaluating data.

### `:` as the Self-Evaluating Quote Operator

First, let's make the `:` symbol work just like `'`, but the resulting data structure is self-evaluating.

{{< highlight clojure "linenos=false" >}}

(def function 'print)
(def shape :(rect 2 4))
(eval (list function shape))
; :(rect 2 4)

{{< /highlight >}}

## Lists for Trees, Vectors for Sequences

It's common for the head of a list in LISP to be a symbol that defines the semantics of the list, whether the list is used to represent code...

{{< highlight clojure "linenos=false" >}}
'(sqrt 2)
{{< /highlight >}}

...or self-evaluating symbolic data where the head is a symbol indicating the data type (this is called "<em>tagged data</em>" in SICP). 

{{< highlight clojure "linenos=false" >}}
:(rect 2 2)
{{< /highlight >}}

Lists used in this way function more like <em>trees</em>, with the first element acting as the <em>root</em> that indicates the type or behavior of that node, followed by zero or more <em>subtrees</em>.

Contrast this to a list where the first item doesn't have any special significance: it just happens to be first.

{{< highlight clojure "linenos=false" >}}
:(red blue)
{{< /highlight >}}

LISP programmers are more likely to use vectors in this latter case, partly because LISP will never try to eval a vector as a function-application, and partly because of the syntactic similarity of vectors to lists in other languages.

{{< highlight clojure "linenos=false" >}}
[:red :blue]
{{< /highlight >}}

But with the self-evaluating data quote operator, we no longer need to "quote" each symbol.

{{< highlight clojure "linenos=false" >}}
:[red blue]
{{< /highlight >}}

TODO: these are the same!!!




I'd argue that in LISP, lists have proved themselves most appropriate for representing tree-structured data, either code or tagged data, where the head of the list is a symbol or expression acting as the root of the tree. Vectors on the other hand have proved themselves most useful for representing ordered collections.

The following code snippet illustrates these contrasts. It uses a list for a function call, self-evaluating lists to represent rectangles as "tagged data", and vectors to represent an collection of rectangles.

{{< highlight clojure "linenos=false" >}}
(render [:(rect 3 2) :(rect 8 10)])
{{< /highlight >}}


## Nesting `:`- and `'`-quotes

Naturally, you can nest self-evaluating data literals (`:`-quotes) inside of `'`-quoted code:

{{< highlight clojure "linenos=false" >}}
(defvar x '[ (square 2), (:square 2) ])
(eval x)
; [4, (square 2)]
{{< /highlight >}}

But there's no reason you shouldn't also be able to nest code inside of self-evaluating data literals:

{{< highlight clojure "linenos=false" >}}
(defvar x :[ ('square 2), (square 2) ])
(eval x)
; [4, (square 2)]
{{< /highlight >}}

The two expressions above produce identical results, but in the first case, `x` is code, and we :-quote the part that we <em>don't</em> want to be evaluated as code. In the second case, `x` is self-evaluating data, and we '-quote the part that we <em>do</em> want to be treated as code.

## Summary



If a single symbol is quoted, the result is a self-evaluating symbol -- e.g., a keyword.

{{< highlight clojure "linenos=false" >}}

:blue
; :blue

{{< /highlight >}}

Code is data, but not all data is code. When LISP was created, it was intended that symbols and lists be used to represent all sorts of symbolic expressions -- not just code. But the requirement for self-evaluating non-code data literals that can be included as literals in code, led to the introduction of keywords and vectors as self-evaluating supplements to symbols and lists.

The self-evaluating data quote operator takes this development one-step further, allowing entire expressions to be marked as self-evaluating data. This allows self-evaluating lists to be used to represent tagged data, leaving vectors to play the more natural role of listing things.