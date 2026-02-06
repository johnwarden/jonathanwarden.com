
### Behavioral Referential Transparency

Expressions where all functions are hermetic have the **Behavioral Referential Transparency (BRT)** property.

Standard referential transparency ensures that `f(x)` depends only on the **value** of `x` (and the definition of `f`). Behavioral referential transparency generalizes this to stateful resources: `f(x)` depends only on the **observable behavior** of `x`.

**The Insight: No Hidden Inputs**
`f` cannot see anything outside its arguments. Therefore, the execution of `f(x)` is determined entirely by the definition of `f` and the interactions exposed by `x`.

#### Observational Equivalence

To be precise, we rely on the concept of **Observational Equivalence**.

Two inputs `x` and `y` are **Observationally Equivalent** with respect to `f` if `f` cannot distinguish between them using any operation available in its scope.

> **The BRT Guarantee:**
> If inputs `x` and `y` are observationally equivalent, then `f(x)` and `f(y)` produce **Isomorphic Traces**.

This handles edge cases like **Object Identity** naturally. If a language allows `f` to compare pointers (`x == y`), then two distinct objects are *not* observationally equivalent (because `f` can distinguish them). In that case, diverging traces are correct.

But for the vast majority of code that relies on **Polymorphism** (interfaces) rather than identity, a `RealFile` and a `MockFile` *can* be observationally equivalent. In those cases, BRT guarantees their execution traces will be identical.

#### Generalizing to Expressions

In a language where all functions are hermetic (no live closures), then the BRT property applies to any part of an expression. If `f` is a (hermetic) function, and `g` is observationally equivalent to `f` (same outputs for same inputs), then `f(x)` and `g(x)` produce isomorphic traces.

Behavioral referential transparency generalizes referential transparency, because a pure value is always behaviorally equivalent to itself.


---


#### An Alternative to the von Neumann Style

Backus argues that conventional languages reflect a von Neumann "machine picture": a vast ambient store plus special I/O primitives, encouraging programs that read, mutate, and write back state "one word at a time". Functional programming abstracts away that machine by making programs algebraic. Hermetic programming abstracts it away in a different way: the implicit “machine” stops being an invisible input, and interaction with state is routed through a small number of explicit interfaces with clear semantics—so programs read less cluttered traffic through the von Neumann bottleneck, and more like pipelines over well-named channels.


> Surely there must be a less primitive way of making big changes in the store than by pushing vast numbers of words back and forth through the von Neumann bottleneck. Not only is this tube a literal bottleneck for the data traffic of a problem, but, more importantly, it is an intellectual bottleneck that has kept us tied to word-at-a-time thinking instead of encouraging us to think in terms of the larger conceptual units of the task at hand. Thus programming is basically planning and detailing the enormous traffic of words through the von Neumann bottleneck, and much of that traffic concerns not significant data itself, but where to find it.[28][29]
