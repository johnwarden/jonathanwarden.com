---
title: "Hermetic Programming"
slug: "hermetic-programming"
# image: image.png
date: "2026-02-19"
weight: 10
math: true
noindex: true
_build:
  list: never
  render: always

---

<style>
    .image-with-caption {
        display: block; margin-left: auto; margin-right: auto; max-width: 450px

    }

.note {
    font-size: 1.2em;
    background-color: #FFDDDD;
    padding: 8px;
    font-weight: bold;
}
</style>


<aside class="note">
    Draft: please don’t share publicly; I’m circulating for feedback.
</aside>

## Introduction

In most programming languages, any function can reach out and touch the world: read the clock, write a file, open a socket. But this **ambient access** to state—through singletons, globals, built-in functions—makes testing brittle and reasoning murky. You run a test once and it passes; run it again and it fails because the clock advanced or a temporary file wasn’t deleted. You call out to a small utility library and it exfiltrates your SSH keys because it had access to your home directory.

The practice of **dependency injection**[^di] can help tame access to state: don’t let code "reach out" for resources; pass them as parameters instead. What if a language take dependency injection to its logical conclusion, and makes it a **semantic property** of code, not just a design pattern? Then all system resources become capabilities that must be injected—for example as parameters passed to `main`.

**Example (Typescript)**

```typescript
import type { Console } from "io";

// Runtime injects the concrete Console implementation.
export function main(console: Console): number {
  console.println("Hello, World!");
  return 0;
}
```


In the above example, `main` is a **hermetic function**:

> A function is hermetic iff it does not access existing state except through its parameters.

If `main` is hermetic, any functions it calls must be hermetic as well (otherwise `main` is indirectly accessing existing state). A hermetic `main` therefore eliminates **ambient authority**, the defining requirement of **object-capability (ocap)[^ocap] languages** such as E, Monte, and Agoric's SES (Secure EcmaScript) . Hermetic programming generalizes this same constraint from external resources to **all existing state**.

The parameters to hermetic functions act as hermetically sealed conduits through which all authority flows. Whether accessing system resources, communication channels, or internal program memory, the caller of a hermetic function controls the world the function can see. Deterministic time? Pass a fake clock. Sandboxed output? Pass a mock filesystem. Every potential access to state is visible at the call boundary. You can read a function's dependencies off its signature. Effects, without ambient side-effects.


## The Purity Gap

This essay proposes hermeticity as a first-class semantic property of functions, distinct from purity. But hermetic programming is not just functional programming-lite. **Hermeticity is orthogonal to purity**.

Suppose I have a global immutable constant `systemClock: Clock` holding an opaque handle to the system clock. And suppose `getClock(): Clock` is a function that simply returns that constant.

`getClock` is pure: it has no effect on the clock, and since it returns a constant, it is referentially transparent.

Suppose I also have a function `getTime(clock: Clock): Time`, which takes a `Clock` (either the real clock or a mock) and returns that clock's time. I can now get the current time from the system clock by calling `getTime(getClock())`. 

`getTime` is hermetic because it doesn't read or write any state other than the clock passed as a parameter. `getClock` is pure. But the resulting composed function `now = getTime ∘ getClock` is *neither hermetic nor pure*! 

How can this be? A hermetic function by definition can't "reach out" and access the clock. And a *pure* function is even...purer...then a hermetic function, isn't it?

The problem is that `getClock`, while being technically pure, is tainted by **access** to ambient state. By returning a reference to the real clock, it **exposes** that state.


## Interaction vs Access

`now` and `getClock` can be thought of as being **hard-wired** to state. They both access the system clock and you can't pass a parameter that redirects that access to some other clock.

`getTime` on the other hand parameterizes its access to state and is not hard-wired to any clock.



<div class="image-with-caption">

<img id="figure1" src="clock-and-wires-3.png"
     alt="Figure 1. Illustration of hermetic vs non-hermetic functions. "/>

<div>
    <strong>Figure 1</strong>. Illustration of hermetic vs non-hermetic functions. Non-hermetic functions are hard-wired to state.
</div>

</div>

Although `getClock` is hard-wired to the clock, it is pure because it does not actually **interact** with the clock like `now` does.

It is interaction with state that makes a function impure. The most widely accepted definition of purity is **referential transparency**: an expression can be replaced by its value in any program context without changing observable[^observable] behavior. A function fails referential transparency when evaluation has observable interaction with external state: either it *affects* state, or it returns different outputs for the same inputs because it is *affected* by state.


Hermetic functions are restricted in terms of access to state, not interaction. So hermeticity is actually *more strict* than purity in some ways (it forbids access to ambient state), and less strict than others (it permits interaction).

**Interaction vs Access Grid: Examples**

<!--

|           |hermetic        | non-hermetic    
|-          |-               |-
|**pure**   | `sqrt(n)` ⬅ pristine        | `getClock()`
|**impure** | `getTime(clock)` | `now()`


-->

<div class="image-with-caption">

<img id="figure2" src="pure-hermetic-grid.png"
     alt="Figure 2. A comparison of example functions along the axes of interaction (pure/impure) and access (hermetic/non-hermetic)" style="width: 25em;"/>

<div>
    <strong>Figure 2</strong>. Example functions arranged along the axes of interaction (pure/impure) and access (hermetic/non-hermetic).
</div>

</div>



This distinction between access and interaction has consequences beyond individual functions—it extends to packages, standard libraries, and whole programming languages. But to get there, we need to understand what exactly it means to **expose** state.



<style>
.glossary {
  margin: auto;
  background-color: lightgrey;
  border: 1px solid black;
  max-width: 600px;
  padding-top: 1em;
  padding-bottom: 0px;
  padding-left: 1em;
  padding-right: 1em;
  margin-bottom: 1em;
}
</style>

<aside class="glossary" markdown="1">

***Summary of Definitions***

* **existing state**: state that existed before the function call
* **external state**: state observable outside the call (including fresh state that escapes)
* **free state**: existing state not accessed through a function’s parameters
* **interact**: to **affect or be affected by** state
* **expose**: to return a value or write it into **external state**
* **access**: to **interact with or expose** state
* **pure**: no **interaction** with **external state**
* **hermetic**: no **access** to **free state**

</aside>


## Live and Inert Values

`getClock` exposes state by returning a handle. A function could also expose the same handle by writing it into existing state reachable from a parameter—for example, `setClock(app, getClock())` could store the clock inside `app` (a way of exposing state we call [grafting](#grafting-state)).

In either case, what is exposed is a value that provides access to state.

> A value that provides access to state is **live**. 

Live values can include object references, handles, primitives, closures, etc.; as well as anything that embeds any of these and thereby provides a path to state.

Live values are like live wires. A value can become live when it is connected to another live value. And if you plug a hermetic function into a live value, you can cause an interaction.

Values that don't provide access to state are **inert**: isolated from state.

### Providing Access to State

Providing access to state is different from merely **designating** state. A file name for example does not by itself provide the ability to interact with the file system. A function that receives a filename would need to reach out to some library or builtin function such as `open` -- in which  case, it's `open` that's live.

Intuitively: a value is live if it can cause interaction with existing state in some context (directly or by enabling a call), whereas it is inert if it cannot, no matter how used, lead to interaction with existing state unless paired with a live value.

Equivalently: a value is live if you can swap it for a mock that redirects the interactions it enables into in-memory state controlled by the caller, without otherwise changing program behavior. In [Appendix D](#appendix-d-the-mockability-test) we formalize this as the **Mockability Test**.

### Hermetic Functions are Inert

We can talk about functions in two roles: as code (a callable) and as values (passable/storable). 

> A function is hermetic as code exactly when it is inert as a value.

Hermetic functions can only access state through live values passed as parameters. Passing a hermetic function to another function cannot provide the latter with access to state. So they are inert as values.

### Live Free Identifiers

A function can only be live if it embeds a live value in its definition.

A **free identifier** is any name appearing in a function definition that is not a parameter or a local variable.

> A function is inert if its definition has no live free identifiers.

Consider the Python example below:

<div class="image-with-caption">

<img id="figure3" src="inert-live-code-example.png" alt="Figure 3. Example of hermetic vs a live function definition."/>

<div>
    <strong>Figure 3</strong>. Example of inert vs a live function definition. 
</div>

</div>

The only lexical name that `hello` references is the parameter `out` (`write` is a member selection on `out`), so `hello` is inert. In contrast, `main` is live because it refers to the live free identifier `stdout`. 

Although `main` is a function and `stdout` is a handle, they are both live as values.

<div class="image-with-caption">

<img id="figure4" src="clock-and-wires-2.png"
     alt="Figure 4. Illustration of difference between inert and live values."/>

<div>
    <strong>Figure 4</strong>. Live and inert values in Hello, World! program. main and stdout are both hard-wired to state.
</div>

</div>


## Hermetic Programming Languages

At this point we've closed the loop: hermeticity is a restriction on **access** to state; access is mediated by **live values**; and a function stops being hermetic exactly when it becomes live as a value. 

A function can only be live if it captures live free identifiers. There are two ways this can happen:

- Ambient identifiers (globals/imports/primitives)
- Captured environments (closures)

### Inert Ambient Scope

**Ambient identifiers** are names that are available to all functions and modules by default. These are the identifiers that are *just there*: default imports, preludes, built-in functions, system calls, global constants, primitives, etc.

Collectively, these form the **ambient scope**. 

> A hermetic programming language requires an **inert ambient scope**.

If the ambient scope contains even one live identifier (like a global `console` object), it allows any function to "reach out" and access ambient state.

#### Inert Packages

> An inert ambient scope implies **inert packages**. 

An import statement must not introduce live values into the ambient scope, nor have any observable side effects (no module initialization with observable interactions).

This means every identifier exported by a package or module must be inert. Consequently, package-scoped functions must not capture live values from other packages.

**Example (go): live package with a global singleton**

```go
package logger

import "os"

// Log is live because it captures a live free identifier os.Stdout.
func Log(msg string) {
    os.Stdout.WriteString(msg + "\n")
}
```

Further, inert packages must cannot host global singletons. Package package-level globals must be immutable, inert constants — otherwise they pollute every function that captures them, making those functions live.

**Example (go): live package with package-global state**

```go
package counter

var count int = 0

// Inc is live because it captures a live free variable.
func Inc() int {
    count++
    return count
}
```


However, inert packages can export inert functions that allocate and expose fresh state.

**Example (go): inert package with exported hermetic constructor**

```go
package counter

// NewCounter is inert because it does not capture any live free variables
func NewCounter() func() int {
    var count int = 0
    return func() int {
        count++
        return count
    }
}
```


Although `NewCounter` returns a closure over mutable state, it is freshly [**minted**](#minting-sate) state that didn't exist before the call, not existing state. `NewCounter` does not embed any live free identifiers and so it is inert as a value.

> Hermetic constructors can mint state.

Inert packages can export types, interfaces, constants, wrappers, methods, constructors, and other hermetic functions. They can provide complex imperative algorithms that *interact* with external resources (e.g., a database client), as long as those resources are passed as parameters.

In a hermetic programming language, the standard library defines *interfaces* to system resources (filesystem, network, clock, etc.), but the actual access happens through parameters injected into main.

#### Example: Hermetic HTTP

It may seem like forcing packages and libraries to be inert would severely limit them. For example, how could an HTTP library be inert? HTTP is all about I/O and state.

But hermetic HTTP just requires parameterizing access to the network. For example, go's `net/http` exports an `http.Serve` function that accesses the network exclusively through its `net.Listener` parameter. This parameter can be substituted for an in-memory `net.Listener` implementation (notably gRPC’s `bufconn`[^bufconn]) that doesn't interact with the actual network at all.

Although go's `net/http` package is live because it exports functions that are hard-wired to the network and other resources (default logger, global transports/resolvers), it could easily be made hermetic by parameterizing access to all these resources, reducing the HTTP package to the protocol logic only. In the Python world "sans-I/O"[^sansio] libraries like the [hyper-h2](https://github.com/python-hyper/h2) HTTP/2 library are pure state machines over bytes,  with all I/O delegated to a thin, parameterized shell.



### Closures

A language with an inert ambient scope has no live ambient identifiers, which guarantees that top-level definitions are inert. But that does not prevent creating closures that capture live local variables. Making all functions hermetic additionally requires eliminating **live closures**.

So there are two design choices:

1. **Allow live closures.**
   Ambient access to state is still forbidden, but function values can be made intentionally live via capture. This is often desirable—especially in languages where partial application and higher-order functions are idiomatic.

2. **Make all function values hermetic.**
   Prevent or expose captures (e.g. capture checks), or adopt the standard *closures-as-objects*[^defun] view: treat a closure as a live object with an `apply` method. Under this view, the `apply`/method can still be **hermetic**—it only reaches state through `self` and its explicit parameters, not through ambient channels. Functions with hidden environments become objects with explicit environments.



### Hermetic Programming Language Requirements 

> A programming language is hermetic iff it has an inert ambient scope.

Making all function hermetic by eliminating live closures is an optional strengthening: it extends hermeticity's isolation and local-reasoning guarantees to all function values, but may be too restrictive for languages where closure capture is central.

> All Functions Hermetic = Inert Ambient Scope + No Live Closures

## Benefits of Hermetic Programming


### Behavioral Referential Transparency 

For pure functions, Referential Transparency ensures that `f(x)` depends only on the **value** of `x` (and the definition of `f`). **Behavioral Referential Transparency** generalizes this to stateful inputs: `f(x)` depends only on the **behavior** of `x` (as observable by `f`). There are no "hidden inputs".

The Behavioral Referential Transparency property facilitates **local reasoning**: minimizing the number of things a programmer needs to keep in mind to understand a fragment of code.

Pure functional programming achieves a strong form of local reasoning by eliminating interaction with state completely. But hermeticity also aids local reasoning by reducing the "splash radius" of possible interaction with state to the **explicit parameters** of a function.

For example, suppose I pass a mutable list to a hermetic function:

```typescript
// x is a mutable list
let x = [1, 2, 3]
f(x)
```

If `f` is indeed hermetic, the only state it can access is the list referenced by `x`: it cannot consult a global, log to a singleton, or touch the clock. To understand `f(x)` the only state I need to think about is `x`. I can forget about any other possible side-effects.


### Composability

Hermeticity scales. Hermetic functions cannot give each other access to state, so hermetic functions are **closed under composition** as well as **closed under higher-order application**, 

This means you can assemble large programs out of small pieces while preserving the **no hidden inputs / no ambient access** guarantees. You can read dependencies off the **signature**, instead of re-auditing the whole program.

### Isolation

Because hermetic functions are isolated from ambient state, all hermetic functions can be **tested with mocks** with no additional refactoring. They can be run **deterministically**: all sources of non-determinism (clock, RNG, network) must be explicitly passed as parameters, and can be replaced with deterministic alternatives. A hermetic function can be run in different execution contexts -- as a stored procedure, plugin, browser, or sandbox -- without the need for separate hermetic runtimes.[^hermetic-runtimes]

### Security

One of the tenets of information security is the **principle of least authority**[^protection]. Instead of running in "pools of authority on which viruses grow"[^paradigm], each function should have only the minimum authority it needs to do its job. When you recognize that the functions you call may be malicious, its clear that the math library you downloaded should not have access to your email.

This vulnerability is at the heart of the modern open-source supply chain crisis. Today, we routinely download thousands of transitive dependencies via package managers like npm or cargo, crossing our fingers that a simple utility library hasn't been compromised. Because most languages grant ambient authority to the network and filesystem by default, a hijacked package can silently exfiltrate environment variables, SSH keys, or database credentials.

**Capability-based security** discipline[^capsec] largely neutralizes this thread by eliminating **ambient authority**.[^capmyths] An untrusted library simply *cannot* access the network or disk if the caller never passed it the live capabilities required to do so.

> Hermetic programming enforces the "no ambient authority" rule as a semantic language property. 

If we define "resources" and "dependencies" to include *any* existing state (filesystem, clock, randomness, communication channels, persistent program memory), then "inject all dependencies" is literally equivalent to "no ambient authority". All authority must be delegated explicitly through live values passed as parameters: providing access is equivalent to granting authority.

> In a hermetic programming language **live values are capabilities**.

Further, the higher-level capability security properties of **overt confinement** and **revocation** are consequences of the core semantics of hermetic functions—not extra security mechanisms. A hermetic function can only overtly communicate via channels explicitly provided by the caller, and it is memoryless across calls (unless memory is explicitly passed in). So if a function never receives *the authority to [graft](#grafting-state) a capability into some existing state*, then any authority it receives is confined to the function call, and automatically revoked when the function returns.

Hermetic programming is thus the semantic generalization of "no ambient authority": not just no ambient access to files, sockets, or clocks, but no ambient access to **any existing state** whatsoever.

## Hermetic Programming Practices

### Capability Security Discipline

While hermetic programming eliminates sources of *ambient authority*, it does not prevent programmers from granting *too much authority*. If `main(world)` is injected with a "world" object with sub-fields for all external resources (`world.clock`, `world.fs`, `world.net`...), it can just pass this "god object" down through the call stack. 

This is still hermetic -- the caller still has control -- but the principle of least authority dictates that `main` should *only ask for the capabilities it needs*. 

Using a standard such as WASI (WebAssembly System Interface)[^wasi] for interfaces, which defines capability-based APIs for system resources, the signature of `main` can act as a dependency manifest, and a capability-aware runtime, OS, or host can determine whether to actually grant those capabilities and how, either through handles to real system resources, sandboxes, or mocks. 

Capability discipline also requires **attenuating** the capabilities a function passes to sub-functions as much as possible[^attenuate] -- delegating authority only on a "need to do" basis. Pass a single file handle instead of the entire filesystem.

Finally, even in a hermetic programming language, a hermetic function can still delegate authority by [graft](#grafting-state) a live value into mutable state whose type is broad enough to hold live values. 

**Example (Go) of a function that delegates authority by grafting**

```go
func handle(ctx map[string]any, db *Database) {
    ctx["db"] = db // grafted: db is now reachable via ctx
}
```


Any code that reads `ctx` now has database access.

A more secure design would require a narrower capability than the whole database, make `ctx` read-only, or restrict the types it can store.

### Contexts

Hermetic Programming requires more "wires". Parameters must be threaded through the call stack to reach all functions that need them ("prop-drilling"[^propdrilling], "parameter pollution"). This can make code noisy and refactoring complicated, especially for cross-cutting aspects of a program such as logging.

One way of reducing the number of explicitly-passed parameters is to support **contexts**[^scala-context] (also known as implicits). In this pattern, the "wires" are hidden from the function *body*, but they usually remain visible in the function *signature*. Here's an example in Scala:

```scala
// 1. The Middleman (main): Carrier of the context
// main does not use Logger, but must declare 'using Logger'
// to allow it to pass implicitly to Bar.
def main()(using Logger): Unit = {
    foo()
}

// 2. The Leaf (foo): Consumer of the context
// foo explicitly states: "I can only run if a Logger is in context."
def foo()(using logger: Logger): Unit = {
    logger.info("Called foo")
}
```
 
This keeps the call sites clean while ensuring that dependencies are clearly documented in the types, satisfying the hermeticity while mitigating the verbosity and facilitating refactoring.


## Hermetic Programming in Pure Functional Languages

You might think hermeticity is irrelevant in a pure language. If functions can't have side effects, why worry about access to state?

Because pure values can still be **live capabilities**.

### Effect Values Can Be Live

In Haskell, an `IO ()` describes an effectful computation.[^awkward] It is like a compiled program: constructing it has no side effects, but when it is interpreted by the runtime it interacts with state. This means **the value itself carries authority**.

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

Although `main` is pure, it is live in our sense: it hard-codes a reference to the live identifier `putStrLn`, which effectively hard-wires `main` to the real console. There’s no principled way to inspect the resulting `IO ()` and swap the console for a buffer. So when the program is executed it invokes a console operation through ambient authority rather than a capability passed as a parameter. 

**Pure does not imply inert.**

<div class="image-with-caption">

<img id="figure5" src="putstrln-example.png"
     alt="Figure 5. Illustration of Hello World! program where main is live because it is ultimately hard-wired to the system console."/>

<div>
    <strong>Figure 5</strong>. Illustration of Hello World! program where main is live because it is ultimately hard-wired to the system console.
</div>

</div>

### The Hermetic Alternative

Compare `main` with a version that parameterizes its access to the console, using what Haskell programmers call the **tagless final**[^tagless] pattern:

```haskell
class Monad m => Console m where
    putLine :: String -> m ()

hermeticMain :: Console m => m ()
hermeticMain = putLine "Hello, World!"
```

`hermeticMain` does not assume a particular console and does not commit to `IO`. The caller can choose which `m` to supply—including an in-memory mock. All access to the console is routed through an injected capability.

A concrete caller wires `hermeticMain` to the real console (the `Console IO` typeclass instance is an implicit parameter):

```haskell
instance Console IO where
    putLine = putStrLn

main :: IO ()
main = hermeticMain
```

So `hermeticMain` is both pure and inert. Whereas `main :: IO ()` is pure but live. Both sit in the "pure" row of our interaction vs. access grid—but in different columns.

### Toward Hermetic Haskell

Haskell's `Prelude` and standard library has many live values. `putStrLn`, `readFile`, `getCurrentTime`—these are all hard-wired to real system resources. Any function that calls one of these becomes live. The liveness propagates through composition, exactly as it does in imperative languages.

A "Hermetic Haskell" would move all live identifiers out of the Prelude and standard libraries, exporting only **interfaces** (e.g. type classes using the tagless final pattern) to system resources, with concrete implementations injected into `main` by the runtime. The `ReaderT`[^readert] pattern — Haskell's analog of the [contexts/implicits](#contexts) discussed earlier — can help manage the extra wiring.

Purity guarantees that code doesn't interact with external state when evaluated. Hermeticity guarantees that code doesn't have *access* to external state unless  authorized. A function that is pure but not hermetic can still be hard-wired to the real filesystem, clock, and network. This can make tests flaky, mocks impossible, and reasoning non-local. That's why tagless final and ReaderT patterns are common in Haskell. Not because Haskell isn't pure enough—but because it isn't hermetic.

## Conclusion

When we think of "pure" data, we may imagine something cleanly serializable into a format like JSON. But the quality we are really reaching for is not purity but **inertness**. References, channels, closures, effect values—these are the living machinery of computation, rooted to their execution environment. Integers, strings, lists—inert matter being computed.

**Hermeticity is inertness applied to functions.** A hermetic function may not be pure, but it is pure functionality. Since it cannot access state directly, it must be plugged into live parameters to do anything in the world. The live/inert distinction applies even in pure functional languages. Pure/impure and inert/live are independent axes of "clean", and functional and hermetic programming are complementary.

Making a programming language hermetic is straightforward in principle: keep the ambient scope inert, export interfaces from standard libraries, not live values, and inject concrete resources into `main`. Contexts or implicits can manage the extra wiring. 

Hermetic programming carves programs at the joints. All hermetic functions are decoupled from the state they act on, completely testable against mocks out of the box, reusable, and composable. Function signatures are dependency manifests. All authority is explicit. Entire classes of supply chain vulnerabilities are structurally eradicated. 

We should look back at `import fs` granting immediate, permissions-based disk access with the same horror we now reserve for manual memory management in critical systems. The next generation of programming languages should be hermetic.

Hermetic programming links dependency injection and capability-based security using a single semantic rule: **a function may only access existing state through its parameters**. No hard-coded dependencies. No ambient authority. No hidden inputs. No leaks. 



## Appendices


### Appendix A: Hermeticity as Isolation

> **Hermeticity is a software engineering concept that refers to the ability of a software unit to be isolated from its environment.**
>
> -- [Scott Herbert (slaptijack)](https://slaptijack.com/programming/benefits-of-hermeticity.html)

The term **hermetic** has been used to describe a number of systems that isolate imperative code from its environment: most notably Google's [**hermetic testing**](https://carloarg02.medium.com/how-we-use-hermetic-ephemeral-test-environments-at-google-to-reduce-test-flakiness-a87be42b37aa), [**hermetic servers**](https://testing.googleblog.com/2012/10/hermetic-servers.html),
hermetic builds ([**bazel**](https://bazel.build/basics/hermeticity)), and hermetic languages ([**wuffs**](https://github.com/google/wuffs#readme) and [**starlark**](https://github.com/bazelbuild/starlark)). These all **isolate** effects to specifically authorized state: a test mock, a build artifact, the function's arguments.

I've appropriated the term *hermetic* in this essay to mean isolation only with respect to *access* to state. But there are levels of isolation beyond hermeticity. For example, Google's Wuffs are also isolated in the ACID sense, and they cannot spawn threads, allocate memory, or panic.

#### Call-Boundedness

A hermetic function might still spawn a goroutine or task that keeps interacting with state after the function has completed. It's still hermetic as long as the thread only interacts with explicitly authorized state.

A function that uses **structured concurrency**[^structured] ensures all spawned threads complete before the function returns. We call such a function **call-bound**. 

Just as a pure function can use **internal state** that doesn't survive the call, a call-bound function may use **internal concurrency** that doesn't survive the call. 

#### Atomicity 

In multi-thread environment with shared state, hermeticity doesn't guarantee **isolation in the ACID sense**: preventing concurrent processes from interleaving reads and writes.

A function call is **atomic** if, from the caller's perspective, any state changes happen in a single instant (or not at all). 

#### Containment

Hermeticity doesn't guarantee that a function will **complete successfully**: it may loop infinitely, run out of memory, panic, etc.

A function is **contained** if it is guaranteed to return control to the caller without crashing the host process, regardless of input—achievable through a language that prevents runtime panics (like Wuffs) or a runtime that sandboxes execution (like Erlang supervisors).

#### Full Isolation

Hermeticity and call-boundedness are properties of the function definition itself, while atomicity and containment depend on the execution context. A function that is hermetic and call-bound, run in a context that guarantees atomicity and containment, could be considered **fully isolated**.


### Appendix B: Restrictions on Access to State

Our definitions of pure and hermetic functions lead to some somewhat surprising conclusions. For example, constructors that return live values can still be hermetic.

In this appendix, we'll summarize our definitions of the different types of state and the restrictions on how pure and hermetic functions can access state.

#### Types of State

State can exist before a function call (**existing state**), or be allocated during the function call (**fresh state**).

Fresh state that is destroyed during the function call (e.g. is de-allocated or becomes immutable when the function returns) is **internal state**. 

Existing state, or fresh state that escapes (is not deallocated) and is therefore observable after the function call, is **external state**.

**Free state** is existing state that is accessed through live free identifiers -- not the function's parameters.

**Ambient state** is state accessed through live ambient identifiers.

#### Grafting State

A function can expose state by writing a live value into existing state — for example, storing a reference in a mutable field or channel — thereby making it reachable by other code without returning it. We call this **grafting** state.

#### Minting State

A hermetic function can expose **fresh** state that it allocates during the call. We call this **minting** state. Minting is why constructors can be hermetic: the minted state didn't exist before the call, so the function isn't accessing existing state. The function as a value remains inert.

#### Summary of Restrictions

Capability | Pure | Hermetic
-|-|-
Access Free State                 | Yes | No  
Interact with External State      | No  | Yes 
Interact with Internal State      | Yes | Yes 
Graft State                       | No  | Yes 
Mint State                        | No  | Yes 


### Appendix C: Glossary of Terms

- **state**: anything that can affect or be affected by a computation that can be observed by normal program operations (ignoring timing, memory and CPU usage). This can be external resources (filesystem, network, clock) or internal memory.
- To **allocate** state: to allocate memory that can be read and written
- **existing state**: state that existed before the function call
- **fresh state**: state that is allocated during the function call
- **internal state**: fresh state that does not survive the function call
- **external state**: state (fresh or existing) that is observable[^observable] outside the call (e.g. that survives the call)
- **parameterized state**: state that is accessed through a function's parameters
- **free state**: existing state that is not parameterized
- **ambient state**: free state accessible to all functions
- **live**: provides access to state (per the mockability test)
- **inert**: not live
- **interact**: to **affect or be affected by** state
- **access**: to **interact with or expose** state
- **expose**: to provide access to state (by returning or grafting a live value)
- **graft**: to write a live value into external state
- **mint**: to expose fresh state, causing it to escape
- **pure**: no **interaction** with **external state**
- **hermetic**: no **access** to **free state**
- **identifier**: any binding or name-to-value association that can be referred to in a function definition
- **live identifier**: any identifier whose value is live
- **free identifier** (relative to a function definition): any identifier that is not a parameter or local variable
- **ambient identifier**: a free identifier available to all functions
- **ambient scope**: all ambient identifiers


### Appendix D: The Mockability Test

We want a definition of **live value** that works in *any* language, without assuming object-capability discipline or appealing to informal explanations about where authority comes from. The only thing we assume is an operational view of evaluation: running code can produce an **interaction trace**—a log of observable interactions with **existing state** (filesystem, network, clock, etc.), ignoring timing and resource usage.

#### The Trace-Projection Idea

A program’s interaction trace can be partitioned by *which state it touches*. If `S` is a particular existing state resource (or set of resources)—a specific file, socket, clock, region of memory, etc.—we write:

* `π_S(trace)` for the **projection** (restriction) of a trace onto only the events that interact with `S`.

Now we can express "this program interacts with `S`" without talking about reference graphs or authority: it simply means `π_S(trace)` is non-empty.

#### Definition of Liveness

We will allow program contexts `C[–]` to range over **expression contexts**: the hole may appear wherever an expression may appear—including in "operator position" (e.g. `C[–] = (–)(x)`), which covers both `read(x)` and method-style calls once desugared (e.g. `x.read()` selects some callable expression and then applies it).

A value `v` provides access to some existing state `S` iff:

> There exists a substitute value `v′` (admissible in place of `v`) such that, for every program context `C[–]` whose behavior does not depend on the representation identity (e.g. pointer address) of the value plugged into the hole, the projected traces  
>
> `π_S(trace(C[v]))` and `π_{S′}(trace(C[v′]))`  
>
> are the same **up to renaming of state identities**, where `S′` ranges over **fresh caller-allocated internal state** (disjoint from `S`).

And:

> `v` is live iff it provides access to some existing state `S`.

Intuitively: `v` provides access to `S` if whatever interactions a program can have with `S` can always be retargeted by swapping `v` for `v′`, to an isomorphic interaction with state that the caller allocates and controls. 

#### Why "Internal State" Matters

The requirement that `S′` must range over the caller's **internal state** is the true test of whether a value really provides access to state, and doesn't just designate it. For example, if the caller of `read(filehandle)` can redirect reads to a different file, but cannot redirect them to an in-memory mock, then the *filehandle token* is just a **designator**. The live part is whatever operation interprets that token (e.g. `read` or `filehandle.read`), because *that* is what must be substituted to retarget the interaction to caller-controlled internal state.

If, on the other hand, `read` accepts an interface/trait/function that the caller can implement, then the caller can substitute a behavioral equivalent that routes reads not just to a different file, but to any internal state the caller controls. In that case, the file handle value itself is live.

#### Liveness Implies Substitutability

The mockability test has a useful contrapositive: if a value *cannot* be substituted for a behavioral equivalent backed by caller-controlled internal state, then the value is not live. The state access it enables must be flowing through some other channel—ambient authority.

#### Pointers Are Live

A pointer to mutable state is live under this test because it is retargetable: in any context that treats pointers extensionally (not inspecting their numeric address), you can substitute a pointer to one region with a pointer to a fresh region and obtain an isomorphic interaction trace over that region. 

This remains true even in a memory-unsafe language where pointers are **forgeable** from integers. That is not a contradiction: **liveness** is about whether a value can serve as a retargetable conduit to state. **Hermeticity** is a stronger language property: it requires eliminating *ambient* ways to obtain conduits. In a language like C, the dereference operator (`*`) and integer-to-pointer casts act as ambient authority over memory.

A hermetic programming language must therefore make references **unforgeable**—some form of memory safety / capability safety—so that a program cannot conjure access to arbitrary memory out of thin air.


#### References

A live value is not necessarily a "reference". It's not clear how to define exactly what constitutes a reference in some languages. But practically, a live value must be connected to state through some sort of **reference graph**: values (nodes) that **embed** references (edges) to other values. Even a built-in function such as `now` that accesses the system clock directly via CPU instructions can be thought of as embedding a reference to the clock. And a function that calls `now` can be thought of as embedding a reference to `now`, and so on.

### Appendix E: Closures, and Methods

#### Closures

A closure that captures a live value is itself live (and therefore not hermetic). However, a function that *receives* a live closure as a parameter can still be hermetic: the state access is parameterized by the closure value. Similarly, a hermetic function can create and return a new closure, as long as that closure does not capture any live free identifiers relative to the enclosing function's body.

Since all package-scoped functions are inert in a hermetic programming language, the only way a live function can enter a program is as a locally-scoped closure or nested function that captures live local variables.

#### Methods

Methods can be thought of as functions parameterized by their receivers. A method that only interacts with state reachable through its receiver (and other parameters) is hermetic—it's the *object* that's live, not the method.

Consider an object that embeds a reference to a logger. If one of the object's methods writes to that logger (and only that logger), the method is hermetic. On the other hand, if the method hard-coded access to a global logger instead of the one embedded in the object, then the method itself would be live.

It follows that in a hermetic programming language, exported types must be hermetic in the sense that they cannot have live methods.

## Footnotes

[^di]: Martin Fowler, *Inversion of Control Containers and the Dependency Injection pattern* (2004). [link](https://martinfowler.com/articles/injection.html)

[^ocap]: Object-capability (ocap) languages enforce capability discipline at the language level: references are unforgeable, there is no ambient authority, and the only way to obtain a capability is to be given one. Examples include E, Monte, Pony, Wyvern, and Agoric's SES (Secure EcmaScript) in Hardened JavaScript. See Mark S. Miller, *Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control* (PhD thesis, Johns Hopkins University, 2006). [PDF](https://www.erights.org/talks/thesis/markm-thesis.pdf); E language: [erights.org](https://erights.org); Monte: [monte.readthedocs.io](https://monte.readthedocs.io).

[^capmyths]: The terms *ambient authority* and *capability* are formalized in the object-capability literature. See Ka-Ping Yee, Mark S. Miller, and Jonathan Shapiro, *Capability Myths Demolished* (Systems Research Laboratory Technical Report SRL2003-02, Johns Hopkins University, 2003). [PDF](https://srl.cs.jhu.edu/pubs/SRL2003-02.pdf)

[^bufconn]: The `bufconn` package provides an in-memory `net.Listener` implementation for testing gRPC services without real network I/O. [link](https://pkg.go.dev/google.golang.org/grpc/test/bufconn)

[^sansio]: Cory Benfield, *Sans-IO: Network Protocol Libraries in Python* (2017). The manifesto for writing protocol libraries as pure state machines, with I/O delegated to a separate layer. [link](https://sans-io.readthedocs.io/); Example: hyper-h2 HTTP/2 library [GitHub](https://github.com/python-hyper/h2)

[^defun]: The closures-as-objects view is the language-design analog of *defunctionalization*, a compiler transformation that replaces closures with data types carrying an `apply` method. See John C. Reynolds, *Definitional Interpreters for Higher-Order Programming Languages* (Higher-Order and Symbolic Computation, 1998; originally presented 1972). [PDF](https://link.springer.com/content/pdf/10.1023/A:1010027404223.pdf)

[^hermetic-runtimes]: By "hermetic runtime" we mean a runtime that enforces isolation for code that is not itself hermetic—e.g., senc for TypeScript or WebAssembly runtimes using WASI[^wasi], which provide capability-based sandboxes for untrusted code. [GitHub](https://github.com/fensak-io/senc)

[^paradigm]: Mark S. Miller and Jonathan S. Shapiro, *Paradigm Regained: Abstraction Mechanisms for Access Control* (ASIAN 2003: Prog. Lang. and Distr. Comp., LNCS 2896, Springer, 2003), pp. 224–242. Discusses how ambient authority pools enable virus propagation, advocating object-capability models for least authority. [PDF](http://www.erights.org/talks/asian03/paradigm-revised.pdf)

[^confused]: The confused deputy problem was first identified by Norm Hardy in 1988. See Norm Hardy, *The Confused Deputy (or why capabilities might have been invented)* (ACM SIGOPS Operating Systems Review, 1988). [PDF](https://www.cap-lore.com/CapTheory/ConfusedDeputy.html)

[^confinement]: Confinement was formalized by Butler Lampson, *A Note on the Confinement Problem* (Communications of the ACM, 1973). Revocation and its relationship to capability discipline is discussed in Miller (2006), cited above. [PDF](https://dl.acm.org/doi/pdf/10.1145/355603.361667)

[^readert]: The `ReaderT` pattern threads a shared environment through a computation via a monad transformer. When that environment carries capabilities (database handles, loggers, etc.), the pattern effectively implements implicit capability passing. See Michael Snoyman, *The ReaderT Design Pattern* (2017). [link](https://www.fpcomplete.com/blog/readert-design-pattern/)

[^isp]: The interface segregation principle—"no client should be forced to depend on methods it doesn't use"—is one of Robert C. Martin's SOLID principles. Here it converges with POLA: passing a narrow interface restricts both the API surface and the authority granted. See Robert C. Martin, *The Interface Segregation Principle* (2002). [link](https://blog.cleancoder.com/uncle-bob/2021/02/26/Interface-Segregation-Principle.html)

[^propdrilling]: "Prop drilling" refers to passing data through multiple layers of components or functions solely to deliver it to a deeply nested consumer. The term originates from the React community, where "props" are the parameters passed to components. See Kent C. Dodds, *Prop Drilling* (2019). [link](https://kentcdodds.com/blog/prop-drilling)

[^scala-context]: Scala 3 calls these *context parameters* (`using` / `given`). See [Scala 3 Reference: Context Parameters](https://docs.scala-lang.org/scala3/reference/contextual/context-parameters.html). Similar mechanisms exist in other languages: Haskell's `implicit` parameters, Kotlin's context receivers, and Rust's planned `impl Trait` in argument position.

[^tagless]: Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan, *Finally Tagless, Partially Evaluated: Tagless Staged Interpreters for Simpler Typed Languages* (Journal of Functional Programming, 2009). The pattern has since become a standard approach to effect abstraction in Haskell and Scala. [PDF](https://okmij.org/ftp/tagless-final/JFP.pdf)

[^awkward]: Simon Peyton Jones, *Tackling the Awkward Squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell* (2001). Canonical reference for "pure values that denote effectful computations." [PDF](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf)

[^observable]: As usual, we ignore changes that are not observable by normal program operations, such as internal allocation, garbage collection, caching, or timing effects. Allocating fresh state does not by itself grant access to other existing heap objects; it is internal to the call unless it escapes (assuming ordinary memory safety / no pointer-forging).

[^capsec]: Capability-based security is a discipline that promotes the principle of least authority by eliminating ambient authority. Code can’t *reach out* for resources; it can only act through explicitly provided capabilities. See Wikipedia for an overview, or Mark S. Miller's *Robust Composition* (2006) for a detailed treatment. [Wikipedia](https://en.wikipedia.org/wiki/Object-capability_model); [PDF](https://www.erights.org/talks/thesis/markm-thesis.pdf)

[^hermeticity]: Scott Herbert (slaptijack), *Benefits of Hermeticity* (2010). Defines hermeticity as the ability of a software unit to be isolated from its environment. [link](https://slaptijack.com/programming/benefits-of-hermeticity.html)

[^wasi]: WebAssembly System Interface (WASI) defines capability-based APIs for system resources, enabling precise authority requests in hermetic programs. See WASI documentation. [link](https://wasi.dev); GitHub: [github.com/WebAssembly/WASI](https://github.com/WebAssembly/WASI)

[^agoric]: Agoric implements object-capability security in JavaScript for secure smart contracts, demonstrating ocap's scalability in blockchain applications. See Agoric whitepaper and SES documentation. [link](https://agoric.com); Whitepaper: [agoric.com/papers](https://agoric.com/papers)

[^capstd]: Capability-based standard libraries for Rust, routing filesystem and networking access through passed-in handles. [GitHub](https://github.com/bytecodealliance/cap-std)

[^starlark]: Starlark is a hermetic dialect of Python used in build systems like Bazel, enforcing isolation from the environment. [GitHub](https://github.com/bazelbuild/starlark); Spec: [docs](https://bazel.build/docs/starlark)

[^wuffs]: Wuffs (Wrangling Untrusted File Formats Safely) is a hermetic language for parsing file formats, isolated in ACID sense with no allocation or panics. [GitHub](https://github.com/google/wuffs)

[^structured]: Structured concurrency ensures all spawned threads complete before a function returns. See Nathaniel J. Smith, *Notes on structured concurrency, or: Go statement considered harmful* (2018). [link](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/)

[^protection]: Jerome H. Saltzer and Michael D. Schroeder, *The Protection of Information in Computer Systems* (Proceedings of the IEEE, 1975). Foundational principles including least authority. [PDF](https://www.cs.virginia.edu/~evans/cs551/saltzer/)

[^attenuate]: In capability systems, to *attenuate* means to derive a new capability with reduced authority, such as creating a read-only handle from a read-write one. See Mark S. Miller, *Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control* (2006), Chapter 4. [PDF](https://www.erights.org/talks/thesis/markm-thesis.pdf)


