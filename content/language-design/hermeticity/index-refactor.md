---
title: "Hermetic Programming"
slug: "hermetic-programming"
# image: image.png
date: "2025-12-26"
weight: 10
math: true

---

<style>
    .image-with-caption {
        display: block; margin-left: auto; margin-right: auto; max-width: 650px

    }
</style>


## Introduction

In most programming languages, any function can reach out and touch the world: read the clock, write a file, open a socket. But this interaction with **ambient state**—through imports, singletons, globals, built-in functions—can make testing brittle and reasoning murky. You run a test once and it passes; run it again and it fails because the clock advanced or a temporary file wasn't deleted.

The practice of **dependency injection** can help tame access to state: don't let code "reach out" for resources; pass them as parameters instead.

**Hermetic programming** is dependency injection taken to its logical extreme: **parameterization of all access to state**.

Hermetic programs cannot directly import any value or function that enables access to state. This forces complete inversion of control: **all references to external state must be injected** by the runtime—for example by passing them as parameters to main.

```java
    // "io" exports a Console interface, but not a Console object
    import Console from "io"

    // The runtime passes Console object to main
    fn main(console: Console): Int {
        console.println("Hello, World!")
        return 0
    }
```

A **hermetic function** is one that **only accesses existing state through its parameters.** Hermetic functions can allocate temporary state during the call, such as mocks or mutable data structures. And they can of course pass references to this temporary state to other hermetic functions. Deterministic time? Pass a fake clock. Sandboxed output? Pass a mock filesystem. The caller maintains complete control.

Hermetic programming means that every potential access to state is visible at the call boundary. All interaction happens through hermetically sealed channels. There are no "leaks".

Effects, without ambient "side-effects".

## Overview / Summary

Hermetic programming is not FP-lite. **It's not a compromise** that relaxes the discipline of functional purity in exchange for some guardrails around access to state. Hermetic programming is independent and complementary to functional programming. 

That's because functional programs still do I/O—pure FP just requires modeling effects as values using things like IO monads. But this doesn't prevent people from creating purely functional programs that hard-code their dependencies: resulting in code that can't be tested with mocks, or build-systems that are not reproducible.

In this essay we'll introduce hermetic as another, independent dimension of "clean". A function can be pure, hermetic, neither, or both. To see how, we'll start by introducing the **purity loophole** and show how a function can be technically pure while still exposing access to existing state.

## Pure and Hermetic Functions

### The Purity Loophole

Suppose I have a function `getClock(): Clock` that, instead of returning the system clock time, **returns a handle to the system clock** (a value that implements a `Clock` interface). `getClock` is pure: it has no effect on the clock, and since it always returns the same handle, it is referentially transparent.

Suppose I also have a function `getTime(clock: Clock): Time`, which takes a `Clock` (either the real clock or a mock) and returns that clock's time. I can now get the current time from the system clock by calling `getTime(getClock())`.

`getTime` is hermetic because it doesn't read or write any state other than the clock passed as a parameter. `getClock` is pure. But the resulting composed function `now = getTime ∘ getClock` is *neither hermetic nor pure*.

How can this be? A hermetic function by definition can't "reach out" and access the clock. And a *pure* function is even...purer...then a hermetic function, isn't it?

The problem is that `getClock`, while being technically pure, is tainted by **access** to existing state. By returning a reference to the real clock, it **exposes** that access.

### Hardwiring to State

`now` and `getClock` can be thought of as being **hard-wired** to state. They both access the system clock and you can't pass a parameter that redirects that access to some other clock. `getTime` on the other hand parameterizes its access to state and is not hard-wired to any clock.

<div class="image-with-caption">

<img id="figure1" src="clock-and-wires-3.png"
  alt="Figure 1. Illustration of hermetic vs non-hermetic functions. "/>

<div>
    <strong>Figure 1</strong>. Illustration of hermetic vs non-hermetic functions. Non-hermetic functions are hard-wired to state.
</div>

</div>


### Redefining Purity

The `now` function is impure, even though it has no effects. That is because it violates the other criteria for pure functions: **referential transparency** (same inputs => same output). `now` takes no arguments yet returns a different value each time, which means something other than its explicit inputs is determining its output: `now()` is **affected by state** (the clock) that changes between calls.

**Affecting and being affected by** state are two ways of **interacting** with state. If we define *state* to mean **anything that a computation can affect or be affected by**, then it seems that **what makes a function impure is interacting with state**.

Now, a function can still use **temporary** state in its computation and remain pure. Purity is only violated by interaction with state that **survives** the function call.

So we now have a new way of defining a pure function:

> **A function is *pure* if it does not interact with surviving state.**

### Interaction vs Access

The `getClock` function is still pure, even though it **exposes** a reference to the clock. That's because it does not actually **interact** with the clock.

So **accessing** state is more general than **interacting**: **accessing** state is **interacting with or exposing** it.

Hermeticity is defined in terms of access to state, not interaction. As a result hermeticity is both more and less strict than purity. While hermetic functions *can* interact with state, they cannot *access* (expose) non parameter-referenced state like pure functions can.

### Summary of Definitions

Let’s define **free state** as non parameter-referenced, surviving state. We can now summarize the definitions of *pure* and *hermetic* functions as:

* **pure**: does not **interact with surviving state**
* **hermetic**: does not **access free state**

A function that is both pure and hermetic is **pristine**.

<div class="image-with-caption">

<img id="figure2" src="pure-hermetic-grid.png"
  alt="Figure 2. A comparison of example functions along the axes of interaction (pure/impure) and access (hermetic/non-hermetic)" style="width: 25em;"/>

<div>
    <strong>Figure 2</strong>. A comparison of example functions along the axes of interaction (pure/impure) and access (hermetic/non-hermetic).
</div>

</div>

## Live and Inert Values

### Providing Access to State

When we say `getClock` exposes state, we mean it exposes a value that **provides access to state**: a pointer, an object reference, etc.

Providing access to state is different from merely **designating** state. A file name does not, by itself, provide the ability to interact with the file system. If the language provides a global `open` that can open arbitrary files, then **`open` is what provides access to state**, while the file name merely influences *which* file is accessed.

In the appendix we define the [The Behavioral Substitute Test](#the-behavioral-substitute-test) for determining unambiguously whether a value provides access to state.

### Live Values

A value that provides access to state, directly or indirectly, is a **live value**.

Live values can include:

* Object references / pointers
* Handles (file, socket, DB, clock)
* Impure functions that interact with existing state
* Pure functions that expose existing state
* Primitives that access state
* Closures that capture live values
* Structs/tuples embedding live values

A value is live if it provides any sort of path that the caller can use to access state: a pointer to a function that returns a function that returns a reference to a mutable struct, etc.

Live values are like live wires: they are connected to state. A value becomes live once it is connected to another live value. And if you plug a live value into a hermetic function, you can cause an interaction.

Values that don’t provide access to state are **inert**: isolated from state.

### Hermetic Functions are Inert

Hermetic functions are exactly those functions that are inert values. They can only access state through live values passed as parameters. No live value is hard-coded in their definition, so they are not hard-wired to state.

A function stops being hermetic only if the function definition captures a live value that is **free**—neither a local variable nor a parameter—causing the value to be embedded in the function.

Passing a hermetic function to another hermetic function cannot give the latter access to state. So hermetic functions are **closed under composition**.

**Any function that is not hermetic is live**—even if it is pure—as is any primitive, system call, global constant, built-in, etc. that interacts with or provides access to external state, such as a reference to standard output or a primitive that returns the clock time.

### Methods

Methods can be thought of as functions parameterized by their receivers. Methods that only interact with their receivers are inert. A method with a "side-effect" that interacts with something else is live.

Consider an object that embeds a reference to a logger. If one of the object's methods writes to that logger (and only that logger), the method is still hermetic. The object itself is the live value that provides access to the logger, not the method. If the object embedded a different logger, the logs could be routed to disjoint state.


## Hermetic Programming Languages

For a programming language to be hermetic, it must prevent access to ambient state. This can be done by eliminating all **live ambient identifiers** from the language.

### Free Identifiers

A function stops being hermetic if it references a **free identifier** that is bound to a **live value**.

A **free identifier** is simply any name appearing in a function that is not a parameter or a local variable. If a free identifier refers to an **inert value** (like a math constant `PI` or a pure function `sqrt`), the function remains hermetic. However, if it refers to a **live value** (like a global `stdout` or a system `clock`), the function is not hermetic.

Compare the two examples below:

<div class="image-with-caption">

<img id="figure3" src="inert-live-code-example.png" alt="Figure 3. Example of hermetic vs a live function definition."/>

<div>
    <strong>Figure 3</strong>. Example of hermetic vs a live function definition. In both examples, `hello` references the live identifier <strong>console</strong>. But in the hermetic example, <strong>console</strong> is a bound variable (parameter), whereas in the live example, <strong>console</strong> is a free identifier bound to a live value.
</div>

</div>

### Inert Ambient Scope

**Ambient identifiers** are names that are available to all functions and modules by default. These are the identifiers that are *just there*: default imports, preludes, built-in functions, and primitives.

Collectively, these form the **ambient scope**. In a hermetic programming language, this ambient scope must be **inert**. If the ambient scope contains even one live identifier (like a global `print` function), it allows any function to "reach out" and access external state without asking for permission.

### Inert Packages

An inert ambient scope implies **inert packages**. An import statement must not be able to introduce live values into the ambient scope.

This means every identifier exported by a package or module must be inert. Consequently, package-scoped functions cannot capture live values.

It follows that package-level globals must be immutable, inert constants! Otherwise, they would "pollute" every function that captured them, making those functions live and un-exportable.

For example, the following golang package for a singleton counter is a live package. The exported `Inc` function is live, because it captures a live free variable `count`.

**Example (go): live package with package-global state**

```go
package counter

var count int = 0

func Inc() int {
    count++
    return count
}
```

On the other hand, constructors that allocate and return **fresh** state are hermetic!

**Example (go): inert package with exported hermetic constructor**

```go
package counter

func NewCounter() func() int {
    var count int = 0
    return func() int {
        count++
        return count
    }
}
```

Inert packages can export types, interfaces, constants, methods, and inert constructors. They can still provide complex imperative algorithms that *interact* with the real world (e.g., a database client), provided the actual connection to the world is passed as a parameter. They just can't provide the connection itself.



## Payoffs

### Modular Software Architecture

Hermetic programming untangles imperative logic from the state it acts upon. This **separation of concerns** immediately satisfying many well-established software design principles: complete **inversion of control**; **reuse** and the **open/closed principle**; the same function used with different resources; all functions are **testable with mocks**. Hermetic functions **compose**.

### Sandboxing and Portability

A hermetic function may not be *pure*, but it is **pure functionality**. It is **portable**, because it has no hidden tethers to the environment (no memory addresses or db handles). It can be serialized and sent "over the wire" to run in a completely different environment—such as a browser, a database stored procedure, or a plugin system—safely.

### Determinism

Hermeticity facilitates **deterministic code**, isolating all sources of non-determinism. No need for a separate hermetic runtime to enable repeatable builds.

### Security: Hermetic Programming and Capability Discipline

#### The Unifying Insight

**Dependency injection** and **capability security** are different disciplines, born in different communities and motivated by different problems. But they are organized around the same core constraint.

To see this, we need only make explicit what is often left implicit.

We define **state** as anything that can affect or be affected by a computation. In strict security discussions, 'state' often implies stored data (like files or databases). But hermeticity treats every source of non-determinism—environment variables, randomness, the system clock—as the same kind of thing: ambient authority.

We then define:

* A **dependency** is anything that provides access to state.
* A **resource** is anything that provides access to state.

At this point, a common thread becomes visible.

* **Dependency Injection**: All access to state must be parameterized.
* **No Ambient Authority**: All access to state must be parameterized.

These are not merely analogous. They express the same underlying constraint, using different vocabularies. Dependency injection realizes inversion of control for testability; capability systems emphasize security. Hermetic programming makes the shared principle explicit: (1) *all dependencies are dependencies on state*, and (2) *all authority is authority over state*.

#### Access as Authority

A hermetic function *does not* access ambient state. In a hermetic programming language, it *cannot* access ambient state.

This makes access synonymous with authority. **A live value is a capability.**

Since a callee cannot interact with the world except through live values passed by the caller, the only way authority can persist beyond a call is if the callee **stores** a live value somewhere (e.g., in a mutable data structure or channel). But since hermeticity extends the "no ambient authority" rule to include **all** state, the mutable structure used to store the leak must *also* be passed as a parameter.

So the caller must explicitly provide the capability to leak the authority.

Consequently, higher-order security concerns like *leaks,* *confinement,* and *revocation*—where the principle of least authority is applied to *delegation itself*—reduce to **graph discipline**. If the only authority a caller passes is a read-only `Filehandle`, there is no path in the reference graph by which the callee can export or leak that authority. Even revocation becomes a question of topology: granting authority via a severable proxy rather than a direct reference.

Further, many restrictions on delegating authority can be encoded in types: you can't leak a `Filehandle` unless you have access to a channel typed to accept a `Filehandle`.

Stronger object-capability security discipline can impose further restrictions—for example, by prohibiting the storage of live values in shared data structures, or enforcing linear or affine ownership disciplines that prevent duplication of authority.

Hermetic programming provides the foundation of capability security—the elimination of ambient authority and control over delegation—while leaving room for stricter delegation disciplines to enforce confinement policies within that graph.


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

#### Local Reasoning

The BRT property facilitates **local reasoning**: minimizing the number of things a programmer needs to keep in mind to understand a fragment of code.

Pure functional programming achieves a strong form of local reasoning by eliminating interaction with surviving state. That can reduce mental clutter to the point that reasoning becomes *algebraic*.

Consider a stack in a pure language (`push` returns a new stack):

```
y = pop(push(stack, x))
```

It is immediately clear that `y == x`: it follows from the semantics of `push` and `pop`. And the reasoning is completely local—nothing outside the expression can affect the result.

Now consider the same idea in an imperative language, where `push` mutates the stack:

```
push(stack, x)
y = pop(stack)
```

It is still obvious that `y == x`. The only difference is what you track: in the functional version you track an intermediate **value** (`push(stack,x)`), while in the imperative version you track an intermediate **state** (the contents of `stack`). Either way, reasoning stays local.

This local reasoning relies on the assumption that push and pop are hermetic: they don’t access any **hidden** state. The rewrite stops being obviously valid the moment `push` or `pop` can consult a global, log to a singleton, or touch the clock.

Hermeticity is orthogonal to purity, so it can improve local reasoning even in a pure language: modeling effects as values doesn’t automatically make those effects easy to reason about if code can still reach out to ambient resources.

#### An Alternative to the von Neumann Style

Backus argues that conventional languages reflect a von Neumann "machine picture": a vast ambient store plus special I/O primitives, encouraging programs that read, mutate, and write back state "one word at a time". Functional programming abstracts away that machine by making programs algebraic. Hermetic programming abstracts it away in a different way: the implicit “machine” stops being an invisible input, and interaction with state is routed through a small number of explicit interfaces with clear semantics—so programs read less cluttered traffic through the von Neumann bottleneck, and more like pipelines over well-named channels.


## Hermetic Haskell: A Pristine Programming Language


In pure functional programming languages like Haskell, all functions are pure, but they are not necessarily all hermetic, because Haskell does not force injection of all dependencies that access state.

In Haskell, evaluating `main` gives you a value of type `IO ()`. An `IO ()` is like a compiled program: it is a value that can be executed. To the runtime, it is a lot like a closure.

And crucially, when this program is run, it **directly accesses** the filesystem, console, etc. It's access to state is not parameterized: it can't be unplugged and plugged into a mock. It is hard-wired to state.

So in Haskell, **`main` is live**

To see this, consider the "Hello, World!" program in Haskell

```haskell
module Main where

import System.IO (putStrLn)

-- main is a value of type IO ()
-- It describes the action, but doesn't execute it yet.
main :: IO ()
main = putStrLn "Hello, World!"
```

Now compare it to a semantically equivalent program in Typescript

```typescript
// Let's define IO simply as a function waiting to be run
type IO<T> = () => T;

// putStrLn returns an IO action (a thunk)
const putStrLn = (msg: string): IO<void> =>
    () => console.log(msg);

// main is a value of type IO<void>.
// It describes the action, but doesn't execute it yet.
export const main = putStrLn("Hello, World!");
```

In both programs, calling `putStrLn` didn't do anything: it just returned a value that can be executed: an `IO ()` in the Haskell version and an `IO<void>` in the Typescript version. Yet in both cases `main` is live, directly tied to the actual console.

<div class="image-with-caption">

<img id="figure4" src="putstrln-example.png"
  alt="Figure 4. Dependency graph for Hello World! program where main is live."/>

<div>
    <strong>Figure 4</strong>. Dependency graph for Hello World! program where main is live because it is
    ultimately hard-wired to the system console.
</div>

</div>

It is counter-intuitive to think of `main` as "live" in Haskell, because it is a "pure value" that describes an action. But consider what action it describes.

The `IO` action returned by `putStrLn` can be thought of as a sealed envelope containing an instruction. In standard Haskell, that instruction is effectively: "Call the operating system's standard output." The reference to the real standard output is hard-coded into that envelope. You, the programmer, cannot swap that instruction for "Write to this memory buffer" without changing the definition of `putStrLn` itself. So the `IO` value is live because it contains an immutable, hard-wired reference to the ambient world.

`main` is also live because it is hard-wired to the standard library's `putStrLn`.

A hermetic version of main would instead say: "Call the output device provided in the arguments."

```haskell
-- A tiny console interface
data Console = Console
  { putStrLn :: String -> IO ()
  }

-- Hermetic main program: message written through injected console
main :: console -> IO ()
main = putStrLn console "Hello, World!"
```

In a hermetic version of Haskell ("Hermetic Haskell") all live functions would have to be moved out of standard libraries, making imports inert. Live functions could then be moved into a single `World` dependency with fields corresponding to the standard libraries.

```haskell
-- A world record that mirrors familiar library groupings.
-- Each field is a record of the effectful operations that library normally provides.

data World = World
  { systemIO   :: SystemIO
  , systemDir  :: SystemDirectory
  , systemTime :: SystemTime
  , systemEnv  :: SystemEnvironment
  , netHTTP    :: NetworkHTTP
  , netSocket  :: NetworkSocket
  , random     :: Random
  }

-- System.IO (live operations that touch the real console/files)
data SystemIO = SystemIO
  { putStrLn  :: String -> IO ()
  , getLine   :: IO String
  , readFile  :: FilePath -> IO String
  , writeFile :: FilePath -> String -> IO ()
  , appendFile :: FilePath -> String -> IO ()
  }
```

The program logic would then be moved into a hermetic function into which the real world could be injected.

```haskell
-- The program is parameterized over the world
program :: World -> IO ()
program w = do
  putStrLn (systemIO w) "Hello, World!"
```

The caller can route the output to any device, including a mock for testing.

```haskell
import Data.IORef

testProgram :: IO Bool
testProgram = do
    -- 1. Create a mutable reference to capture output
    outputRef <- newIORef []

    -- 2. Define the mock behavior
    -- Instead of writing to stdout, we append the message to our reference.
    let mockPutStrLn msg = modifyIORef outputRef (\logs -> logs ++ [msg])
    let mockConsole = Console { putStrLn = mockPutStrLn }

    -- 3. Inject the mock into the program
    main mockConsole

    -- 4. Verify the captured side-effects
    captured <- readIORef outputRef
    return (captured == ["Hello, World!"])
```

For compatibility with the host runtime, build system, and tooling, Hermetic Haskell programs might include an implicit `main` that does nothing but inject `program` with a live `world` value.

```haskell
-- Implicit live `main` that binds the program to the real world
main :: IO ()
main = program world
```


## Design Space and Ergonomics

### Managing the Wires

Hermetic Programming requires more "wires". Parameters must be threaded through the call stack to reach all functions that need them ("prop-drilling", "parameter pollution").

This can add noise to function signatures and complicate refactoring, especially for cross-cutting aspects of a program such as logging.

Reducing the number of wires can tempt programmers into practices that undercut many of the advantages of hermetic programming.

#### The God Object (Not Recommended!)

If `main(world)` is injected with a "world" object with subfields for all external resources (`world.clock`, `world.fs`, `world.net`...), it can just pass this down through the call stack. So every function always takes `world` as the first parameter and has access to everything.

This is still hermetic: callers still have complete control. They can still mock the world or parts of it.

But this is very messy. It is not clear from a function signature what part of the world it is interacting with. It violates the **interface segregation principle** by forcing functions to depend on tools they don't use. It also makes testing difficult, as testing a simple function requires mocking the entire world.

#### The Principle of Least Access

So Hermetic Programming's benefits are limited if it is not practiced in conjunction with the **principle of least access** (or principle of least authority). A function should require the minimum access to state necessary for it to do its job.

For example if (`main`) receives the whole world, it should immediately unwrap it to pass only specific access to sub-functions.

```haskell
-- Main receives the whole world...
main :: World -> IO ()
main world = do
    -- But only passes the console to helloWorld
    helloWorld (console world)
```


#### Contexts

To reduce the number of explicitly-passed parameters, a language can support **contexts** (also known as implicits).

In this pattern, the "wires" are hidden from the function *body*, but they usually remain visible in the function *signature*. Here's an example in Scala:

```scala
trait Logger {
    def info(msg: String): Unit
}
 
// 1. The Entry (Main): Injects the context
// The 'given' keyword places an instance into the implicit scope
@main def main(): Unit = {
    given consoleLogger: Logger = new Logger {
        def info(msg: String) = println(s"[LOG] $msg")
    }
    // We call foo() without manually passing the logger argument
    foo()
}

// 2. The Middleman (Foo): Carrier of the context
// Foo does not use Logger, but must declare 'using Logger'
// to allow it to pass implicitly to Bar.
def foo()(using Logger): Unit = {
    bar()
}

// 3. The Leaf (Bar): Consumer of the context
// Bar explicitly states: "I can only run if a Logger is in context."
def bar()(using logger: Logger): Unit = {
    logger.info("Called bar")
}
```


### Closures

Does hermetic programming forbid (live) closures? Not necessarily: hermetic programming forbids access to *ambient state* through live ambient identifiers—it does not forbid access to live *free* identifiers (e.g. local variables).

A closure that captures a live free identifier is live. However, a function that receives and executes a live closure can still be hermetic: the state access is parameterized by the closure value itself. A hermetic function can also create a new closure and call it or return it, as long as that closure does not capture any live free identifiers (free wrt the enclosing hermetic function's body).

For example, the `doTwice` function below (Typescript) takes and returns a closure. But the closure it returns captures only bound variables (the `action` parameter). So `doTwice` doesn't access any state except through its parameters. So `doTwice` is hermetic.

```typescript
function doTwice(action: () -> ()) {
    return () -> {
        action()
        action()
    }
}
```

Since hermetic programming requires all package-scoped functions to be inert, the only possible way for a non-hermetic function to enter the language is as a locally-scoped closure or nested function.

However, eliminating non-hermetic functions could be a nice feature in a language: it would ensure the BRT property would hold for any expression.

On the other hand, closures have an important place in imperative programming languages.

If we want to have our cake and eat it too, our only choice is to decide that closures aren't functions. They must be objects of some sort, with a hermetic method such as `run`.

### Actions

A live closure that doesn't take any arguments is sometimes called a "thunk". A hermetic programming language might require all functions to be hermetic by forbidding closures from capturing live free variables and instead introduce an operator that defines a thunk, or what I prefer to call an **action**—a bit of logic that interacts with state, to be executed later.

For example, in a hermetic variant of Typescript, the following would be a compile error:

```typescript
var count = 0
// Not allowed! count is a live free variable
function inc() {
    count++
}
```

Instead, `inc` could be an action.

```typescript
var count = 0
// Define an action that increments the count
const inc = action {
    count++
}

// Execute the action
inc.run()

console.log(counter) // output: 1
```

The `run` method itself is hermetic—its access to state is entirely parameterized by its receiver.

The `doTwice` method from the earlier example might look like this, with type annotations added:

```typescript
function main(console: Console) {
    function doTwice(a: Action<void>): Action<void> {
        return action {
            a.run()
            a.run()
        }
    }

    const twiceHello: Action<void> = doTwice(do { console.println("Hello, World") })
    twiceHello.run()
}
```

Now notice something interesting: the `doTwice` and `twiceHello` functions are pure! This is starting to look a lot like an effect system: an `Action` looks a lot like an `IO`.

There are some interesting places a language designer could go from here.


## Etymology of Hermeticity

> **Hermeticity is a software engineering concept that refers to the ability of a software unit to be isolated from its environment.**
>
> -- [Scott Herbert (slaptijack)](https://slaptijack.com/programming/benefits-of-hermeticity.html)

The term **hermetic** has been used in the past to describe a number of systems that isolate imperative code from its environment: most notably Google's [**hermetic testing**](https://carloarg02.medium.com/how-we-use-hermetic-ephemeral-test-environments-at-google-to-reduce-test-flakiness-a87be42b37aa), [**hermetic servers**](https://testing.googleblog.com/2012/10/hermetic-servers.html),
hermetic builds ([**bazel**](https://bazel.build/basics/hermeticity)), and hermetic languages ([**wuffs**](https://github.com/google/wuffs#readme) and [**starlark**](https://github.com/bazelbuild/starlark)). These all **contain** effects to specifically authorized state: a test mock, a build artifact, the function's arguments; although in some cases they enforce a more strict notion of isolation: wuffs for example [cannot panic or throw exceptions](https://skia.googlesource.com/external/github.com/google/wuffs/+/HEAD/doc/note/hermeticity.md).

Although the term *hermeticity* implies isolation of code from its environment, I've appropriated the term *hermetic* here to mean isolation with respect to *access to state*.

## Conclusion

So Hermetic programming ties together some common threads in software design patterns with respect to **access to state**: the principles of **isolation** (hermetic builds/mocking), **parameterization** (DI/IoC/ports-and-adapters), and elimination of **ambient access** (capability-based security), and turns it into a semantic rule: **a function may only access existing state through its parameters**.

This cleanly separates two ideas that are often conflated:

* **Purity** is about *interaction* with surviving state.
* **Hermeticity** is about *access* to free state—interaction **or exposure**.

Once you take access seriously as the primitive, a lot of familiar practices snap into place. A pure FP language can still hard-wire effect values to the real world, which is why “Hermetic Haskell” is even a coherent phrase. A language with an **inert ambient scope** has no ambient authority -- **live value is a capability**. And for hermetic code, **Behavioral Referential Transparency** becomes the right generalization of referential transparency: no hidden inputs, therefore no hidden behavioral differences.

---


<!--

-----


### Premature Abstraction

On the other hand, parameterizing all access to state might be seen as premature abstraction. Maybe you just want a quick script that modifies some local files; generalizing it to work with an abstract Filesystem interface seems like overkill.

However, there is actually very little difference in complexity between a program that accesses the filesystem by importing it and one that accepts it as as parameter. In both cases, the filesystem must be identified as a dependency: its just a question of replacing an import statement with a parameter name. 
rule of three?



---


### Three Definitions of a Hermetic Function

We now have three ways of defining a hermetic function, based on:

1. its **behavior**: does not access existing state except through its parameters
1. its **value**: does not embed any reference to state
1. its **definition**: does not capture any live free values

These can all be tied together by the idea that, when the definition of a function **captures a live free value** it causes the live value to be **embedded into the function**, enabling that function to directly **access state except through its parameters**.
-->
