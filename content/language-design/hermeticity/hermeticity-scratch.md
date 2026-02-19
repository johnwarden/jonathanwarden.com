Much confusion around functional purity comes from an implicit assumption that pure is not **operational**—that it directly defines program output without describing a sequence of steps. But modern pure languages routinely allow operational code—step-by-step descriptions of effectful computations—represented as values (e.g. an `IO ()` in Haskell).

But modeling operational code as values doesn’t shield it from the hazards of interacting with the world, such as flaky tests and non-deterministic output. Pure functional codecan still needs dependency injection (capability passing)be **live capabilities**: they can provide access to avoid hard-coding dependencies.state.


<!--

### Hermetic Programming in Haskell

A hermetic version of the Hello, World! program might require the console to be injected:

```haskell
-- A tiny console interface 
data Console = Console
  { consolePutStrLn :: String -> IO ()
  }

main :: Console -> IO ()
main console = consolePutStrLn console "Hello, World!"
```

In practice, Haskell code usually avoids manually passing capabilities down every call. A more canonical approach is to thread them implicitly using a `ReaderT` environment (the "mtl / ReaderT pattern"). The program’s *logic* runs in an `App` monad that has access to a `Console` capability, and `main` supplies the concrete implementation at the boundary.


```haskell
newtype App a = App { unApp :: ReaderT Console IO a }

hello :: App ()
-- ... uses `ask` to obtain the injected Console ...

main :: IO ()
main = runReaderT (unApp hello) (Console putStrLn)
```
-->


<!--

#### Purity as Non-Interaction

Purity is often described as "no side-effects": evaluation can’t **affect** state. But purity also rules out the converse: evaluation can’t be **affected by** state.

`now()` is impure even though it has no effects. Its result varies across calls, so something other than its explicit inputs is determining its output: `now()` is affected by the state of the clock.

The most widely accepted definition of purity is **referential transparency**: an expression can be replaced by its value in any program context without changing observable behavior. `now()` fails this test, as does any function whose evaluation has an observable effect.

Affecting state and being affected by state are both ways of **interacting** with state. So it seems that **observable interaction with external state is what breaks referential transparency.**

A pure function can use state internally during its computation (e.g. freshly allocated memory that doesn't escape) and still be pure. It just can't interact with **external state**: state that is externally observable by normal operations (excluding memory usage and timing). 

-->

<!--
So if we define purity as referential transparency, then:

> **A function is pure iff it does not interact with external state.**
-->



<!--

#### Purity as Non-Interaction

Purity is often described as "no side-effects": evaluation can’t **affect** state. But purity also rules out the converse: evaluation can’t be **affected by** state.

`now()` is impure even though it has no effects. Its result varies across calls, so something other than its explicit inputs is determining its output: `now()` is affected by the state of the clock.

The most widely accepted definition of purity is **referential transparency**: an expression can be replaced by its value in any program context without changing observable behavior. `now()` fails this test, as does any function whose evaluation has an observable effect.

Affecting state and being affected by state are both ways of **interacting** with state. So it seems that **observable interaction with external state is what breaks referential transparency.**

A pure function can use state internally during its computation (e.g. freshly allocated memory that doesn't escape) and still be pure. It just can't interact with **external state**: state that is externally observable by normal operations (excluding memory usage and timing). 

-->

<!--
So if we define purity as referential transparency, then:

> **A function is pure iff it does not interact with external state.**
-->

<!--
In fact `identity` is also pure, since it doesn't interact with state either.-->

<!--

So one function provides the ability to interact with state, the other provides the access, and the combination, `now`, is what we'll call a **dirty** function that can directly interact with un-parameterized state. 
-->


<!--
On the other hand, `getTime` is not hard-wired by anything. It's access to state is parameterized. So it is hermetic.-->


<!--
Hermeticity is defined in terms of access, not interaction. As a result hermeticity is both more and less strict than purity. While hermetic functions *can* interact with state, they cannot *access* non-parameterized state like pure functions can.
-->

<!--
It is interaction with state that makes a function impure. A an impure function either affects state, or fails referential transparency because it *is affected by state*.
-->


<!--

```go

func runServer(ln net.Listener, lg *log.Logger) error {
    h := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Write([]byte("hello\n"))
    })

    srv := &http.Server{
        Handler:  h,
        ErrorLog: lg, // avoids default logging behavior
        // ReadHeaderTimeout: ...,
    }

    // Hermetic relative to ln: all network I/O flows through ln and accepted conns.
    return srv.Serve(ln)
}
```

The above `runServer` is hermetic. Although the `Serve` method itself....

TODO: problem. http.Server is live, but this is still hermetic! Our definition is not right. Embedding a live value is not enough to make something non-hermetic. It has to be non-trivial.
-->

https://lukasa.co.uk/2015/10/The_New_Hyper/
    Firstly, and most notably, Hyper-h2 does absolutely no I/O. It exists entirely in memory, reading to and writing from in-memory buffers. The reason for this is that it becomes possible to use this same kernel of code in any programming paradigm. If you like synchronous code, that’ll work. If you like threads, that’ll work too. If you like gevent, that’s fine. Twisted? Check. Tornado? All good. asyncio? You bet. All you need to do is write the bit around the outside that does the boring stuff of reading from and writing to sockets. Pass the data into hyper-h2, and it’ll parse it and turn it into something you can actually work with.



https://lukasa.co.uk/2015/10/The_New_Hyper/
    For example, it will almost certainly confine itself to strictly enforcing the HTTP/2 state machine: this means that it may not be a good choice for implementations that occasionally need to violate that state machine.


https://www.youtube.com/watch?v=7cC3_jGwl_U
    gien there is one and only one way to parse http
    why do libraries have their own http parsing code
    and the answer is they do IO
    they mix their IO into their parsing code

        also some http libraries in python mix in concurrecy primitives

        a better way to do IO
        this is exactly what it looks like:
            events = handle_data(in_bytes)
            out_bytes = perform_action()
            it's just bytes
            could have come by pigeon, or on a boat 




## Abstract

A pure function can return an impure function. Although such a function may be referentially transparent, it nonetheless enables its caller to interact with state. We introduce a new classification -- hermetic -- for a function that closes this "purity loophole": it cannot *access* any existing state except through its parameters. 

We then introduce the related concept of **live** values that provides access existing state, and **inert** values that do not. We provide an operational definition of liveness, instead of one based on a reference graph. We then assert two key properties of hermetic functions: they are inert when considered as values, and they are closed under composition and higher-order composition. 

We define a hermetic programming language as one where all functions are hermetic, and therefore all access to system resources must be injected as dependencies into `main`. We show that this is only possible in a language where there the ambient environment has no live free identifiers, and that this means packages/libraries can only export inert values. It also requires closures to be invoked as objects (e.g. with an `apply` method) and not functions. We include a case study of the HTTP library in Go that exports a hermetic HTTP server that parmeterizes its access to the network (TODO or sans-io).

We then touch on the architectural benefits of hermetic programming, such as separation of concerns, inversion of control, testability, and portability. We show that hermetic programming languages have the property of **behavioral referential transparency** (a function depends only on the behavior of its inputs -- no hidden inputs), which facilitates local reasoning. 

We also show how a hermetic programming language provides the capability-security baseline for a single process: it eliminates not only **ambient authority**, but also any ambient capability-bearing sinks, making confinement, and revokation graph properties. 

Finally, we discuss hermetic programming in an purely functional languages. We show how an action (e.g. an `IO ()` in haskell) is live: it smuggles authority, is hard-wired to state, and passes the mockability test. And we illustrate how Haskell functions can be made both pure and hermetic using dependency injection.

---


## Overview


This essay will illustrate these ideas in five sections:

* **Pure and Hermetic Functions (The Purity Loophole).** We’ll see how a function can be perfectly pure and yet still *expose* access to state, and illustrate the pure/hermetic distinction using a 2×2 grid with examples.

* **Live and Inert Values.** **live** values provide paths to state; **inert** values do not. We’ll then show that hermetic functions are inert values, and therefore hermeticity is closed under composition and higher-order use.

* **Hermetic Programming Languages.** We’ll define what it takes for a programming language to be hermeti: eliminate access to ambient state by requiring an **inert ambient scope**. This has immediate consequences for imports, packages, and the shape of the standard library.

* **Benefits and Costs.** With the model in place, we’ll discuss benefits: determinism, mockability, local reasoning, composability, and security via the elimination of ambient authority—along with the main cost (parameter “wiring”) and how contexts/implicits can help manage it.

* **Hermetic Programming in Functional Languages.** Finally, we’ll revisit pure languages: effect values can be *pure* yet still **live**, carrying authority when executed. Eliminating ambient authority in a pure language requires something stricter: a **pristine** language—pure and hermetic.




<!--

### Summary of Costs

Overall, it doesn't take much to make a new programming language hermetic. Keep live values out of the standard library -- use inert interfaces instead, and keep the concrete implementations of the filesystem, network, etc. in the runtime. Maybe make closures into objects with methods instead of functions. Add contexts/implicits to manage the wires. That's all.
-->

<!--
This pattern offers significant **Refactoring Resilience**. In a strictly wired hermetic system, if a deep-leaf function suddenly requires access to a logger, you would typically have to modify the function signatures of every caller up the stack to pass that new parameter down. This violates the **Open/Closed Principle** -- you are forced to modify intermediate code (like `foo`) that shouldn't change just to transport data.

With Contexts, you only modify the provider (`main`) and the consumer (`bar`). The intermediate function acts only as a pass-through for the control flow, not the data flow.

In this pattern, the "wires" are invisible in the middle, but explicit at the ends. The caller injects the context, and the receiver declares the need for it. The intermediate functions don't touch the wires unless they need to access that context or override it.
-->



<!--
### Hermetic Programming Language Requirements

In summary, a hermetic programming language can be thought of as one that guarantees:

- no ambient access to state
- an inert ambient scope
- behavioral referential transparency

These all come down to the same thing -- each implies the other.
-->

<!--

## Hermetic Programming and Capability Discipline

### The Unifying Insight

**Dependency injection** and **capability security** are different disciplines, born in different communities and motivated by different problems. But they are organized around the same core constraint.

In strict security discussions, "state" often implies stored data (like files or databases). But hermeticity treats every source of non-determinism -- environment variables, randomness, the system clock -- as state.

We then define:

* A **dependency** is anything that provides access to state.
* A **resource** is anything that provides access to state.

At this point, a common thread becomes visible.

* **Dependency Injection**: All access to state must be parameterized.
* **No Ambient Authority**: All access to state must be parameterized.

These are not merely analogous. They express the same underlying constraint, using different vocabularies. Dependency injection realizes inversion of control for testability; capability systems emphasize security. Hermetic programming makes the shared principle explicit: (1) *all dependencies are dependencies on state*, and (2) *all authority is authority over state*.

### Access as Authority

A hermetic programming language enforces hermeticity: a function *cannot* access any resource unless it is provided access via live values passed as parameters. This makes access synonymous with authority. **A live value is a capability.**

Since a callee cannot interact with the world except through live values passed by the caller, the only way authority can persist beyond a call is if the callee **stores** a live value somewhere (e.g., in a mutable data structure or channel). But since hermeticity extends the "no ambient authority" rule to include **all** state, the mutable structure used to store the leak must *also* be passed as a parameter. So the caller must explicitly provide the capability to leak the authority.

Consequently, higher-order capability-security concerns like *leaks,* *confinement,* and *revocation* -- where the principle of least authority is applied to *delegation itself* -- reduce to **graph discipline**. If the only authority a caller passes is a read-only `Filehandle`, there is no path in the reference graph by which the callee can export or leak that authority. Even revocation becomes a question of topology: granting authority via a severable proxy rather than a direct reference.

Stronger object-capability security discipline can impose further restrictions -- for example, by prohibiting the storage of live values in shared data structures, or enforcing linear or affine ownership disciplines that prevent duplication of authority. 

TODO: full ocap model

Hermetic programming provides the foundation of capability security -- the elimination of ambient authority and control over delegation -- while leaving room for stricter delegation disciplines to enforce confinement policies within that graph.
-->



        **operational grid**

        |         | hermetic              | live
        | -       | -                     | -
        | pure    | hello(world): IO()    | hello(): IO()
        | impure  | hello(world): void    | hello(): void


        **non-operational grid**

        |         | hermetic              | live
        | -       | -                     | -
        | pure    | sqrt(3)               | getClock()
        | impure  | sortWithTrace(list, tracer)    | sortWithTrace(list)

