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





# TOdos

although go's http library is not hermetic, *it very easily could be* without losing functionality* or becoming noticeably more "noisy"

operational/non-operational soon
hirdwiring graph that includes both non-functions and functions

remove pristine? use it for hermetic+non-operational

inert ambient scope -- deref operator




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


    # hermeticity in the small and in the large

        Eliminating access to ambient authority is hermeticiy "in the large". 

    you can take photographs of photographs

    shoot -- deref operator is live. But so is pointer. How can that be?
    another probelm with def of live via mockability: functions that take concrete unmockagle types



    incorporate mTL / tagless-final, briefly in hermetic haskell




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



    captures => closes over

    early on -- maybe near the grid: Purity restricts what a computation can do. Hermeticity restricts where its authority can come from (previewing capability security).


    Define reference a function that returns a live reference is not a reference, but it is live
    Once a language becomes hermetic, or given a guarantee rhat a function is hermetic, 
    providing access becomes equivalent to granting authority. 
 
        the consequences of taking this to the extreme are suprising, and we'll explore in this essay

    a pure function can return a "dirty" function. IN a hermetic programming language this is not possible: all pure functions are pristine.

    proofs under operational semantics
        hermetic functions are inert values

        behavioral referential transparency iff inert ambient scope
        live values and substitutability / mockability test
        f(x) and f(y) have isomorphic traces of behaviorally equivalent

        identity function is pristine
        hermetic functions can mint state
        hermetic functions are closed under composition


    Show how minted state can be returned and then returned again.  Doesn’t infect caller by tying it to state. So when practicing hermetic programming don’t be afraid of retuening live values, whether minted or merely transmitted. Even closures around newly minted state

    consistency with I and we

    ocap

        Hermetic programming lifts this restriction from a security mechanism to a semantic principle

        Under call-bound hermeticity, authority is synonymous with reachability. 

        an object-capability language generalized to treat all interactions (not just security-sensitive ones) as capabilities.

        Hermetic programming does not introduce new authority concepts; 

        Capability security can be understood as a restriction on which interactions a computation may perform. Hermetic programming generalizes this idea by treating all interaction with the world—not only traditionally “security-sensitive” resources—as authority that must be explicitly granted.

        Hermeticity treats every non-lexically-provided dependency as authority—whether it lives in the OS, the runtime, the process, or the heap

        environment variables, signals, global registries, singletons, thread-locals, RNGs, clocks, and hidden mutable heap objects all become the same kind of thing: ambient influence.


    test: if you can compose with hermetic function to get impure function

    benefits
        don't need to create a hermetic runtime for python or TS - already hermetic


    TODO: Haskell code with more murky local reasoning.

    in haskell, newIoBuf, readIoBuf are inert?? No but tagless final functions are.
        property of a pure test function

    behavioral referential transparency
        similar to liskov substitution
                Preconditions: Subclasses cannot strengthen preconditions (require more specific input) than the superclass.
                Postconditions: Subclasses cannot weaken postconditions (guarantee less output) than the superclass.
                Invariants: Subclasses must maintain the invariants (properties always true for the class) of the superclass. 

        can replace a value with a reference to a value *with the same state* (but what about concurrency)


    todo: all state interaction happens through hermetic functions
    hermetic primitives

    The implementation of a pure function may 1) allocate new temporary state 2) call impure functions **as long as they are hermetic**.

    TODO existing hermetic languages: sans, sans-IO 

---