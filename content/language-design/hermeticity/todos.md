Where I say we could also expose state by grafting, the problem with that is we're not assuming a Hermetic programming language, so a function could expose state by writing to ambient channels. What we should really say is that it can expose state by writing to ambient channels or whatever. I'll give you some examples, but then this... we lose our opportunity to introduce grafting, so we need to re-evaluate. 


bring back sans-io.



use cap-std instead of rust as example.


A function cannot amplify authority because it has no authority except what was passed in.

(e.g., how it differs from Wuffs' hermeticity or Agoric's ocap) could be deeper—Appendix A starts this but doesn't contrast enough.

"effects wihtout implicit side effects"



Robust composition paper in footnotes three times.  
--

<!--

This sort of proactive restriction helps prevent **confused deputies**[^confused], where a function uses the capabilities it was given—accidentally or maliciously—to do something it wasn't meant to do.
-->


<!--
So pointers can be live even in unsafe languages. But a hermetic language must prevent forging of live references (which typically implies some form of memory safety / capability safety).-->

<!--TODO: Of course, where does the actual `net.Listener` come from? -->


<!--

-->


<!--

In many Object capability (ocap)[^ocap] languages, "no ambient authority" implies an inert ambient scope -- 

-->

----

Property F: Access-Controlled Delegation Channels

attenuation

all functions hermetic = inert ambient scope + no live closures

Work in Agoric


todo; global singletons, static class members, 

ref for react contexts

[TODO: *-properties]

better intro text:
    [Capability-based security](https://en.wikipedia.org/wiki/Object-capability_model) 


# TOdos

TODO: 
The WASI standard 
WASM is a hermetic environment?
not just webasm



operational/non-operational soon
#hirdwiring graph that includes both non-functions and functions

inert ambient scope -- deref operator



----

    consistency with I and we

    The implementation of a pure function may 1) allocate new temporary state 2) call impure functions **as long as they are hermetic**.

    test: if you can compose with hermetic function to get impure function




    hermeticity in the small and in the large

        Eliminating access to ambient authority is hermeticiy "in the large". 

    shoot -- deref operator is live. But so is pointer. How can that be?
    another probelm with def of live via mockability: functions that take concrete unmockagle types


    Define reference a function that returns a live reference is not a reference, but it is live

    proofs under operational semantics
        hermetic functions are inert values

        behavioral referential transparency iff inert ambient scope
        live values and substitutability / mockability test
        f(x) and f(y) have isomorphic traces of behaviorally equivalent

        identity function is pristine
        hermetic functions can mint state
        hermetic functions are closed under composition



    Show how minted state can be returned and then returned again.  Doesn’t infect caller by tying it to state. So when practicing hermetic programming don’t be afraid of retuening live values, whether minted or merely transmitted. Even closures around newly minted state


    ocap

        an object-capability language generalized to treat all interactions (not just security-sensitive ones) as capabilities.


        Hermeticity treats every non-lexically-provided dependency as authority—whether it lives in the OS, the runtime, the process, or the heap

        environment variables, signals, global registries, singletons, thread-locals, RNGs, clocks, and hidden mutable heap objects all become the same kind of thing: ambient influence.


    in haskell, newIoBuf, readIoBuf are inert?? No but tagless final functions are.
        property of a pure test function

    behavioral referential transparency
        similar to liskov substitution
                Preconditions: Subclasses cannot strengthen preconditions (require more specific input) than the superclass.
                Postconditions: Subclasses cannot weaken postconditions (guarantee less output) than the superclass.
                Invariants: Subclasses must maintain the invariants (properties always true for the class) of the superclass. 

        can replace a value with a reference to a value *with the same state* (but what about concurrency)

    todo: all state interaction happens through hermetic functions

---