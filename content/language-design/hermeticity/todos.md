

don't use haskell for the world god object example

enables interaction instead of provides accesss

# TOdos

Make sure to emphasize that Object-Capability is a security discipline, while Hermeticity is the language property that enforces it. You mention this, but bringing it to the forefront will make the essay highly attractive to capability researchers. Hermeticity is the compiler-enforced "no ambient authority" rule.


Your argument that a hermetic language requires an inert ambient scope is fantastic. However, the Go example used to illustrate a "live package" uses global mutable state (var count int = 0).



operational/non-operational soon
#hirdwiring graph that includes both non-functions and functions

inert ambient scope -- deref operator



----

    consistency with I and we

    The implementation of a pure function may 1) allocate new temporary state 2) call impure functions **as long as they are hermetic**.

    test: if you can compose with hermetic function to get impure function


    # hermeticity in the small and in the large

        Eliminating access to ambient authority is hermeticiy "in the large". 

    you can take photographs of photographs

    shoot -- deref operator is live. But so is pointer. How can that be?
    another probelm with def of live via mockability: functions that take concrete unmockagle types


#    incorporate mTL / tagless-final, briefly in hermetic haskell




    captures => closes over

    early on -- maybe near the grid: Purity restricts what a computation can do. Hermeticity restricts where its authority can come from (previewing capability security).


    Define reference a function that returns a live reference is not a reference, but it is live
 
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


    ocap

        Hermetic programming lifts this restriction from a security mechanism to a semantic principle

        Under call-bound hermeticity, authority is synonymous with reachability. 

        an object-capability language generalized to treat all interactions (not just security-sensitive ones) as capabilities.

        Hermetic programming does not introduce new authority concepts; 

        Capability security can be understood as a restriction on which interactions a computation may perform. Hermetic programming generalizes this idea by treating all interaction with the world—not only traditionally “security-sensitive” resources—as authority that must be explicitly granted.

        Hermeticity treats every non-lexically-provided dependency as authority—whether it lives in the OS, the runtime, the process, or the heap

        environment variables, signals, global registries, singletons, thread-locals, RNGs, clocks, and hidden mutable heap objects all become the same kind of thing: ambient influence.


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


---