
## Taxonomy of State Access (extracted from Appendix B)

These detailed sections were extracted from the hermetic programming essay. They may form the basis of a separate essay on the taxonomy of state access patterns.

### Leaking State

When a function such as `getClock(): Clock` exposes ambient state, it **leaks** state.

### Grafting State

A function can expose state by writing a live value into existing state, thus making the state available to the caller, or any concurrent thread sharing the state, without directly returning it. We call this **grafting** state.
 
For example, when a hermetic function appends a node to a linked list or inserts one into a binary tree, it stores a reference to the new node in the `left` or `right` or `next` field of an existing node: it has grafted fresh state (a new node) into the existing data structure.

### Relaying State

A hermetic function cannot leak state because it cannot access free state. But a hermetic function can still expose state accessed through its parameters. A function such as `identity` which returns a live value passed to it exposes state but does not leak state. 

When a function exposes parameterized state without interacting with state, we say it **relays** state.

A function relays state if it "unwraps" a live value (takes a tuple containing a reference and returns the reference), or otherwise accesses a live value through one of its parameters. 

But **it must do so without observably interacting with state**. So if a function takes a reference to *mutable* state that embeds another reference, and returns the embedded reference, it is **not** merely Relaying state, because it had to actually read the state to expose it.

### Minting State

Also, a hermetic function can expose **new** state. So hermetic functions can be constructors.

Fresh state that is exposed will survive the function call (and if fresh state survives the function call, that's because it has been exposed). When fresh state survives the function call it is said to "escape".

Exposing fresh state (causing it to escape) is called **minting** state.

Now, if a constructor function can mint state, isn't it live? This is a subtle point, but no, a live value is one that references **existing state**. A hermetic function, considered as a **value**, does not reference any existing state, even if it can mint state. So it is inert.

Now we might argue that the heap itself is state and so minting state is interacting with existing state. But access to the heap does not imply access to all objects on the heap. Heap interaction is **isolated** from all other heap-allocated state in a way that prevents non-parameterized state access. 

### Types of Clean Function

So if we define purity as referential transparency, we arrive at the following definitions:

- **pure**: no **interaction with external state**
- **hermetic**: no **access to free state**
