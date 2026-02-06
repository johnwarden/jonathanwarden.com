

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
