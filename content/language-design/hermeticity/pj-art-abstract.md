---
# Structured 500-word abstract for The Art, Science, and Engineering
# of Programming (Art track). Sidecar only: do not use this to overwrite
# the essay voice in abstract.md. Required at submit time; not submitted here.
#
# Deadline aimed at: 1 Oct 2026 AoE. No APC. Do not submit.
abstract: |
  **Context.** Most programming languages let any function reach ambient state—globals, built-ins, and system calls. Hidden access undermines local reasoning, testing, portability, and security. Functional programming, dependency injection, and object-capability security each discipline some aspect of state, but they are often treated as separate traditions.

  **Inquiry.** What semantic property of functions and languages makes inversion of control and explicit authority flow the same thing? Prior work already named the function-level restriction: stoicity (Liu, Stucki, Amin, Giarrusso, and Odersky). This essay does not claim that property as new. The remaining question is inertness as a value-level distinction orthogonal to purity, and what follows if the program’s main entry point is stoic.

  **Approach.** This essay is an expository synthesis. It keeps a simple operational vocabulary—access versus interaction, live versus inert—and lifts the function-level rule to a language-level design point: an inert ambient scope and a stoic main. It applies that distinction to both imperative and purely functional settings, including effect values that are pure yet live.

  **Knowledge.** Purity restricts interaction with observable state; hermeticity, which is stoicity at the function level, restricts access. A function can be pure and still hard-wired to the real console, clock, or filesystem. A hermetic function can be impure if it interacts only through parameters. Live values provide access to state; inert values do not. Mutability is not the axis. Making main stoic forces packages and the ambient scope to be inert, so authority enters only through explicit interfaces. Capture checking is more flexible than this syntactic discipline; it is not claimed moot, and it is not the topic.

  **Grounding.** The argument is definitional and comparative. It is illustrated with small programs in TypeScript, Go, Python, and Haskell, and situated against stoicity, Gordon’s use/mention distinction, Joe-E, Melicher and colleagues’ pure modules, and common library patterns such as Go’s net/http, Rust’s cap-std, and sans-I/O. An appendix characterizes liveness with a mockability test that does not assume an object-capability model.

  **Importance.** Treating inertness as orthogonal to purity gives a shared vocabulary across paradigms and explains why capability discipline still matters in languages such as Haskell. A stoic main is a simple whole-program baseline: no ambient authority, function signatures as dependency manifests, and benefits for reasoning, testing, portability, and security.
---
