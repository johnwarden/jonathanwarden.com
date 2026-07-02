Dear Professor Hermans,

Thank you for the decision letter and for coordinating the reviews. Please convey my thanks to the reviewers for their thoughtful feedback. The pointer to the Stoic paper is especially helpful and clearly changes how the essay should be framed.

A revised version would foreground that prior art and build on the concept of *stoicity* rather than introducing a new term. What would remain is an expository synthesis exploring the consequences of lifting stoicity to a language-level property, especially by making the program’s main entry point stoic. That framing leads naturally to an inert ambient environment, explicit capability flow from the program boundary, and no ambient authority by default.

The main conceptual contribution I would preserve is the orthogonality of purity and inertness: interaction versus access. Gordon’s use/mention distinction seems relevant here. Purity is a use/interaction property; inertness is a mention/access property. The distinction is especially important in functional programming languages, because a function can be pure in the ordinary FP sense while still being live—that is, while still carrying authority to the real console, clock, filesystem, network, or other state. This is why capabilities and authority safety remain relevant even in languages like Haskell.

Scala’s capture-checking documentation also illustrates why the pure/inert distinction is useful. The latest Scala docs use “pure” for types with empty capture sets. That is internally coherent in the capture-checking setting, but it overloads a term that traditionally has a different meaning in functional programming. A traditionally pure function can capture capabilities without using them and thus be “impure” in the Scala capture sense; conversely, a traditionally impure function can have effects through its parameters without capturing capabilities and thus be “pure” in the Scala capture sense.

Wyvern's “pure modules” appear to closely correspond to what my essay called inert packages. Melicher et al.’s note that functions defined in a pure module may have side effects, but only if the relevant state is passed as an argument or created within the function itself. In other words, they may be impure as long as they are stoic. A revised version would summarize that concept and argue that enforcing a stoic main through an inert ambient/import scope requires available modules and packages to be pure or inert in this sense.

While the literature has described functions, values, modules, and references that convey no authority, the vocabulary is often tied to a particular paradigm or overloaded with purity. Terms like “immutable,” “read-only,” or “reference immutable” are related but not sufficient, because they describe object-state, aliasing, or object-graph properties rather than authority/access properties. Additionally, an immutable or read-only value may still convey observational authority over external state (e.g. SSH keys), and in pure functional settings “mutability” is not the right category at all.

The inert/live distinction is meant to fill that terminology gap across paradigms. It applies equally to object references, FP effect values, modules, handles, channels, built-ins, and the identifiers that refer to them. In that sense, the revised essay is not really about stoicity itself; it is about inertness, with stoicity as inertness applied to functions. I would retitle and reframe the essay accordingly.

I also agree with the reviewer’s point that capture types are more flexible than the syntactic discipline of an inert language. An inert ambient scope rules out capture of ambient capabilities, but live parameters and freshly allocated mutable state can still be captured and propagated. The revised essay would present capture checking as complementary to stoic programming.

If there is any possibility of considering a substantially revised version along these lines, I would be grateful for the opportunity. If not, I appreciate the reviews and will use them to revise the essay for a future venue.

Regardless of the committee’s decision, I would appreciate it if you could forward this response to the reviewers with my sincere thanks. Reviewer 100A’s feedback, in particular, was a gift that has already improved the trajectory of this work.

If Reviewer 100A would have any interest in a brief chat to help me further refine these ideas for a future venue, I would welcome the connection. Please feel free to share my email address with them, entirely at their discretion.

Thank you again for your time, guidance, and the rigor of this process.

Best regards,

Jonathan Warden



----

Notes on ONward! essay rejection

Stoic: https://infoscience.epfl.ch/entities/publication/7694497c-7b7c-4e1c-83d6-0f86ffedc45e
https://infoscience.epfl.ch/entities/publication/7694497c-7b7c-4e1c-83d6-0f86ffedc45e	

	Can we formulate some fundamental capability disciplines as typing rules?

	As an incarnation of this idea, we introduce stoic functions in a functional language. In contrast to normal functions, stoic functions cannot capture capabilities nor non-stoic functions from the environment	

	Capabilities are widely used in the design of software systems to ensure security. A system of capabilities can become a mess in the presence of objects and functions: objects may leak capabilities and functions may capture capabilities. They make reasoning and enforcing invariants in capability-based systems challenging if not intractable. How to reason about capability-based systems formally? What abstractions that programming languages should provide to facilitate the construction of capability-based systems? Can we formulate some fundamental capability disciplines as typing rules? In this paper we propose that stoicity is a useful property in designing, reasoning and organizing capabilities in systems both at the macro-level and micro-level. Stoicity means that a component of a system does not interact with its environment in any way except through its interfaces. As an incarnation of this idea, we introduce stoic functions in a functional language. In contrast to normal functions, stoic functions cannot capture capabilities nor non-stoic functions from the environment. We formalize stoic functions in a language with mutable references as capabilities. In that setting, we show that stoic functions enjoy non-interference of memory effects. The concept of stoic functions also shows its advantage in effect polymorphism and effect masking when used to control side effects of programs.

		"In functional programming languages, functions are typically allowed to close over arbitrary values from the environment"

		" Meanwhile, local mutations are usually regarded as harmless,
109 thus they may be masked [17, 22], in contrast to global or environmental mutations. "

		"We call non-stoic functions free functions, which can freely capture capabilities or non-stoic
144 functions from its environment. "

		"We write functions as (x: T) => t. The types of stoic functions are 164 represented by T -> R, while the types of free functions are represented by T => R. "

	The function f can be
	206 either a stoic function (Int → Int) or a free function (Int ⇒ Int) that produces effects. In this
	207 sense, the function map is effect-polymorphic. 	


	"2 A function may locally create new references and mutate them. If they are not observable
733 from outside, those effects can be masked. This is also called effect masking in the literature"

	pure types (Tpu) and impure types (Tim). Impure types include capabilities296
(Ref T) and free function types (T ⇒T).

	"We can also think of a function value as a capability if the value is a reference to an eﬀectful operation"


Gordon, “Designing with Static Capabilities and Effects: Use, Mention, and Invariants”
	https://drops.dagstuhl.de/storage/00lipics/lipics-vol166-ecoop2020/LIPIcs.ECOOP.2020.10/LIPIcs.ECOOP.2020.10.pdf	

		Especially important: capability-bound reasoning can conflate merely having/mentioning a capability with actually using it; this loses precision compared with effect systems

		abstract:
		Capabilities (whether object or reference capabilities) are fundamentally tools to restrict effects. Thus static capabilities (object or reference) and effect systems take different technical machinery to the same core problem of statically restricting or reasoning about effects in programs. Any time two approaches can in principle address the same sets of problems, it becomes important to understand the trade-offs between the approaches, how these trade-offs might interact with the problem at hand.
Experts who have worked in these areas tend to find the trade-offs somewhat obvious, having considered them in context before. However, this kind of design discussion is often written down only implicitly as comparison between two approaches for a specific program reasoning problem, rather than as a discussion of general trade-offs between general classes of techniques. As a result, it is not uncommon to set out to solve a problem with one technique, only to find the other better-suited.
We discuss the trade-offs between static capabilities (specifically reference capabilities) and effect systems, articulating the challenges each approach tends to have in isolation, and how these are sometimes mitigated. We also put our discussion in context, by appealing to examples of how these trade-offs were considered in the course of developing prior systems in the area. Along the way, we highlight how seemingly-minor aspects of type systems - weakening/framing and the mere existence of type contexts - play a subtle role in the efficacy of these systems.


	One of the most well-developed bodies of work on static capabilities uses reference capabilities,
	which associate different permissions1
	to individual references in a program, in contrast to
	the object capability view that all references to an object are equal and restrictions stem
	from using different objects with fewer or modified operations available. 


	 Use-Mention Distinction:
	 		possessing authority means only that the code has the ability to use it, not that it necessarily does. This seems to be anecdotally understood among designers of static capability systems, but rarely discussed. To the best of our knowledge, this paper is the first to explicitly call out and name this trade-off



	This can be combined nicely with object immutability [65], as all references to an immutable object are read-only.
		Yoav Zibin, Alex Potanin, Mahmood Ali, Shay Artzi, Adam Kiezun, and Michael D. Ernst.
Object and Reference Immutability Using Java Generics. In ESEC-FSE, 2007. doi:10.1145/
1287624.1287637.


	They have also been used to infer method purity [33, 32]: if a method accepts only (transitively) read-only inputs (including
the receiver), it has no externally-visible side effects.3
			interestingly, neither of htese reference is Joe-E


	By effect systems, we mean the sort of type system extension that reasons about bounds on program
behavior as part of the type judgment, in the sense of the work on FX that originally coined the term
“effect system” [40, 24]. This stands in contrast to denotational approaches which attempt to assign
meaning to effects, often by way of monads or some extension thereof, following Moggi [46]. Filinksi [20]
offers an excellent discussion of the distinction. 


https://research.google/pubs/permission-and-authority-revisited-towards-a-formalization/
Permission and Authority Revisited: towards a formalization

	Drossopoulou

	Miller’s notions of permissions and authority are foundational to the analysis of object-capability programming. Informal definitions of these concepts were given in Miller’s thesis. In this paper we propose definitions for permissions and authority, based on a small object-oriented calculus. We quantify their bounds (current, eventual, behavioral, topological), and delineate the relationships between these definitions.



Deny Capabilities for Safe, Fast Actors

	Abstract
	Combining the actor-model with shared memory for performance is efficient but can introduce data-races. Existing
	approaches to static data-race freedom are based on uniqueness and immutability, but lack flexibility and high performance implementations. Our approach, based on deny properties, allows reading, writing and traversing unique references, introduces a new form of write uniqueness, and guarantees atomic behaviours.


https://blog.jtfmumm.com//2016/03/06/safely-sharing-data-pony-reference-capabilities/
	Safely Sharing Data: Reference Capabilities in Pony
		Write Law: Write only when you know no one else can read or write.

		Read Law: Read only when you know no one else can write.

	An `iso` is mutable, but it’s safe to send to other actors as long as we give up our reference to it in the process.
	`val` is a reference to an immutable data structure that can be shared among actors

	`tag` can be shared by many actors but which denies both read and write permissions.
		used to identify actors
	sendables: These are references we know we can pass safely to one or more actors: iso, val, and tag

	iso –> val –> tag

	with a single actor execution is synchronous:
	
	`ref`: A reference to mutable data that makes no guarantees about how many local aliases to that data exist

	`trn` reference is writeable, but allows no other writeable aliases. Unlike an iso, however, it allows other readable aliases. 

	`box` means that you can read it locally but not write to it.

	 iso can be substituted for any of the reference capabilities.

	How do we give up an alias in Pony? We simply use consume



https://www.ponylang.io/media/papers/a_prinicipled_design_of_capabilities_in_pony.pdf

	A Principled Design of Capabilities in Pony


	A formal model of a programming language gives confidence that the language fulfils
	any guarantees it claims about safety or liveness, also helping to uncover bugs or inconsistencies within the language design or implementation. We focus on the programming
	language Pony: a relatively new, actor-model, concurrent programming language with
	an existing partial model showing that Pony’s type system guarantees freedom from
	data-races but lacking a number of important features found in the language itself.
	In this thesis we describe PonyG , a formal model for a significantly larger subset of
	the Pony language. We begin by revisiting the existing formal model, simplifying and
	enhancing the model considerably in several ways with a number of novel components.
	Firstly, we introduce the explicit extracting viewpoint adaptation operator, which
	allows us to distinguish between field read and write operations and allows us to type
	such expressions in a less restrictive way than that enforced by the old model. Secondly,
	we introduce the distinction between temporaries at the focus of the execution, which
	we refer to as active temporaries, and other passive temporaries such as those being
	passed as arguments to a method call. By combining these two new distinctions we
	are able to considerably simplify the definition of well-formed runtime configurations
	and more easily reason about the heap at arbitrary points during execution, allowing
	us to prove that our well-formedness definitions are preserved through execution of the
	program.
	After simplifying the model, we move on to include a number of extensions found in
	the Pony language, namely inheritance, unions, tuples and intersection types. We also
	note and provide potential solutions for a number of bugs in the existing implementation
	for the language exposed during development	


https://infoscience.epfl.ch/entities/publication/cd99b591-7ee9-4396-8d28-4a12768320f2
A Study of Capability-Based Effect Systems
	A Study of Capability-Based Effect Systems
	Fengyun Liu


	The problem of effect polymorphism is a major obstacle to wide adoption of effect systems in the programming community. The absence of effect systems reduces compiler optimization opportunities and disables effect constraints on APIs in parallel and distributed computations. This study shows that capability-based effect systems, equipped with stoic functions and free functions, can easily solve the problem of effect polymorphism without incurring notational burden on programmers. With this advantage, capability-based effect systems stand a better chance to be adopted by the programming community. The central idea of capability-based effect system is that a capability is required in order to produce side effects. If capabilities are passed as function parameters, by tracking capabilities in the type system we can track effects in the program. To ensure that capabilities are passed through function parameters, instead of being captured from the environment, we need to impose a variable-capturing discipline, stipulating that capa- bility variables cannot be captured. Functions observing the discipline are called stoic functions, while functions not observing the discipline are called free functions.




https://se.cs.uni-tuebingen.de/publications/boruch2023capturing.pdf
Capturing Types

		Type systems usually characterize the shape of values but not their free variables. However, many desirable
	safety properties could be guaranteed if one knew the free variables captured by values. We describe CC<:□ ,
	a calculus where such captured variables are succinctly represented in types, and show it can be used to safely
	implement effects and effect polymorphism via scoped capabilities. We discuss how the decision to track
	captured variables guides key aspects of the calculus, and show that CC<:□ admits simple and intuitive types
	for common data structures and their typical usage patterns. We demonstrate how these ideas can be used to
	guide the implementation of capture checking in a practical programming language.


	map is classified as pure since it does not produce any effects in its own code, but when analyzing an
application of mapto some argument, the capabilities required by the argument are also capabilities
required by the whole expression. In that sense, we get effect polymorphism for free.
		

https://docs.scala-lang.org/scala3/reference/experimental/cc.html
	If a type T does not have a capture set, it is called pure, and is a subtype of any capturing type that adds a capture set to T.

	The usual function type A => B now stands for a function that can capture arbitrary capabilities. We call such functions impure. By contrast, the new single arrow function type A -> B stands for a function that cannot capture any capabilities, or otherwise said, is pure.

	To summarize, there are two "sweet spots" of data structure design: strict lists in side-effecting or resource-aware code and lazy lists in purely functional code. Both are already correctly capture-typed without requiring any explicit annotations. Capture annotations only come into play where the semantics gets more complicated because we deal with delayed effects such as in impure lazy lists or side-effecting iterators over strict lists. This property is probably one of the greatest plus points of our approach to capture checking compared to previous techniques which tend to be more noisy.

	A => B is an alias type that expands to (A -> B)^

Melicher et al. A Capability-Based Module System for Authority Control. ECOOP 2017. https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2017.20

	The principle of least authority states that each component of the system should be given authority to access only the information and resources that it needs for its operation. This principle is fundamental to the secure design of software systems, as it helps to limit an application's attack surface and to isolate vulnerabilities and faults. Unfortunately, current programming languages do not provide adequate help in controlling the authority of application modules, an issue that is particularly acute in the case of untrusted third-party extensions.

	In this paper, we present a language design that facilitates controlling the authority granted to each application module.  The key technical novelty of our approach is that modules are first-class, statically typed capabilities. First-class modules are essentially objects, and so we formalize our module system by translation into an object calculus and prove that the core calculus is type-safe and authority-safe. Unlike prior formalizations, our work defines authority non-transitively, allowing engineers to reason about software designs that use wrappers to provide an attenuated version of a more powerful capability.

	Our approach allows developers to determine a module's authority by examining the capabilities passed as module arguments when the module is created, or delegated to the module later during execution. The type system facilitates this by identifying which objects provide capabilities to sensitive resources, and by enabling security architects to examine the capabilities passed into and out of a module based only on the module's interface, without needing to examine the module's implementation code. An implementation of the module system and illustrative examples in the Wyvern programming language suggest that our approach can be a practical way to control module authority.	
	---

	So modules are first class objects.

	An extension imports a logger. We don't want logger provide filesystem except to the extension.

	If the logger doesn't return a file object..., extentsion does not import IO directly.

	In a hermetic programming language, this is moot:
		- extension cannot import a capability directly
		- the logger class is statically typed as proposed in this paper 

	" This is in contrast to conventional languages and module systems, in which global variables, unrestricted reflection, arbitrary downcasts, and other “back doors” make capability-based reasoning infeasible."

		
	The third contribution of our work is the formalization of authority control in the designed module system, in which we introduce a novel, non-transitive definition of authority
	that explicitly accounts for attenuated authority (e.g., as in the logger example above).
	We also introduce a definition of authority safety and formally prove the designed system
	authority-safe. Our result contrasts prior, trans	


	For a module to be pure, all of these conditions must be satisfied. The third condition has a caveat: The prohibition is on whether a module and its functions capture state, not whether they affect it. Functions defined in a pure module may have side effects on state, but only if the state in question is passed in as an argument or created within the function itself.


https://abgru.me/publication/capturing-types/capturing-types.pdf
Keeping Track of Capabilities
MARTIN ODERSKY, EPFL


Type systems usually characterize the shape of values but not their free variables. However, many desirable
safety properties could be guaranteed if one knew the free variables captured by values. We describe CC<:□ ,
a calculus where such captured variables are succinctly represented in types, and show it can be used to
safely implement effects and effect polymorphism via scoped capabilities. We discuss how the decision to track
captured variables guides key aspects of the calculus, and show that CC<:□ admits simple and intuitive types
for common data structures and their typical usage patterns. We demonstrate how these ideas can be used to
guide the implementation of capture checking in a practical programming language

---






----

notes on email:

	take the approach of allowing static immutable data in the ambient environment (since that's often useful), which is more permissive than what the essay describes.


outline of response

	would build on the paper...optional title "stoic programming: live vs inert"

	one observation the Stoic paper missed is that non-stoic function are themselves capabilities. So the definition 

			"stoic functions cannot capture capabilities nor non-stoic functions from the environment"

	we would simplify this 

		"stoic functions cannot capture capabilities from the environment"

	In fact a stoic functions are extensionally equal to inert functions.

	Then our essay would focus on "inertness" as a term worth introduicing, and distinguishing from:
		- mutability/immutability
		- pure/impure		
		- a pure value is not necessarily inert
		- a stoic function is inert

the inertness/purity orthogonality is worth mentioning....

"live value" better definition of capability

inert ambient scope / stoic main 
	turns even a FP language into a capability language



idea of inert ambient scope is not new
	- equating this with a stoic main
	- captured ambient variables don't need to be represented in types
		- unless closures

	"non-stoic function = free function"


close the loop:
	a stoic function is an inert function
	one that captures no live value


Our definition is "stoic functions cannot capture *live values* from the environment"

	a non-stoic isa capability == live value


regarding gordon:
	makes capability vs. effect systems as alternatives. Both at once.



if scala 3 were an inert programming language, then the universal capability `cap` would not exist, nor would Capability Classes.


"and capture types are strictly more flexible than the syntactic discipline behind stoicity/hermeticit"
	what is he saying here? More flexible?

	*


Scala 3's capture checking documentation uses the terms *pure*/*impure* for functions that can/cannot capture capabilities. However these definitions are equivalent to the definitions of *stoic*/*nonstoic*.



More generally, Scala 3 defines pure/impure as types with empty/nonempty capture sets.


Or rather, the more flexible term is *inert*/*live*, which can apply to both function and not-function types. What is defined in Scala 3's capture checking document as a *pure* type is *inert*, and 



Scala 3's capture checking documentation uses the terms *pure*/*impure* to describe values that cannot/can capture capabilities. These definitions are identical to the definitions of *stoic*/*nonstoic* when describing functions. When describing values in general to my definitions of *inert*/*live*.

I would argue that the terms stoic/non-stoic should be resuscitated and used instead of pure/impure to designate functions with empty/non-empty capture sets, because pure/impure have traditional meanings that are orthogonal to inert/live as I argue in my essay.

To port the example from my essay to scala 3 with Capture Checking:

	var getTime: ( getClock: () => Clock^ ) -> Int

Using the Scala 3 terminology, the function getTime is pure, and getClock is impure. But in traditional



I would argue that the terms inert/live should be used instead of pure/impure in Scala 3 to designate values with empty/non-empty capture sets. pure/impure have traditional meanings that are orthogonal to inert/live as I argue in my essay.

	var getTime: ( getClock: () => Clock^ ) -> Int

In the terminology of the Scala 3 docs c is impure and getTime is pure.

However in the traditionally PL design, c would be considered *pure* in that it is referentailly transparent. getTime is impure because evaluating it has an effect. The terminology is backwards.

The fact that getTime, as a value, has an empty capture set does not make it *pure* in the traditional sense. Rather, it makes it stoic, or more generally inert.




def incCounter(c: => Counter^) => Int^{c}





Confusing themmm...mistakes

	pass a pure but live function to foo, which is hermetic 

Of course the function passed to map or filter could also be pure










In a stoic variant of Scala 3, Capability Classes would not exist because ambient capabilities could not be captured. Only bound variables (parameters or local variables) could be captured. The type of a live value would be a capturing type (the values it captured that made it live). 

A live 



----

my essay assumes only a single type of reference, Pony-style  


---



The definition "stoic functions cannot capture capabilities nor non-stoic functions from the environment" can be
	"stoic functions cannot capture live values from the environment"



The term live/inert to distinguish between values that are or are not capabilities.



"stoic functions cannot capture capabilities nor non-stoic functions from the environment"


"To ensure that capabilities are passed through function parameters, instead of being captured from the environment, we need to impose a variable-capturing discipline, stipulating that capa- bility variables cannot be captured"
	this is not true











 Further eliminating authority from the ambient environment is not new—the literature on capability security and authority control seem to be converging on a consensus that ambient authority should be....




was to foreground existing work in capsec and authority control, in which there has been convergence around the idea of eliminating authority from the ambient environment. But


As an example of 



 (e.g. module systems as proposed in X become moot).




reposition contribution as a semantic/expository synthesis

inert is broader than immutable


the generalization of stoicicity -- interness





I now see that the function-level property overlaps with stoicity and Scala capture checking. 



----


In a language with an inert ambient scope and hermetic top-level definitions, capture checking is no longer needed to police ambient authority in ordinary functions. It remains useful only where the language deliberately permits live closures, scoped capabilities, or capability-bearing data structures.


Thank you for the reviews. The pointer to Stoic is very helpful and clearly changes how the essay should be framed. I now see that the function-level property overlaps with stoicity and Scala capture checking. The revised version would foreground that prior art and reposition the contribution as a semantic/expository synthesis: distinguishing inertness/liveness from purity, applying that distinction to pure functional languages, and arguing for a whole-language design point in which hermetic main plus inert ambient scope makes capability flow a default language property rather than a capture-tracking feature. If there is any possibility of submitting a substantially revised version, I would be glad to do so; otherwise I’ll revise for a future venue.

This essay isolates inertness/liveness as a semantic property of values orthogonal to both purity and immutability, and uses it to explain why pure functional programs can still contain authority-bearing values.

Stoic functions are inert function values. A stoic programming language is one whose ambient scope is inert and whose live values enter through explicit interfaces, typically through main. This matters even in pure functional languages, because pure values can still be live.



What are my poitns
  built on stoicity
  stoic main eliminates ambient authority
  	non-stoic are only local closures

	live/inert vs pure/impure and mutable/immutable


	build on stoic functions


	 capa- bility variables cannot be captured



live value mockability test

"take the approach of allowing static immutable data in the ambient environment (since that's often useful), which is more permissive than what the essay describes."

okay he basically says my only point is:
	The idea of restricting functions from accessing anything not derived from their explicit parameters
	lots of papers talk about reference immutability

	- A number of languages under the old banner of "reference immutability" or the newer banner of "reference capabilities" use transitively read-only references to prohibit what the essay calls grafting.

	- More generally, what the essay calls hermetic programming falls into a long line of work on syntactic (often type based) approaches to enforcing authority safety.
		...but the *right* approach is an inert ambient environment, and that is equivalent to a stoic program entrypoint.
		don't want to reinvent authority safety/capability based security. 

		But enforcing authority safely has essentially converted on inert ambient environment -- the capability based module system proposed by Melicher is moot in a stoic language like JoeE or systems like WASI -- which is equivalent to stoic program entry point. 


my response: "reference immutabiliy" or "reference capabilities"
	my contribution is the generalization of stoicity: *inertness* and its distinction from purity.


although ... is a unique and old idea, it has never been named until the stoic technical report. 

my essay helps tie stoicity to familiar concepts of DI and ref transparency but most importantly defines its orthogoncal relationship to purity....cloases the loop...language level


The strict discipline of an inert programming language does not necessarily mean all functions are inert: a function may still allow live closures that capture bound identifiers (parameters and local variables). Capture checking as proposed for Scala 3 would still be relevant and useful for...I would mention briefly in the section on closures 


---





being grafted (written into other live values)


capability classes would need to be passed as parameters to main




...I would mention briefly in the section on closures 


I would distinguish stoicity is a semantic property of functions from stoic programming as a language property or discipline that eliminates ambient authority with a stoic main function. However even in a stoic programming language non-stoic functions may still exist as closures capturing bound identifiers (parameters and locals). Capture checking would be relevant and valuable in preventing (escaping...)

The reviewer comments that reference immutability prohibits what I call grafting. Grafting is writing a live value into another live value. Reference immutability makes a value/reference inert and prohibits writing any value. Capture checking could prevent this...so could POny's....








----






E's DeepFrozen is the same idea. Or 




. I do think it would be valueable


Drossopolou et al. Permission and Authority Revisited: 

....indeed Scala 3 becomes effectively a pure object capability language once you make its main function inert:
	



Inversion of control, inert ambient environment, "no hidden inputs", "IO at the edges" are all existing disciplines that converge on a stoic main function




The novel contribution would be the orthogonality of stoicity and purity, and introducing the terms inert/live. Like *stoic*, these terms name existing concepts



applying that distinction to pure functional languages, and finally showing that an inert main function forces an inert ambient environment and makes authority control a default language property rather than an additional feature. 








I agree that parameterizing all access to state is an old and simple idea, but this idea had not been named and formally defined as a semantic property prior to X's paper, and I believe the concept and its relationship to purity deserves more attention. For me, and I imagine for a lot readers, understanding the relationship of stoicity to purity makes a lot of things click. Inversion of control, inert ambient environment, "no hidden inputs", "IO at the edges" are all existing disciplines that converge on a stoic main function

But if there is a valuable idea that didn't come through, it's the idea of inertness and its distinction from purity.



----

the whatisname essay with the module system is actually what we call inert modules


----






---

Melicher et al.’s state that all functions defined in pure modules must be stoic but not necessarily pure, without using the term stoic: Functions defined in a pure module may have side eﬀects on state, but only if the state in question is passed in as an argument or created within the function itself." In my paper I show that an inert ambient environment implies inert packages. In my revised paper I would show that inert packages are equivalent to Wyvern's pure modules.





 of access to state via the mockability test. This defini

This definition applies across paradigms, not only to functions, but also to object references, closures, handles, channels, built-ins, and pure effect values that may nevertheless convey authority.


"Authority is stronger and more behavioral. The paper starts from Miller’s phrase “the Ability to Cause Effects,” then defines current authority CA(P,σ,o) as the set of objects o, whose fields may be changed by a method call originating from object o, using receivers and arguments accessible from o, in the current program state.""



Finally, I take the reviewer’s point that capture types are more flexible than the syntactic discipline of a stoic language. The revised essay would not present stoic programming as a replacement for capture checking. Rather, it would present stoic main plus inert ambient scope as a simpler whole-program baseline: ambient capability entry is ruled out by construction, while capture checking remains useful for live closures, capability-bearing data structures, and retained or escaping authority.

-----


Purity and stoicity/inertness are often confounded. For example, in the Scala 3 documentation on capturing types the terms "stoic"/"nonstoic" have been dropped in favor of the terms "pure"/"impure". But these definitions conflict with traditional definitions of these terms in the FP literature. To see this, consider the example from my paper, but written in Scala 3 with capture types:

	var getTime: ( getClock: () => Clock^ ) -> Int = ...

The function `getTime` has an empty capture set (as indicated by the -> in the type signature), while `getClock` captures a reference to the clock (as indicated by the =>). Thus `getTime` is "pure" and `getClock` is "impure" by the Scala 3 definitions. However by traditional FP definitions it is just the opposite: `getClock` is pure because it is referentially transparent, whereas `getTime` is impure because it interacts with the clock. I would suggest the scala 3 folk consider resuscitating the terms "stoic"/"non-stoic" for function types with empty/non-empty capture sets.

 Scala 3 extends the definition of "pure" to include any type with an empty capture set—not just functions. But defining "pure" values in this way is especially problematic in the context of a FP programming language, where all values are typically considered "pure". 

What is needed is a term for a value with an empty capture set. Or in other words, a value that conveys no authority, or a non-capability: whether that value is a function or reference to an object or a capability. An "immutable reference" is a pretty good term for a non-capability in an OO language, but it is an object-graph property not a authority/access property and "immutability" is irrelevant in a pure FP language. 

So the inert/live distinction fills this terminology gap in a way that works across paradigms, applying to object references, functions, FP effect values, handles, channels, built-ins, etc., or any identifier or value that may convey authority.

But as discussed in the essay, the strict discipline of a stoic *programming language* does not necessarily mean all functions are inert: live closures may still capture live bound identifiers (parameters and local variables). If Scala 3 were hermetic, capture checking would still be relevant and useful to prevent capabilities from escaping.

----



Subject: Hermetic Programming / Stoicity revision

Dear Felienne and Onward! Essays reviewers,

Please convey my thinks to the reviewers for their thoughtful reviews. The pointer to the Stoic paper is very helpful and clearly changes how the essay should be framed. A revised version would foreground that prior art and build on the concept of *stoicity* rather than introducing a new term. What would remain is an expository synthesis exploring the consequences of lifting stoicity to a language-level property by making the program's main entry point stoic.

 , eliminating ambient authority and forcing an inert ambient environment and inert modules (which appear to be semantically equivalent to Melicher et al.’s pure modules).

But the most important novel insight remaining in my revised paper would be the orthogonality of purity and inertness (a generalization of stoicity), and applying that distinction to pure functional languages. 

Purity is an effect/interaction property; inertness is an authority/access property. Gordon’s use/mention distinction seems relevant here: a function or value may mention authority without using it. The distinction is especially in FP languages, because a function can be pure in the ordinary functional-programming sense while still being live—that is, while still carrying authority to the real console, clock, filesystem, or other state. This is why approaches to authority safety are still relevant even in FP languages.

Scala's capture checking documents also illustrates why the pure/inert distinction is useful. The current docs use the term “pure” intead of "stoic" for types with empty capture sets is internally coherent but overloads a term that traditionally has a different meaning in functional programming. A pure function in the traditional sense can capture capabilities without using them, making it "impure" in the Scala 3 sense. And an impure function can have effects via its parameters without capturing any capabilities, making it "pure" in the Scala 3 sense. 

The terms *inert* and *live* are meant to fill this terminology gap at the value level. Types with empty capture sets would be *inert*. I use inert/live rather than immutable/mutable because mutability is an object-graph property not a authority/access property and in an FP language all values are immutable.

While a term like "immutable reference" serves as a useful proxy for a non-capability in object-oriented systems, it describes an object-graph property that loses its meaning in pure functional environments.


My definition of access to state substantially overlaps Drossopoulou et al.’s definition of authority but is broader because it includes reading mutable state (e.g. reading the clock). My revised essay would attempt to reconcile these definitions.

I take the reviewer’s point that capture types are more flexible than the syntactic discipline of a stoic language. The revised essay would not present stoic programming as a replacement for capture checking. Rather, it would present stoic main plus inert ambient scope as a simpler whole-program baseline: capture checking remains useful for live closures, capability-bearing data structures, and retained or escaping authority.

If there is any possibility of considering a substantially revised version along these lines, I would be grateful for the opportunity.

Regardless of the committee's decision, I would appreciate it if you could forward this response to the reviewers with my sincere thanks. Reviewer 100A’s feedback, in particular, was a massive gift that has already improved the trajectory of this work.

Finally, if Reviewer 100A would have any interest in a brief chat to help me further refine these ideas for a future venue, I would deeply welcome the connection. Please feel free to share my email address with them, entirely at their discretion.

Thank you again for your time, guidance, and the rigor of this process.

Best regards,

Jonathan Warden


----



Subject: Hermetic Programming / Stoicity revision

Dear Felienne and Onward! Essays reviewers,

Please convey my thinks to the reviewers for their thoughtful reviews. The pointer to the Stoic paper is very helpful and clearly changes how the essay should be framed. A revised version would foreground that prior art and build on the concept of *stoicity* rather than introducing a new term. What would remain is an expository synthesis exploring the consequences of lifting stoicity to a language-level property by making the program's main entry point stoic.

The novel insight would be the orthogonality of inertness and purity (access and interaction). Gordon’s use/mention distinction seems relevant here: Purity is an use/interaction property; inertness is an mention/access property. The distinction is especially important in FP languages, because a value can be pure in the ordinary functional-programming sense while still being live—that is, while still carrying authority to the real console, clock, filesystem, or other state. This is why capabilities and authority safety are still relevant in languages like Haskell.

Scala's capture checking documents also illustrates why the pure/inert distinction is useful. The latest Scala docs use the term “pure” instead of "stoic" for types with empty capture sets. This is internally coherent but overloads a term that traditionally has a different meaning in functional programming. A pure function in the traditional sense can capture capabilities without using them and thus be "impure" in the Scala 3 sense. And an impure function can have effects via its parameters without capturing any capabilities and thus be "pure" in the Scala 3 sense. 

Extending the definition of "pure" to mean "empty capture set" becomes especially problematic when brought back to a pure FP context, where all values are already considered pure.

Melicher et al.’s pure modules overload the term pure in the same way. Pure modules appear to be semantically equivalent to my paper's inert package (I take the reviewers point about reinventing things that are well known). In particular pure modules may export functions that are impure as long as they are stoic: "Functions defined in a pure module may have side eﬀects on state, but only if the state in question is passed in as an argument or created within the function itself". My revised paper would summarize the concept from that paper, and stress that making the main function stoic forces all modules to be pure modules.

While the concept of functions, values, modules, etc. that convey no authority has been described many times in the literature, the field currently lacks is a precise, cross-paradigm name for it. While a term like "immutable reference" serves as a useful proxy for a non-capability in object-oriented systems, it describes an object-graph property that loses its meaning in pure functional environments. 

The inert vs. live distinction fills this terminology gap across paradigms. It applies equally to object references, FP effect values, modules, handles, channels, and built-ins, and the identifiers that refer to them, giving the language design community a precise vocabulary for non-authority-bearingness that is not easily conflated with purity.

I take the reviewer’s point that capture types are more flexible than the syntactic discipline of a stoic language. The revised essay would not present stoic programming as a replacement for capture checking. Even in a stoic programming language, capture checking remains useful for live closures.

in summary, my essay is not really about *stoicity*, it is about *inertness*—stoicity is just inertness applied to functions (stoic and inert functions are extensionally the same). When the concept is descriged in the literature, it is generally described as *purity*. The revised paper would use inert instead of stoic in the title, and would be more thorough in surveying the literature to discover where this concept has appeared, and show how they all connect. 

If there is any possibility of considering a substantially revised version along these lines, I would be grateful for the opportunity.

Regardless of the committee's decision, I would appreciate it if you could forward this response to the reviewers with my sincere thanks. Reviewer 100A’s feedback, in particular, was a massive gift that has already improved the trajectory of this work.

Finally, if Reviewer 100A would have any interest in a brief chat to help me further refine these ideas for a future venue, I would deeply welcome the connection. Please feel free to share my email address with them, entirely at their discretion.

Thank you again for your time, guidance, and the rigor of this process.

Best regards,

Jonathan Warden





----

...formal definition of authority...and this can be the basis for such a definition. Also their definition of authority only includes authority to *write*, but the ability to *read state that can change* (e.g. read the clock or file on disk) also is treated as authority in the capability security literature (and I don't understand why Miller has )


My definition of access to state substantially overlaps Drossopoulou et al.’s definition of authority but is broader because it includes reading mutable state (e.g. reading the clock). My revised essay would attempt to reconcile these definitions.


---



