---
layout: single
title:  "Warrants and Corelevance"
slug: bayesian-warrants
toc: true
tags: ['Argumentation Theory']
series: ['Bayesian Argumentation']
weight: 74
summary: ''
canonical_url: https://jonathanwarden.com/warrants-and-corelevance/

---

Relevance exists in the context of the subject's other prior beliefs. For example, if the subject beleives tha (𝐶̅) *the car is out of gas*, and also ($\bar{B}$) *the battery is dead*, then both of these are good reasons to believe (𝐴̅) *the car won't start*. Yet neither is relevant on its own by our definition of relevance! 

Given that the car is out of gas, it makes no difference whether the battery is dead or not: the car won't start anyway. In other words, ($\bar{B}$) *the battery is dead* is irrelevant to (𝐴̅) *the car won't start* given (𝐶̅) *the car is out of gas*.

But if the subject believes (𝐶) *the car has gas*, then ($\bar{B}$) the battery is dead will probably be relevant. When accepting one premise causes another premise to become relevant, we say that the premises are **corelevant**. If a premise is corelevant with some unexpressed premise, we can say that the premise is **conditionally relevant**.

### Definition of Conditional Relevance

To define corelevance mathematically, we need to first define the **conditional relevance** of 𝐵 to 𝐴 given 𝐶, $R(A,B \vert C)$:

$$
    R(A,B|C) = P(A|B,C) - P(A|\bar{B},C)
$$

### Definition of Corelevant

Then 𝐵 and 𝐶 are corelevant to 𝐴 if:

$$
    R(A,B|C) ≠ R(A,B|\bar{C})
$$


### Quantifying Corelevance 

We can measure the magnitude of the correlevance as the difference:

$$
    CR(A;B,C) = R(A,B|C) - R(A,B|\bar{C})
$$

It's easy show that co-relevance is symmetrical ([proof](#proof1)).

$$
    CR(A;B,C) = CR(A;C,B)
$$




## Basic Argumentation Theory

This idea of unexpressed beliefs that justify an argument evokes the idea of the **warrant** from the field of [argumentation theory](https://en.wikipedia.org/wiki/Argumentation_theory). Argumentation theory views argument as a kind of flexible, informal logic. People don't argue with logical syllogisms -- instead they make simple statements, or **claims** which support other claims. For example, I might claim *people are wearing jackets* to support the claim *it's probably cold outside*. 

We use the terms **premise** and **conclusion** to differentiate between the supporting and supported claims. An **argument** is just a premise stated in support of some conclusion.

In every argument there is an unstated claim that *this premise supports this conclusion*. This doesn't need to be stated because it's implied by the fact that the argument was made. After asserting *people are wearing jackets* in support of the conclusion *it's probably cold outside*, I don't need to add, pedantically, "and you see, if people are wearing jackets it must be cold outside".

This unexpressed premise that justifies the inferential leap from premise to conclusion is called the [**warrant**](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.). 

The warrant doesn't have to be a logical formula such as "if people are wearing jackets it must be cold outside". It can be based on any kind of inferential rule (deductive, inductive, intuitive) or [argumentation scheme](https://en.wikipedia.org/wiki/Argumentation_scheme) (authority, analogy, example) -- whatever justifies the inference in the mind of the arguer. Some academics use different terms for these concepts: our terminology is influenced by the influential [Toulmin model](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.), except we prefer the traditional terms **premise** and **conclusion** over **grounds** and **claim**. More precise definitions of our terms are given in the [Deliberati Argument Model](/argument-model).

## Bayesian Warrants

In Bayesian terms, a rational agent is said to acquire **evidence**, which causes them to change their belief in the probability of some **hypothesis** (see this [Bayesian Inference Primer](/bayesian-inference-primer)).

There is clearly an analogy here: **evidence is premise as to hypothesis is to conclusion**. But argumentation theory also has warrants. What is the warrant in a Bayesian model? 

The warrant clearly has to do with the subject's **prior beliefs**, because a Bayesian agent's priors are precisely what justify, in their mind, any inferential leap from premise to conclusion.

For example, if our subject is more likely to believe that (𝐴) *it is going to rain today* if they believe that (𝐵) *the sky is cloudy* than if they do not, then there clearly exists a warrant justifying, in the subject's mind, the inferential leap from 𝐵 to 𝐴.

But **why** does this warrant exist in the subject's mind? What is the inferential rule that actually justifies the inference? Is it a deductive inference? Inductive? Gut feeling?

In the Toulmin Model, the warrant would be a rule along the lines of *a cloudy sky is a sign of rain*. But in a Bayesian model, there are no rules: there is just a probability distribution modeling the Bayesian agent's beliefs. This gives us the end result of the agent's thought process, but not how they got there. But this probability distribution still serves the role of the warrant in that it justifies the inferential leap from premise to conclusion.

If the prior beliefs of our subject are represented by the probability measure $P$, then we can say that, in the mind of the subject, **a warrant exists justifying the inference from premise 𝐵 to conclusion 𝐴 iff**:

$$
    P(A|B) ≠ P(A|\bar{B})
$$

If the warrant exists, we say 𝐵 is relevant to 𝐴.


### Counterfactual Relevance

Unfortunately, this definition of conditional relevance still doesn't capture the the common notion of "relevance" pefectly well, because we can almost always find some second premise that makes the premise conditionally relevant. For example, the premise (𝐻) *The car has a hood ornament* may not seem relevant to (𝐴) *the car will start*, but it is conditionally relevant given the premise (𝑀) *The car is powered by a magical hood ornament*.

Of course, 𝑀 is pretty implausible -- $P(M)$ may be infinitesimally small. But other more plausible corelevant premises may have small probabilities. For example, if the subject just filled the car with gas, they will be quite certain that (𝐺) *the car has gas* and thus $P(\bar{G})$ might be infinitesimally small. So in both cases we have corelevant premises with small prior probabilities, but a car running out of gas is something that is likely to actually happen in many similar scenarios, even if not this particular one. 

Accounting for the difference in relevance in these two cases takes us into the metaphysical realm of modal logic, possible worlds, counterfactuals, and other difficult epistemological questions, that we won't try to answer here.


## Summary

So in a Bayesian argument, an arguer asserts a **premise** in support/opposition to some **conclusion**, and if the premise is **relevant** -- the subject is more likely to believe the conclusion if they believe the premise -- then there must be some **warrant** justifying the inference from premise to conclusion.


<style>
.sample-distribution {
    table-layout: auto; 
    display: table;
    width: 100%;
    max-width: 250px;
    margin: 25px auto;
} 

.example
{
  margin: auto;
  background-color: lightgrey;
  border: 1px solid black;
  max-width: 600px;
  padding-top: 1em;
  padding-bottom: 0px;
  padding-left: 1em;
  padding-right: 1em;
  margin-bottom:  1em;
}

.example h3 {
    margin-top: 0px;
}


</style>


## Proofs

### Proof 1

**Symmetry of Corelevance**

$$
    CR(A;B,C) = CR(A;C,B)
$$

**Proof:**

$$
\begin{aligned}
    CR(A;B,C)   &= R(A,B \vert C) - R(A,B \vert \bar{C}) \cr
                &= ( P(A \vert B,C) - P(A \vert \bar{B},C) )  \cr
                &\space\space\space\space- ( P(A \vert B,\bar{C}) - P(A \vert \bar{B},\bar{C}) )  \cr
                &= ( P(A \vert B,C) - P(A \vert B,\bar{C}) )  \cr
                &\space\space\space\space- ( P(A \vert \bar{B},C) - P(A \vert \bar{B},\bar{C}) )  \cr
                &= ( P(A \vert C,B) - P(A \vert \bar{C},B) \cr
                &\space\space\space\space- ( P(A \vert C,\bar{B}) - P(A \vert \bar{C},\bar{B}) )  \cr
                &= R(A,C \vert B) - R(A,C \vert \bar{B}) \cr
                &= CR(A;C,B) \cr
\end{aligned}
$$




[^1]: Hahn, U., & Oaksford, M. (2007). The rationality of informal argumentation: A Bayesian approach to reasoning fallacies. (https://psycnet.apa.org/record/2007-10421-007) Psychological Review, 114(3), 704–732. https://doi.org/10.1037/0033-295X.114.3.704
[^2]: Hahn, U., Oaksford, M., & Harris, A. J. L. (2013). Testimony and argument: A Bayesian perspective. (https://psycnet.apa.org/record/2013-00206-002). In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 15–38). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_2
[^3]: Oaksford, M., & Hahn, U. (2013). Why are we convinced by the ad hominem argument?: Bayesian source reliability and pragma-dialectical discussion rules. In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 39–58). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_3






