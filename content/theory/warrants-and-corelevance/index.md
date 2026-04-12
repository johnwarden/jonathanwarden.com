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

This is the final article in my series on Bayesian Argumentation. To understand this essay, read the [introductory article](/bayesian-argumentation) and the article on [Relevance and Acceptance](/relevance-and-acceptance).

## Relevance is Not Absolute

Relevance exists in the context of the subject's other prior beliefs. For example, if the subject believes that ($\bar{𝐶}$) *the car is out of gas*, and also ($\bar{B}$) *the battery is dead*, then both of these are good reasons to believe ($\bar{A}$) *the car won't start*. 

Yet neither is relevant on its own by our definition of relevance! A premise is only relevant if changing acceptance of the premise changes acceptance of the conclusion. But given the car is out of gas, it makes no difference whether the battery is dead or not: the car won't start anyway. In other words, ($\bar{B}$) *the battery is dead* is irrelevant to ($\bar{A}$) *the car won't start* given ($\bar{𝐶}$) *the car is out of gas*.

But if the subject believes (𝐶) *the car has gas*, then ($\bar{B}$) *the battery is dead* will probably become relevant. When accepting one premise causes another premise to become relevant, we say that the premises are **corelevant**.

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


## Warrants 

This idea of a third belief that justifies the relevance of a premise to a conclusion evokes the idea of the [**warrant**](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.) from the field of [argumentation theory](https://en.wikipedia.org/wiki/Argumentation_theory).

In every argument there is an unstated claim that *the premise supports the conclusion*. This doesn't need to be stated because it's implied by the fact that the argument was made at all. After asserting *people are wearing jackets* in support of the conclusion *it's probably cold outside*, I don't need to add, pedantically, "and you see, if people are wearing jackets it must be cold outside". This unexpressed third premise is called the warrant. 

The warrant doesn't have to be a logical formula such as "if people are wearing jackets it must be cold outside". It can be based on any kind of inferential rule (deductive, inductive, intuitive) or [argumentation scheme](https://en.wikipedia.org/wiki/Argumentation_scheme) (authority, analogy, example) -- whatever justifies the inference in the mind of the arguer. Some academics use different terms for these concepts: our terminology is influenced by the influential [Toulmin model](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.), except we prefer the traditional terms **premise** and **conclusion** over **grounds** and **claim**. More precise definitions of our terms are given in the [Deliberati Argument Model](/argument-model).

## Bayesian Warrants

In a Bayesian model, a warrant could be any belief that is *necessary to the conclusion and corelevant with the premise*: that is, anything that the subject accepts that makes the premise support the conclusion. 

<!--So (𝐶) *the car has gas* warrants that argument that ($\bar{A}$) *the car won't start* because ($\bar{B}$) *the battery is dead*,-->


<!--
For example, if our subject is more likely to believe that (𝐴) *it is going to rain today* if they believe that (𝐵) *the sky is cloudy* than if they do not, then they clearly also believe something along the lines of 𝐶 *a cloudy sky is a sign of rain*.
-->

So (𝐶) *a cloudy sky is a sign of rain* is a warrant for the argument that (𝐴) *it is going to rain today* because (𝐵) *the sky is cloudy*, because accepting 𝐶 makes 𝐵 more relevant to 𝐴.

Every necessary corelevant belief helps, to paraphrase Toulmin, to "justify the inferential leap from premise to conclusion". So if we are looking for a single warrant for each argument, we can think of it as *the conjunction of necessary corelevant priors*.

<!--

If 𝐵 is relevant to 𝐴, then intuitively there must exist *some* warrant -- some corelevant belief 𝐶 that, if the subject rejected 𝐶, 𝐵 would no longer be relevant. So for any argument, there always exists a Bayesian warrant.
-->

### Counterfactual Relevance

Unfortunately, this definition of conditional relevance above still doesn't fully capture the the common notion of "relevance", because we can almost always find some second premise that makes the premise conditionally relevant. For example, the premise (𝐻) *The car has a hood ornament* may not seem relevant to (𝐴) *the car will start*, but it is conditionally relevant given the premise (𝑀) *The car is powered by a magical hood ornament*.

Of course, 𝑀 is pretty implausible -- $P(M)$ may be infinitesimally small. But other more plausible corelevant premises may have small probabilities. For example, if the subject just filled the car with gas, they will be quite certain that (𝐺) *the car has gas* and thus $P(\bar{G})$ might be infinitesimally small. So in both cases we have corelevant premises with small prior probabilities, but a car running out of gas is something that is likely to actually happen in many similar scenarios, even if not this particular one, and so it is definitely "relevant" in one sense.

Accounting for the difference in "relevance" in these two cases takes us into the metaphysical realm of modal logic, possible worlds, counterfactuals, and other difficult epistemological questions, that we won't try to answer here.

## Summary

So we've rounded out our reconciliation of Bayesianism and argumentation theory by defining the **warrant** as those prior beliefs that make the premise **relevant**. If you've read this far, I am impressed! I hope this has been useful.

In the next article I summarize all the definitions and mathematical formulas introduced in this series.


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






