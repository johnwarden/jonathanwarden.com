---
layout: single
title:  "A Bayesian Account of Argumentation"
slug: bayesian-argumentation
toc: true
tags: ['Argumentation Theory']
series: ['Bayesian Argumentation']
weight: 71
summary: 'In this essay, I present an account of argumentation as the exchange of information between Bayesian rational agents. The basic idea of the Bayesian view of probability is that probabilities represent subjective degrees of belief. So if we know the beliefs of some rational "subject", we can precisely define and measure various concepts relating to the quality of an argument in the mind of the subject. In other words we can objectively measure the subjective quality of an argument.'
canonical_url: https://jonathanwarden.com/bayesian-argumentation/

---

## Quantifying Argument

What makes for a *good* argument?

From a logical point of view, a good argument is logically sound. But in the real-world people rarely argue with pure logic. 

From a rhetorical point of view, a good argument is one that is convincing. But how can this be measured?

In this series of essays, I present a Bayesian model of argumentation, where arguments are treated as information that may cause a Bayesian rational agent to modify their beliefs. From a Bayesian perspective, the beliefs of any rational "subject" are modeled as a probability distribution. Given this distribution, we can define various metrics that relate the information given in an argument to the subject's other beliefs. This lets us define **objective** measures of what would normally be considered **subjective** aspects of an argument's quality or strength, such as **relevance** and **persuasiveness**.

This perspective can provide some powerful insights about argumentation for people working in artificial intelligence, law, argument mapping software, or in our case, design of [social protocols](https://social-protocols.org).


<!--

Consideration of how both logic and probability work to provide measures of argument quality suggests that ‘consistency’ and ‘relevance’ are two sides of the same coin.

https://www.cell.com/trends/cognitive-sciences/pdf/S1364-6613(20)30020-6.pdf

Related WOrk: 
https://link.springer.com/article/10.1007/s11229-005-5233-2
-->

<!--
[The basic idea of Bayesianism is that subjective beliefs can be modeled as a probability distribution, and when a rational agent acquires new information, they should update their beliefs based on the laws of probability]
-->


## Introductory Example 1

Consider the argument *this is a good candidate for the job* because *he has a pulse*. 

If our subject is a Bayesian rational agent with common sense, then probably:

- The argument is not very **persuasive**.
- Nor is it **informative**. *He has a pulse* is probably not new information to the subject. 
- Yet the argument is clearly **relevant**, because:
    - If the subject learned that the subject did **not** have a pulse, this would be **sufficient** to reject him as a candidate.
    - Alternatively, the belief that he probably has a pulse is **necessary** for the belief that he might be a good candidate.


## The Value of Bayesian Argumentation: Hypotheticals

As everyone knows, a Bayesian rational agent updates their beliefs when they acquire new information. An argument that is not informative therefore can't be persuasive. 

But even when an argument is not persuasive, a Bayesian model tells us what an agent **would** believe if they accepted or rejected the information in the argument (e.g. if they thought that the candidate *didn't* have a pulse). This simple insight cracks open a number of ways of measuring argument strength other than just persuasiveness. In the essays on [Relevance and Corelevance](/relevance-and-corelevance) and [Necessity and Sufficiency](/necessity-and-sufficiency) I will define these measures and see how they all relate mathematically to the informativeness of the argument.

The Bayesian model of argumentation also allows us to take into account the reliability of the arguer themselves. A Bayesian rational agent will only update their beliefs if they believe the information given them. So for an argument to be informative it must not just be new information, it must also be *believable*. This perspective shows us when what looks like an *ad hominim* fallacy may somteimes be perfectly rational[^3]. These ideas are discussed in the essays on [informativeness and persuasiveness](/informativeness-and-persuasiveness).

Like all models, the Bayesian model of subjective belief is an incomplete description of the human mind. But it is clearly defined. Building clear terminology on top of a clear model helps clarify our thinking, facilitate discussion, and sharpen our intuition about what argument actually is. 

There is a lot of recent academic work on Bayesian argumentation[^1][^2]. These essay are intended not as an overview of current theory, but as a useful set of definitions and formulas for practitioners: specifically software engineers building practical applications of argumentation for AI, argument mapping systems, or in our particular case, design of [social protocols](https://social-protocols.org). Our goal is to provide a useful and clear vocabulary, with common-sense but precise definitions for common concepts related to argument strength. This can hopefully help clarify discussion among collaborators and prove useful in documentation and code.


## Introductory Example 2

Now consider another example argument: *the car won't start* because *the car is out of gas*. If the subject previously believed the car had gas, then this new information might well be persuasive. But suppose the subject accepts this information, but also believes that *the car's battery is dead*? With this assumption, the car being out of gas is now in a sense irrelevant. 

Clearly the relevance of an argument depends on context: it depends on other beliefs the subject has about the state of world. 

Theoretically, if we have a model of some subject's beliefs about the world, we can identify the **corelevant** beliefs -- the beliefs cause the argument to be relevant. I will define this more precisely in the essay on [relevance and corelevance](/relevance-and-corelevance).


## Basic Argumentation Theory

This idea of unexpressed beliefs that justify an argument evokes the idea of the **warrant** from the field of [argumentation theory](https://en.wikipedia.org/wiki/Argumentation_theory). Argumentation theory views argument as a kind of flexible, informal logic. People don't argue with logical syllogisms -- instead they make simple statements, or **claims** which support other claims. For example, I might claim *people are wearing jackets* to support the claim *it's probably cold outside*. 

We use the terms **premise** and **conclusion** to differentiate between the supporting and supported claims. An **argument** is just a premise stated in support of some conclusion.

In every argument there is an unstated claim that *this premise supports this conclusion*. This doesn't need to be stated because it's implied by the fact that the argument was made. After asserting *people are wearing jackets* in support of the conclusion *it's probably cold outside*, I don't need to add, pedantically, "and you see, if people are wearing jackets it must be cold outside".

This unexpressed premise that justifies the inferential leap from premise to conclusion is called the [**warrant**](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.). 

The warrant doesn't have to be a logical formula such as "if people are wearing jackets it must be cold outside". It can be based on any kind of inferential rule (deductive, inductive, intuitive) or [argumentation scheme](https://en.wikipedia.org/wiki/Argumentation_scheme) (authority, analogy, example) -- whatever justifies the inference in the mind of the arguer. Some academics use different terms for these concepts: our terminology is influenced by the influential [Toulmin model](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.), except we prefer the traditional terms **premise** and **conclusion** over **grounds** and **claim**. More precise definitions of our terms are given in the [Deliberati Argument Model](/argument-model).

## A Bayesian Definition of Warrant

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

If the warrant exists, we say that 𝐵 is **relevant** to 𝐴. Otherwise, we say it is **irrelevant**.

## Summary

So in a Bayesian argument, an arguer asserts a **premise** in support/opposition to some **conclusion**, and if the premise is **relevant** -- the subject is more likely to believe the conclusion if they believe the premise -- then there must be some **warrant** justifying the inference from premise to conclusion.


## This Series

In the [next essay](/relevance-and-corelevance) in this series, I will formally define a measure of **relevance** from a Bayesian perspective and discuss some of its mathematical properties. In the remaining articles in this series I will define measures of **necessity**, **sufficiency**, **informativeness**, and **persuasiveness**, all of which relate back to this central concept of relevance.

- [Relevance and Corelevance](/relevance-and-corelevance)
- [Necessity and Sufficiency](/necessity-and-sufficiency)
- [Informativeness and Persuasiveness](/informativeness-and-persuasiveness)
- [Summary of Definitions](/bayesian-argumentation-definitions)



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






