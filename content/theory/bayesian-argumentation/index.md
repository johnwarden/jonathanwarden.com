---
layout: single
title:  "A Bayesian Account of Argumentation"
slug: bayesian-argumentation
toc: true
tags: ['Argumentation Theory']
series: ['Bayesian Argumentation']
weight: 71
summary: 'What is a *good* argument?

From a logical point of view, a good argument is logically sound. But in the real-world people rarely argue with pure logic. 

From a rhetorical point of view, a good argument is one that is persuasive. But how can this be measured?

In this series of essays, I consider this question from a Bayesian point of view. 
'
canonical_url: https://jonathanwarden.com/bayesian-argumentation/

---

## Quantifying Argument

What is a *good* argument?

From a logical point of view, a good argument is logically sound. But in the real-world people rarely argue with pure logic. 

From a rhetorical point of view, a good argument is one that is persuasive. But how can this be measured?

In this series of essays, I consider this question from a Bayesian point of view. 


## Introductory Example 1

Consider the argument *this is a good candidate for the job* because *he has a pulse*. 

To anyone with common sense, this argument is probably:

- not very **persuasive**.
- not **informative**, because *he has a pulse* is probably not new information to the subject.
- clearly **relevant**, because:
    - If the subject learned that the subject did *not* have a pulse, this would be **sufficient** to reject him as a candidate.
    - Alternatively, the belief that he probably has a pulse is **necessary** for him to be a good candidate.

The Bayesian model of argumentation allows us to define precise measurements of all the above concepts.

## Bayesian Agents

Under the Bayesian point of view, arguments are simply information that may cause a Bayesian rational agent to modify their beliefs. 

The beliefs of a Bayesian agent, which I will refer to as a **subject**, can be modeled as a probability distribution. If we know this distribution, we can measure how the information in an argument affects or may affect the subject's other beliefs. 

## Objective Measures of Argument Strength

Clearly, if an argument changes the subject's beliefs about something, it is **persuasive**. However, this can't be the only measure of a good argument, because once the subject already knows something, the argument is no longer persuasive! If you argue *you should wear a seat belt* because *seat belts save lives*, that is not going to change my beliefs because I already know seat belts save lives! Only *new* information can change a Bayesian agent's beliefs. 

But clearly, in another sense, saving lives is a good argument for wearing seatbelts, even if hearing it doesn't change my mind. Fortunately the Bayesian model tells us not just what the subject believes, but what they **would** believe if they had different information. This simple insight cracks open other ways of measuring argument strength. In the essays on [Relevance and Acceptance](/relevance-and-acceptance) and [Necessity and Sufficiency](/necessity-and-sufficiency) I will define these measures and see how they all relate mathematically.

## Advantages of the Bayesian Perspective

These are all **objective** measures. And yet they measure what many people would consider **subjective** aspects of an argument's quality or strength.

Like all models, the Bayesian model of subjective belief is an incomplete description of the human mind. But it is clearly defined. Building clear terminology on top of a clear model helps clarify our thinking, facilitate discussion, and sharpen our intuition about what argument actually is. My hope is that this can provide insights about argumentation for people working in artificial intelligence, law, tools for argument mapping and deliberation, or in our case, design of [social protocols](https://social-protocols.org).

There is a lot of recent academic work on Bayesian argumentation[^1][^2]. These essay are intended not as an overview of current theory, but as an opinionated set of definitions and measures of argument strength that may be useful for practitioners.


## Basic Argumentation Theory

To define these measure of argument strength, I will start by briefly defining some basic terminology and concepts from argumentation theory.

Argumentation theory views argument as a kind of flexible, informal logic. People don't argue with logical syllogisms -- instead they make simple statements, or **claims**, meant to support or oppose other claims. 

### Claims

A **claim** is a declarative statement that people can accept or reject (agree with or disagree with). 

This definition is broad enough to include not only **descriptive** (or **empirical**) claims about the world, such as *the universe is expanding*, but also **normative** claims about the way things **should be**, such as *we should go to the beach*. 

### Arguments

In argument involves two claims: 

- A **conclusion**: the claim in dispute.
- A **premise**: the reason given to accept or reject the conclusion.

For example, I might assert the premise *people are wearing jackets* to support the conclusion *it's cold outside*. 

There is also a third, unstated claim called the **warrant**, which I will discuss this in the last article of this series.

### Premise and Conclusion

An argument may be worded in such a way that the premise is unclear (sarcasm, etc.), but there is general agreement among argumentation theorists that there is always a premise hiding in an argument. 

Since any logical combination of premises can be treated as a single premise, I will speak of an argument as **always having one (possibly compound) premise**. For example, the claim *BMWs are unreliable and overpriced* when used in an argument is treated as a single (compound) premise, even though it can be broken down into to distinct premises (e.g. *BMWs are unreliable* and *BMWs are overpriced*). 

It is easy to confuse the term *claim* with the term *premise* or *conclusion* -- especially since different authors use these terms differently. In this model, a claim is any declarative statement that can be agreed with or disagreed with. A claim can also take the **role** of premise or conclusion in some argument. 

### Reconciling Argumentation Theory with Bayesianism

In Bayesian terms, a rational agent is said to acquire **evidence**, which causes them to update their belief in the probability of some **hypothesis** (see this [Bayesian Inference Primer](/bayesian-inference-primer)). There is clearly an analogy here: **evidence is to premise as to hypothesis is to conclusion**.

But this analogy is not perfect. Many arguments assert a claim without any supporting "evidence" in the way we usually think of the term. The claim is simply asserted. And yet if the subject believe the person making the argument, they might actually change their beliefs. How can a claim without evidence change beliefs?

So it's important to understand that a claim is not evidence: *the fact that the arguer asserted the claim is evidence*.

This perspective allows us to take into account the subject's belief in the reliability of the arguer themselves. This perspective shows us when what looks like an *ad hominim* fallacy may sometimes be perfectly rational[^3]. These ideas are discussed in the essays on [informativeness and persuasiveness](/informativeness-and-persuasiveness).


## This Series

In the [next essay](/relevance-and-acceptance) in this series, I will formally define a measure of **relevance** from a Bayesian perspective and discuss some of its mathematical properties. In the remaining articles in this series I will define measures of **necessity**, **sufficiency**, **informativeness**, and **persuasiveness**, all of which relate back to this central concept of relevance. Finally, I will relate these concepts to the idea of **warrant** from the field of argumentation theory.

- [Relevance and Acceptance](/relevance-and-acceptance)
- [Necessity and Sufficiency](/necessity-and-sufficiency)
- [Informativeness and Persuasiveness](/informativeness-and-persuasiveness)
- [Warrants and Corelevance](/warrants-and-corelevance)
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

[^1]: Hahn, U., & Oaksford, M. (2007). The rationality of informal argumentation: A Bayesian approach to reasoning fallacies. (https://psycnet.apa.org/record/2007-10421-007) Psychological Review, 114(3), 704–732. https://doi.org/10.1037/0033-295X.114.3.704
[^2]: Hahn, U., Oaksford, M., & Harris, A. J. L. (2013). Testimony and argument: A Bayesian perspective. (https://psycnet.apa.org/record/2013-00206-002). In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 15–38). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_2
[^3]: Oaksford, M., & Hahn, U. (2013). Why are we convinced by the ad hominem argument?: Bayesian source reliability and pragma-dialectical discussion rules. In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 39–58). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_3






