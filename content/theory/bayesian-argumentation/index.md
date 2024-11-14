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

What is a *good* argument?

From a logical point of view, a good argument is logically sound. But in the real-world people rarely argue with pure logic. 

From a rhetorical point of view, a good argument is one that is persuasive. But how can this be measured?

In this series of essays, I consider this question from a Bayesian point of view. Under this point of view, arguments are simply information that may cause a Bayesian rational agent to modify their beliefs. 

The beliefs of a Bayesian agent, which I will refer to as a **subject**, can be modeled as a probability distribution. If we know this distribution, there are various ways we can measure how the information in an argument affects or may affect the subject's other beliefs. For example, if an argument changes the subject's mind, it is **persuasive**, and the degree to which it changes the subjects mind is a measure of persuasiveness. Yet persuasiveness is not the only measure of a good argument, because an argument won't change the subject's mind if the subject has already heard it; only **new** information can change a Bayesian agent's mind. Other measures of argument strength relate to how much the subject **would** change their mind if they accepted/rejected the premise. These include **relevance**, **sufficiency**, and **necessity**.

These are all **objective** measures. And yet they measure what many people would consider **subjective** aspects of an argument's quality or strength.

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


## The Value of Bayesian Argumentation

As everyone knows, a Bayesian rational agent updates their beliefs when, and only when, they acquire new information.

If persuasiveness is the only criteria for a "strong" argument, then an argument based on information the agent already knows can never be considered strong. That means, for example, that if yesterday I learned that the candidate was deceased, and you tell me today we should reject the candidate because he doesn't have a pulse, that would not be a strong argument. But clearly this is a pretty strong argument. So what's wrong?

It is a strong argument because, if I *didn't* know he was deceased, I might have a different opinion.

A Bayesian model tells us not just what the subject believes, but what they **would** believe if they had different information. This simple insight cracks open a number of ways of measuring argument strength other than just persuasiveness. In the essays on [Relevance](/relevance) and [Necessity and Sufficiency](/necessity-and-sufficiency) I will define these measures and see how they all relate mathematically to the informativeness of the argument.

The Bayesian model of argumentation also allows us to take into account the reliability of the arguer themselves. A Bayesian rational agent will only update their beliefs if they believe the information given them. So for an argument to be informative it must not just be new information, it must also be *believable*. This perspective shows us when what looks like an *ad hominim* fallacy may somteimes be perfectly rational[^3]. These ideas are discussed in the essays on [informativeness and persuasiveness](/informativeness-and-persuasiveness).

Like all models, the Bayesian model of subjective belief is an incomplete description of the human mind. But it is clearly defined. Building clear terminology on top of a clear model helps clarify our thinking, facilitate discussion, and sharpen our intuition about what argument actually is. 

There is a lot of recent academic work on Bayesian argumentation[^1][^2]. These essay are intended not as an overview of current theory, but as a useful set of definitions and formulas for practitioners: specifically software engineers building practical applications of argumentation for AI, argument mapping systems, or in our particular case, design of [social protocols](https://social-protocols.org). Our goal is to provide a useful and clear vocabulary, with common-sense but precise definitions for common concepts related to argument strength.



## This Series

In the [next essay](/relevance) in this series, I will formally define a measure of **relevance** from a Bayesian perspective and discuss some of its mathematical properties. In the remaining articles in this series I will define measures of **necessity**, **sufficiency**, **informativeness**, and **persuasiveness**, all of which relate back to this central concept of relevance. Finally, I will relate some of the concepts to the idea of **warrant** from the field of argumentation theory.

- [Relevance](/relevance)
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






