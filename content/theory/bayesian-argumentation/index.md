---
layout: single
title:  "A Bayesian Account of Argumentation"
slug: bayesian-argumentation
toc: true
tags: ['Argumentation Theory']
series: ['Bayesian Argumentation']
weight: 71
summary: 'In this essay, we present an account of argumentation as the exchange of information between Bayesian rational agents. The basic idea of the Bayesian view of probability is that probabilities represent subjective degrees of belief. So if we know the beliefs of some rational "subject", we can precisely define and measure various concepts relating to the quality of an argument in the mind of the subject. In other words we can objectively measure the subjective quality of an argument.'
canonical_url: https://jonathanwarden.com/bayesian-argumentation/

---

## Quantifying Argument

What makes for a *good* argument?

From a logical point of view, a good argument is logically sound. But in the real-world people rarely argue with pure logic. 

From a rhetorical point of view, a good argument is one that is convincing. But how can this be measured?

In this series of essays, we present a Bayesian model of argumentation, where arguments are treated as information that may cause a Bayesian rational agent to modify their beliefs. Given a model of the beliefs of some Bayesian "subject", we can objectively define "subjective" measures of an argument's quality or strength, such as **relevance**, **persuasiveness**, and **sufficiency**.

This perspective can provide some powerful insights about argumentation for people working in artificial intelligence, law, argument mapping software, or in our case, design of [social protocols](https://social-protocols.org).

Consider the following example.


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

As everyone knows, a Bayesian rational agent updates their beliefs when they acquire new information. An argument that is not informative to the agent therefore can't be persuasive. But a Bayesian model allows us to calculate what an agent **would** believe if they did not have that information (e.g. if they thought for some reason the candidate *didn't* have a pulse). This simple insight cracks open a number of ways of measuring argument **strength** other than just persuasiveness. In the essays on 
[Relevance and Corelevance](/relevance-and-corelevance) and [Necessity and Sufficiency](/necessity-and-sufficiency) we will define these measure and see how they all relate mathematically to the informativeness of the argument.

The Bayesian model of argumentation also allows us to take into account the reliability of the arguer themselves. A Bayesian rational agent will only update their beliefs if they believe the information given them. So for an argument to be informative it must not just be **new information**, it must also be believable. This perspective shows us when what looks like an *ad hominim* fallacy may somteimes be perfectly rational[^3]. These ideas are discussed in the essays on [informativeness and persuasiveness](/informativeness-and-persuasiveness).

## Introductory Example 2

Now consider another example argument: *the car won't start* because *the car is out of gas*. If the subject previously believed the car had gas, then this new information might well be persuasive. But suppose the subject accepts this information, but also believes that *the car's battery is dead*? With this assumption, the car being out of gas is now in a sense now irrelevant. 

Clearly the relevance of an argument depends on context: it depends on other beliefs the subject has about the state of world. 

Theoretically, if we have a model of some subject's beliefs about the world, we can identify the **corelevant** beliefs -- the beliefs cause the argument to be relevant. We will define this more precisely in the essay on [relevance and corelevance](/relevance-and-corelevance).


## Why Bayesian Argumentation?

Like all models, the Bayesian model of subjective belief is an incomplete description of the human mind. But it is clearly defined. Building clear terminology on top of a clear model helps clarify our thinking, facilitate discussion, and sharpen our intuition about what argument actually is. 

There is a lot of recent academic work on Bayesian argumentation[^1][^2]. These essay are intended not as an overview of current theory, but as a useful set of definitions and formulas for practitioners: specifically software engineers building practical applications of argumentation for AI, argument mapping systems, or in our particular case, design of [social protocols](https://social-protocols.org). Our goal is to provide a useful and clear vocabulary, with common-sense but precise definitions for common concepts related to argument strength. This can hopefully help clarify discussion among collaborators and prove useful in documentation and code.


## Argumentation Theory and Warrants

This idea of unexpressed beliefs that justify an argument evokes the idea of the **warrant** from the field of [argumentation theory](https://en.wikipedia.org/wiki/Argumentation_theory). Argumentation theory views argument as a kind of flexible, informal logic. Instead of arguing with logical syllogisms, we make simple statements, or **claims** which support other claims. For example, I might claim *people are wearing jackets* to support the claim *it's probably cold outside*. 

We use the terms **premise** and **conclusion** to differentiate between the supporting and supported claims. An **argument** is just a premise stated in support of some conclusion.

In every argument there is an unstated claim that *this premise supports this conclusion*. This doesn't need to be stated because it's implied by the fact that the argument was made. After arguing *it's probably cold outside* because *people are wearing jackets*, I don't need to add, pedantically, "and you see, if people are wearing jackets it must be cold outside".

The unexpressed premise that justifies the inferential leap from premise to conclusion is called the [**warrant**](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.). The warrant can be based on any kind of inferential rule (deductive, inductive, intuitive) or [argumentation scheme](https://en.wikipedia.org/wiki/Argumentation_scheme) (authority, analogy, example) -- whatever justifies the inference in the mind of the arguer. Some academics use different terms for these concepts: our terminology is influenced by the influential [Toulmin model](https://owl.purdue.edu/owl/general_writing/academic_writing/historical_perspectives_on_argumentation/toulmin_argument.html#:~:text=Toulmin%2C%20the%20Toulmin%20method%20is,the%20grounds%2C%20and%20the%20warrant.), except we prefer the traditional terms **premise** and **conclusion** over **grounds** and **claim**. More precise definitions of our terms are given in the [Deliberati Argument Model](/argument-model).

## A Bayesian Definition of Warrant

In Bayesian terms, a rational agent is said to acquire **evidence**, which causes them to change their belief in the probability of some **hypothesis** (see this [Bayesian Inference Primer](/bayesian-inference-primer)).

There is clearly an analogy here: **evidence is to hypothesis as premise is to conclusion**. But what is the warrant? 

The warrant clearly has to do with the subject's **prior beliefs**, because a Bayesian agent's priors are precisely what justify, in their mind, any inferential leap from premise to conclusion.

For example, if our subject is more likely to believe that (𝐴) *it is going to rain today* if they believe that (𝐵) *the sky is cloudy* than if they do not, then there clearly exists a warrant justifying, in the subject's mind, the inferential leap from 𝐵 to 𝐴.

But **why** does this warrant exist in the subject's mind? What is the inferential rule that actually justifies the inference? Is it a deductive inference? Inductive? Gut feeling?

We can't necessarily answer this question, because a Bayesian agent's beliefs are modeled by a simple probability distribution, which gives us the end result of the agent's internal belief structure, but not how they got there.

If the prior beliefs of our subject are represented by the probability measure $P$, then we can at least say that, in the mind of the subject, **a warrant exists justifying the inference from premise 𝐵 to conclusion 𝐴 iff**:

$$
    P(A|B) ≠ P(A|\bar{B})
$$

If the warrant exists, we say that 𝐵 is **relevant** to 𝐴. Otherwise, we say it is **irrelevant**.

## Summary

So in a Bayesian argument, an arguer asserts a **premise** in support/opposition to some **conclusion**, and if the premise is **relevant** -- the subject is more likely to believe the conclusion if they believe the conclusion -- then there must be some **warrant** justifying the inference from premise to conclusion.


## This Series

In the [next essay](/relevance-and-corelevance) in this series, we will formally define a measure of **relevance** from a Bayesian perspective and discuss some of its mathematical properties. In the remaining articles in this series we will define measures of **necessity**, **sufficiency**, **informativeness**, and **persuasiveness**, all of which relate back to this central concept of relevance.

- [Relevance and Corelevance](/relevance-and-corelevance)
- [Necessity and Sufficiency](/necessity-and-sufficiency)
- [Informativeness and Persuasiveness](/informativeness-and-persuasiveness)

## Summary of Definitions

But skipping ahead, here is a tl;dr; of all the terms that will be defined in the essays in this series:

*For an argument with premise 𝐵 and conclusion 𝐴, and a subject whose beliefs are represented by probability measure P...*

- **Relevant**: The premise is **relevant** to the conclusion (or, the argument is relevant) **iff** $P(A \vert B) ≠ P(A \vert \bar{B})$
    - Otherwise, the premise is **irrelevant** to the conclusion  (or, the argument is irrelevant)
        - Irrelevance implies statistical independence of A and B.

- **Support**: The premise **supports** the conclusion **iff** $P(A \vert B) > P(A \vert \bar{B})$

- **Oppose**: The premise **opposes** the conclusion **iff** $P(A \vert B) < P(A \vert \bar{B})$
    - If 𝐵 supports 𝐴, then 𝐵 opposes $\bar{A}$

- **Relevance**: The relevance of the premise to the conclusion is $R(A,B) = P(A \vert B) - P(A \vert \bar{B})$

- **Conditional Relevance**: *Given some third premise 𝐶*: $R(A,B \vert C) = P(A \vert B,C) - P(A \vert \bar{B},C)$

- **Corelevant**: The premises 𝐵 and 𝐶 are corelevant to the conclusion 𝐴 iff: $R(A,B \vert C) ≠ R(A,B \vert \bar{C})$

- **Corelevance**: $CR(A;B,C) = R(A,B \vert C) - R(A,B \vert \bar{C}) = R(A,C \vert B) - R(A,C \vert \bar{B})$

- **Necessity**: The necessity of the premise to the conclusion is $N(A,B) = P(A) - P(A \vert \bar{B}) = P(B)R(A,B)$

- **Sufficiency**: The sufficiency of the premise for the conclusion is $S(A,B) = P(A \vert B) - P(A) = P(\bar{B})R(A,B)$

- **Testimony Event**: The event, directly observed by the subject, that the arguer asserted the premise in support of the conclusion.

- **Post-Argument Belief**: *Given the testimony event I*: $P_i(∙) = P(∙ \vert I)$
    - e.g. $P_i(B) = P(B \vert I)$ is the post-argument belief in 𝐵.

- **Informative**: The assertion of the premise is **informative** (the argument is informative) **iff** $P_i(B) > P(B)$

- **Informativeness**: The informativeness of the argument is $P_i(B) - P(B)$

- **Persuasive**: The argument is **persuasive** **iff** $P_i(A) > P(A)$
    - Alternatively, the argument is **persuasive** if the argument is relevant and informative

- **Persuasiveness**: The persuasiveness of the argument is $P_i(A) - P(A)$


<!--

- **Necessity for Relevance**: The **necessity of claim 𝐶 for the relevance of premise 𝐵 to conclusion 𝐴** is the difference (absolute, percent, relative entropy, etc.) between $R(A,B)$ and $R(A,B \vert \bar{C})$$

- **Warrant**: The warrant is the premise with the highest necessity for the relevance of the premise to the conclusion. 
    - Given a set Σ of claims that the subject believes in, the warrant is:
    $ \text{arg}\,\max\limits_{S ∈ Σ}\,-R(A,B \vert \bar{S}) $$
-->


## Key Equations

And here is a summary of key equations in this series: $\label{1}$

- Jeffrey's Rule: $$P'(A) = P(A \vert \bar{B}) + P'(B)R(A,B)\tag{1} $$
- Relevance of Rejection of Premise/Conclusion: $R(A,B) = -R(A,\bar{B}) = -R(\bar{A},B) = R(\bar{A},\bar{B})$
- Symmetry of Corelevance: $CR(A;B,C) = CR(A;C,B)$
- Necessity = Relevance × Acceptance: $N(A,B) = P(A) - P(A \vert \bar{B}) = R(A,B)P(B)$
- Sufficiency = Relevance × Rejection: $S(A,B) = P(A \vert B) - P(A) = R(A,B)P(\bar{B})$
- Relevance = Necessity + Sufficiency: $R(A,B) = N(A,B) + S(A,B)$
- Sufficiency/Necessity of Rejection of Premise/Conclusion: $N(A,B) = S(\bar{A},\bar{B})$ and $S(A,B) = N(\bar{A},\bar{B})$
- Persuasiveness = Relevance × Informativeness: $ P_i(A) - P(A) = (P_i(B) - P(B))R(A,B) $


## Numerical Example

The following example illustrates all of the concepts introduced in this series.

Suppose the priors of the subject are modeled by the probability measure 𝑃 given in this table: 

| a | b | P(a,b)     |
| - | - | ---------- |
| $\bar{A}$ | $\bar{B}$ |  .25       |
| $\bar{A}$ | 𝐵 |  .10       |
| 𝐴 | $\bar{B}$ |  .25       |
| 𝐴 | 𝐵 |  .40       |

The marginal probabilities are:

$$
\begin{aligned}
    P(A) &= P(A,B) + P(A,\bar{B}) = .40 + .25 = .65 \cr
    P(B) &= P(A,B) + P(\bar{A},B) = .40 + .10 = .50
\end{aligned}
$$

And the conditional probabilities:

$$
\begin{aligned}
    P(A|B) &= \frac{P(A,B)}{P(B)} = \frac{.4}{.5} = .8  \cr
    P(A|\bar{B}) &= \frac{P(A,\bar{B})}{P(\bar{B})} = \frac{.25}{(1 - .5)} = .5
\end{aligned}
$$

**Relevance**


Which lets us calculate the relevance:

$$
    R(A,B) = P(A|B) - P(A|\bar{B}) = .8 - .5 = .3
$$

**Necessity and Sufficiency**

The necessity of 𝐵 to 𝐴 is:

$$
    N(A,B) = P(A) - P(A|\bar{B}) = .65 - .5 = .15
$$

And the sufficiency of 𝐵 to 𝐴 is:

$$
    S(A,B) = P(A|B) - P(A) = .8 - .65 = .15
$$

Notice that relevance is the sum of necessity and sufficiency:

$$
    R(A,B) = N(A,B) + S(A,B) = .15 + .15 = .3
$$

And that necessity is relevance times acceptance:

$$
    N(A,B) = R(A,B)P(B) = .3 \times .5 = .15
$$

And that sufficiency is relevance times rejection:

$$
    N(A,B) = R(A,B)(1 - P(B)) = .3 \times (1 - .5) = .15
$$



<!--
The necessity expressed as information gain would be (using log base 2):

$$
\begin{aligned}
    &P(A) log(\frac{P(A)}{P(A|\bar{B})}) + P(\bar{A}) log(\frac{P(\bar{A}))}{P(\bar{A}|\bar{B}}) \cr 
    &=  0.8 log(\frac{0.8}{0.3}) + (1-0.8) log\frac{(1-0.8)}{(1-0.3)} \cr
    &= .25 \text{ bits of information}
\end{aligned}
$$


p = .5
q = .77
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))

import math
p = .8
q = .3
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))

-->
**Post-Argument Belief**

Now suppose the assertion of 𝐵 in support of 𝐴 causes the subject to increase their belief in 𝐵 from $P(B)=50\\%$ to $P_i(B)=90\\%$.

The subject's post-argument belief in 𝐴 will be, according to formula $\eqref{1}$:

$$
\begin{aligned}
    P_i(A)  &= P(A|\bar{B}) + P_i(B)R(A,B)  \cr
            &= .5 + .9 \times .3  \cr
            &= .77
\end{aligned}
$$

This is slightly less than $P(A \vert B)=.8$ because the subject still harbors some doubt about 𝐵.



**Informativeness**

The **informativeness**  is:

$$
    P_i(B) - P(B) = 0.9 - 0.5 = 0.4
$$

<!--
$$
\begin{aligned}
        &P_i(B) log(\frac{P_i(B)}{P(B)}) + P_i(\bar{B}) log(\frac{P_i(\bar{B})}{P(\bar{B})})  \cr
        &= P_i(B) log(\frac{.99}{.5}) + P_i(.01) log(\frac{.01}{.5}) \cr
        &= 0.53 \text{ bits of information}

\end{aligned}
$$
-->

<!--
import math
p = .90
q = .5
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))
-->

**Persuasiveness**

And the persuasiveness is:

$$
    P_i(A) - P(A) = 0.77 - 0.65 = 0.12
$$

Notice that persuasiveness is equal to relevance times informativeness:

$$
   P_i(A) - P(A) = R(A,B)(P_i(B) - P(B)) = 0.3 × (0.9 - 0.5) = 0.12
$$

**Post-Argument Necessity and Sufficiency**

If **after** the argument the subject were to learn **additional** information causing them to reject 𝐵, the **new** posterior would be $P_i(A \vert \bar{B}) = P(A \vert \bar{B}) = .5$. 

The post-argument necessity is therefore:

$$
    N_i(A,B) = P_i(A) - P_i(A | \bar{B}) = .77 - .5 = .27
$$

And if the subject were to learn additional information causing them to accept $B$ completely, then new posterior would be $P_j(A) = P_i(A \vert B) = P(A \vert B) = .8$.

The post-argument sufficiency is therefore:

$$
    S_i(A,B) = P_i(A \vert B) - P_i(A) = .8 - .77 = .03
$$

[^1]: Hahn, U., & Oaksford, M. (2007). The rationality of informal argumentation: A Bayesian approach to reasoning fallacies. (https://psycnet.apa.org/record/2007-10421-007) Psychological Review, 114(3), 704–732. https://doi.org/10.1037/0033-295X.114.3.704
[^2]: Hahn, U., Oaksford, M., & Harris, A. J. L. (2013). Testimony and argument: A Bayesian perspective. (https://psycnet.apa.org/record/2013-00206-002). In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 15–38). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_2
[^3]: Oaksford, M., & Hahn, U. (2013). Why are we convinced by the ad hominem argument?: Bayesian source reliability and pragma-dialectical discussion rules. In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 39–58). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_3



<!--

Persuasiveness expressed as information gain would be:

$$
\begin{aligned}
    &P_i(A) log(\frac{P_i(A)}{P(A)}) + P_i(\bar{A}) log(\frac{P_i(\bar{A})}{P(\bar{A})}) \cr 
    &=  0.8 log(\frac{0.8}{0.5}) + (1-0.8) log\frac{(1-0.8)}{(1-0.5)} \cr
    &= 0.32\text{ bits of information}
\end{aligned}
$$
-->

<!--
p = .5
q = .77
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))
-->




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






