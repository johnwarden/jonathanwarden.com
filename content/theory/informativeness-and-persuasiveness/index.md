---

layout: single
title:  "Informativeness and Persuasiveness"
toc: true
toc_sticky: true
weight: 73
tags: ['Argumentation Theory']
sidebar:
  - title: "In This Series"
    nav: "bayesian-argumentation"
  - nav: "bayesian-argumentation-related"
    title: "Related Articles"
series: ['Bayesian Argumentation']

---

## Why Accept the Premise?

In the [previous essay](/necessity-and-sufficiency) in this series, we defined the ideas of **necessity and sufficiency** from the perspective of a Bayesian rational agent. If an argument is *necessary*, then if the subject were to reject the premise, they would decrease their acceptance of the conclusion. And if an argument is *sufficient*, then if the subject were to accept the premise, they would increase their acceptance of the conclusion. 

But why would the subject accept the premise? A Bayesian rational agent only changes their beliefs in light of evidence. But what evidence has the arguer given? When someone makes an argument, all they do is assert that the premise is true. **The only direct evidence is the observation that the arguer asserted that the premise was true**.

For example, suppose Bob asserts that (𝐵) *the sky is cloudy*, in an attempt to convince Alice that (𝐴) *it is going to rain today*. Alice's posterior belief in 𝐵, after hearing Bob's argument, will be

$$
    P(B|Bob~claimed~that~B)
$$

If Alice can see the sky through her window and already knows the sky is cloudy, then Bob's claim provides no new information. But if she can't see the sky, why does the event that *bob claimed that the sky was cloudy* change her beliefs?

There are various reasons that someone else merely asserting a premise would induce a rational human being to change their beliefs. 

1. First, they may **simply believe the person making the argument**. For a Bayesian agent, the **testimony** of the arguer -- the fact that they asserted something to be true -- is itself information. How that testimony effects the agent's belief in the premise depends on how reliable they believe the arguer to be, how plausible they find the premise, and what other ulterior motives they believe the arguer may have for making the claim. For a deeper discussion on a Bayesian perspective on agent reliability and testimony, see [this paper](https://psycnet.apa.org/record/2013-00206-002)[^1].

2. Second, even if they initially doubt the premise, the agent may be **prompted to seek information** about the claim. Bob's assertion may prompt Alice to look out the window. Or the claim my include a link or reference to a trusted source.

3. Third, an argument may simply **remind them of knowledge they had temporarily forgotten**. My spouse reminding me *I bought milk yesterday* can convince me not to go to the grocery store even if it is something I already knew.

4. Forth, they may be **convinced by reasoning that provides no new information**. My belief about the answer to a math problem can change when someone shows me how to solve it. But the solution is only the logical consequence of my existing beliefs, not new information about the world. 

## Imperfect Bayesians

A rational agent that actively seeks and forgets information is not exactly a perfect Bayesian. **But regardless of the actual reason** that the assertion causes the subject to change their beliefs, we can treat the assertion **as if it were simply new information** that caused them to update their beliefs according to the rules of Bayesian belief revision. We can then **model** our subject as a perfect Bayesian with beliefs represented by the prior probability distribution that would result in the same posterior distribution given this new information.

So we can **model** an imperfect Bayesian as an equivalent perfect Bayesian.


## Definition of Post-Argument Belief

We'll use $I$ to indicate the **testimony event**. This is the event, which is directly observed by the subject, that the argued premise 𝐵 in support of conclusion 𝐴. We always assume that the subject trusts the medium of communication, and their own senses, and so accepts 𝐼 as 100% true, even if they don't accept 𝐵.

We use $P_i$ to denote the **Post-Argument beliefs** of the subject, defined as:

$$
    P_i(∙) = P(∙|I) = P(∙|X\text{ argued }B\text{ in support of }A)
$$

So for example $P_i(B)$ is the subject's opinion on 𝐵 after the testimony event 𝐼, and $P_i(A)$ is their opinion on 𝐴 after the testimony event.

## Definition of Informative

An argument that causes the subject to change their belief in the premise of the argument is **informative**. That is, an argument with premise 𝐵 is informative if:

$$
    P_i(B) > P(B)
$$

An argument is said to be informative if the assertion of its premise is informative.

The difference between the prior and posterior (post-argument) belief in 𝐵 can be quantified in various ways (percent difference, absolute difference, ratio, etc.). We will focus on the absolute difference. 

## Definition of Informativeness

**The informativeness of an argument with premise 𝐵 is:**

$$
    P_i(B) - P(B)
$$

<!--

, but it is especially interesting to take an information-theoretic point of view, and quantify the amount of [**information gain**](https://en.wikipedia.org/wiki/Information_gain_in_decision_trees) from using the more Post-Argument probability distribution $P_i$ instead of $P$. This can be calculated as the [**relative entropy**](https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence#Interpretations) or KL-Divergence between the pre- and post-argument belief in 𝐵. Using $p$ to indicate the probability distribution $P$ limited to events B and B̅.

$$
\begin{aligned}
    D_{KL}(p_i || p) &= - ∑_b p_i(b) log(\frac{p_i(b)}{p(b)}) \cr
                                &= - P_i(B) log(\frac{P_i(B)}{P(B)}) - P_i(\bar{B}) log(\frac{P_i(\bar{B})}{P(\bar{B})})
\end{aligned}
$$
-->



## Definition of Persuasive

**An argument is persuasive iff**:

$$
    P_i(A) ≠ P(A)
$$

Persuasiveness can be measured as the difference between $P_i(A)$ and $P(A)$, expressed as a ratio, percent difference, information gain, etc. We will focus on the absolute difference. 

## Definition of Persuasiveness

The **persuasiveness of an argument with premise 𝐵 and conclusion 𝐴 is**:

$$
    P_i(A) - P(A)
$$


<!--
An information-theoretic measure of persuasiveness would be the [**information gain**](https://en.wikipedia.org/wiki/Information_gain_in_decision_trees) from using the more Post-Argument probability distribution $P_i(a)$ instead of $P(a)$, measured as relative entropy:

$$
    D_{KL}(P_i(a) \vert\vert P(a)) &= - ∑_{a∈\{A,\bar{A}\}} P_i(a) log(\frac{P_i(a)}{P(a)}) \cr
                                   &= - P_i(A) log(\frac{P_i(A)}{P(A)}) - P_i(\bar{A}) log(\frac{P_i(\bar{A})}{P(\bar{A})}) 
$$

-->



## Persuasiveness = Relevance × Informativeness 

In [Relevance and Corelevance](/relevance-and-corelevance/#relevance-as-slope), we introduced Jeffrey's Rule of Conditioning, which says that if a Bayesian reasoner acquires information that has no effect other than to cause them to increase their belief in the premise, then their posterior belief in the conclusion changes according to the formula:

$$
\label{1}
P'(A) = P(A|\bar{B}) + P'(B)R(A,B) 
\tag{1}
$$

So if the argument is informative, their posterior belief in $B$ will be $P_i(B)$ and therefore their posterior belief in $A$ will be:

$$ 
P_i(A) = P(A|\bar{B}) + P_i(B)R(A,B) 
$$ 

So as long as $R(A,B)$ is not equal to zero, then a change in $P_i(B)$ will result in a change in $P_i(A)$. So a relevant and informative argument **must** also be persuasive. 

In fact, persuasiveness is the product of relevance and informativeness ([proof](#proof-1)).

$$
    P_i(A) - P(A)  = (P_i(B) - P(B))R(A,B)
$$


<!--
<aside class="proof" markdown="1">

**Proof**:

If

$$
    R(A,B) > 0 \text{ and } P_i(B) > P(B)
$$

Then:

$$
\begin{aligned}
    P_i(A) &= P(A|\bar{B}) + P_i(B)R(A,B)           &&\eqref{1} \cr
           &> P(A|\bar{B}) + P(B)R(A,B)             &&\text{(}P(B) > P_i(B) \cr
           &                                   &&\text{and } R(A,B) > 0\text{)}\cr
           &> P(A|\bar{B}) + P(B)(P(A|B)-P(A|\bar{B}))  &&\text{(Definition of R)} \cr
           &> P(A|B)P(B) + P(A|\bar{B})(1-P(B))  &&\text{(Algebra)} \cr
           &> P(A)                              &&\text{(Law of total prob.)}
\end{aligned}
$$
</aside>

-->

## Conclusion

So we can look at argument as the **exchange of information among Bayesian reasoners**, except the information is merely the fact that one of them asserted some premise. But these **assertions can influence belief** under certain conditions.

First, the assertion must be **informative**. The assertion itself must effectively be new information that causes the subject to change their belief in the premise (even if the assertion induces them to change their belief by actively seeking new information, fixing their reasoning, etc.). 

Second, the premise must be **relevant** to the conclusion, which means the belief in the conclusion has a linear relationship with belief in the premise, as illustrated in [Chart 1 from Relevance and Corelevance](/relevance-and-corelevance/#chart1). When this is the case, the agent will necessarily revise their beliefs in the conclusion according to Jeffrey's Rule $\eqref{1}$. The argument can then said to be **persuasive**.

However, if the argument was not informative, it is not necessarily a bad argument. The subject may already believe in the premise, and it may still be **necessary**, because the premise forms the basis for the subject's belief in the conclusion in that the subject **would** change their belief in the conclusion if they were for any reason to reject the premise.


<!--

However, this account of argumentation still doesn't account for arguments such as *the car won't start because it is out of gas*, in the case that the battery is also dead. This is clearly a good argument, and yet by this account it is not relevant and therefore not persuasive, because changing belief in the premise would have little or not effect on belief in the conclusion. 

So clearly this account of argumentation is still limited. And yet it provides at least some intuition on what might be going on in the minds of people engaged in argument when they perceive arguments as relevant, persuasive, necessary, etc. This idea of a Bayesian Reasoner who changes their belief through argument is the basis for the idea of the [Meta Reasoner](/the-meta-reasoner) and the logic of [Distributed Bayesian Reasoning](/distributed-bayesian-reasoning-introduction).

-->






<!--
This means that we can just as well consider 𝐵 to be the "rule" that justifies the inference from the 𝐶 to the conclusion. 


[TODO: we havent' introdued this example]

If someone where to argue that (𝐶) *A Swede can be taken almost certainly not to be a Roman Catholic* in support of (𝐴) *Petersen is not a Roman Catholic*, then this seems to imply that (𝐵) *Petersen is a Swede*, because Petersen being a Swede is the only thing that makes Swedes not being Roman Catholic relevant.
-->

<!--

## Corelevance as Slope

This equation shows us how P(A) varies with P(B) and P(C). 

$$
    P(A) = P(A|notB,notC) + P(B)R(A,B|notC) + P(C)R(A,C|notB) + P(B,C)CRD(A,B,C)
$$

P(A|notB,notC) is the minimum value of P(A) if the subject rejects both B and C. If R(A,B|notC) is nonzero, it means there is the component of relevance of B to A that exists independently of the subjects acceptance of C. And vice versa if R(A,C|notB) is nonzero. CRD(A,B,C) can be thought of as the component of relevance of the conjunction of B and C to A. 
-->



## Proofs



### Proof 1

**Persuasiveness = Relevance × Informativeness**

$$
    P_i(A) - P(A) = R(A,B)(P_i(B) - P(B))
$$

**Proof:**


This proof uses the following equality defined in the [previous essay](/relevance-and-corelevance#relevance-as-slope).

$$
\label{2}
P(A) = P(A|\bar{B}) + P(B)R(A,B) 
\tag{2}
$$

Then

$$
\begin{aligned}
P_i(A)  &= P(A \vert \bar{B}) + P_i(B)R(A,B) && \text{Formula }\eqref{1}\cr
        &= P(A \vert \bar{B}) + P_i(B)R(A,B) - P(B)R(A,B) + P(B)R(A,B)\cr
        &= P(A \vert \bar{B}) + (P_i(B) - P(B))R(A,B) + P(B)R(A,B) \cr
        &= ( P(A \vert \bar{B}) + P(B)R(A,B) ) + (P_i(B) - P(B))R(A,B) && \text{Formula }\eqref{2}\cr
        &= P(A) +  (P_i(B) - P(B))R(A,B)
\end{aligned}
$$

So:

$$
    P_i(A) - P(A) = R(A,B)(P_i(B) - P(B))
$$

[^1]: Hahn, U., Oaksford, M., & Harris, A. J. L. (2013). Testimony and argument: A Bayesian perspective. (https://psycnet.apa.org/record/2013-00206-002). In F. Zenker (Ed.), Bayesian argumentation: The practical side of probability (pp. 15–38). Springer Science + Business Media. https://doi.org/10.1007/978-94-007-5357-0_2



