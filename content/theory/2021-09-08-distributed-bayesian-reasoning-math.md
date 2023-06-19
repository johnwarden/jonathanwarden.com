---
layout: single
title:  "Distributed Bayesian Reasoning Math"
date:   2021-10-27 00:00:00 +0200

weight: 110
# toc_sticky: true
series: ['Distributed Bayesian Reasoning']
# categories: ["social-protocols"]

sidebar:
  - title: "In This Series"
    nav: "distributed-bayesian-reasoning"
  - nav: "distributed-bayesian-reasoning-related"
    title: "Related Articles"
  - title: ""
    text: "
    <h5>Sample Argument Used in this Article</h5>
            <img src='/assets/images/distributed-bayesian-reasoning/sample-argument-reference.svg'
                 alt='Simplified Argument'
                 style='display: block; margin-left: 0px; margin-right: 0px; padding-left: 0px; padding-right: 0px; max-width: 350px' />
     "

# image: assets/images/distributed-bayesian-reasoning/jeffrey-conditioning-formula.png


---


In this article we develop the basic mathematical formula for calculating the opinion of the meta-reasoner in arguments involving a single main argument thread.

<!--more-->

## Background Reading

To understand this article you should first read:

- [Introduction to Distributed Bayesian Reasoning](/distributed-bayesian-reasoning-introduction)
- [The Meta-Reasoner](/the-meta-reasoner)

For a deeper understanding of some of the assumptions we make in this article, it may also help to read:

- [The Argument Model](/argument-model)

You also need familiar with basic syntax and concepts from probability theory, specifically:

- the definition of [conditional probability](https://en.wikipedia.org/wiki/Conditional_probability)
- the [law of total probability](https://en.wikipedia.org/wiki/Law_of_total_probability)


## Sample Argument

<!-- {{< figure src="/assets/images/distributed-bayesian-reasoning/sample-argument-reference.svg" title="Sample Argument" >}} -->




<img src='/assets/images/distributed-bayesian-reasoning/sample-argument-reference.svg'
                 alt='Simplified Argument'
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 400px' />

Suppose a sensational murder trial is being discussed in an online platform that allows the general public to vote on what they think the verdict should be and why.

Initially, 1,000 users vote on the root claim (𝐴) *the defendant is guilty*, before any discussion has taken place on the platform. Then after this initial vote, somebody submits an argument claiming (𝐵) *the defendant signed a confession*, and users are asked to vote on this claim. 

150 out of the 1,000 users vote on 𝐵. Of these 150 users, a small number changed their vote on 𝐴 after voting on 𝐵 (presumably, because they found 𝐵 convincing). 

The final votes are tabulated in the following table. We represent votes using the numeric values **0=reject**, **1=accept**, and **-1=didn't vote**. 


|          | A=-1 | A=0 | A=1 |  SUM
| -------- | ---- | --- | --- |  ----
| **B=-1** |  0   | 455 | 395 |  850
| **B=0**  |  0   | 25  | 25  |  50
| **B=1**  |  0   | 20  | 80  |  100
| **SUM**  |  0   | **500** | **500** | **1000**
| **B≥0**  |  0   | 45  | 105 |  150


According to this table, all 1,000 users voted on 𝐴, with 500 rejecting 𝐴 (𝐴=0) and 500 accepting 𝐴 (𝐴=1). But only 150 users voted on 𝐵 (𝐵≥0).

## Raw Probabilities

Our first step is to convert these counts into probabilities

Let's define a function 𝑐 that returns the values of a cell in this table. For example:

$$
\begin{aligned} 
    𝑐(A=1)     &= 500 \cr
    𝑐(A=1,B=0) &= 25 \cr
    𝑐() &= 1000
\end{aligned}
$$


From this, we can define a function 𝑃 that tells us the probability that a random user voted in some way:


$$
    P(A=a) = \frac{𝑐(A=a)}{c()}
$$

So for example: 

$$
\begin{aligned} 
    P(A=1) &= \frac{𝑐(A=1)}{c()} \cr
            &= \frac{500}{1000} = 50\\% 
\end{aligned}
$$



We can also define [conditional probabilities](https://en.wikipedia.org/wiki/Conditional_probability), for example the probability that a random user accepts 𝐴 given they accept 𝐵 is:

$$
    P(A=1|B=1) = \frac{P(A=1,B=1)}{P(B=1)} 
$$

We can calculate conditional probabilities just by taking the ratio of counts, because:


$$
\begin{aligned}
    P(A=a|B=b) &= \frac{P(A=a,B=b)}{P(B=b)}\cr \cr
           &= \frac{𝑐(A=a,B=b) \div c()}{ 𝑐(B=b) \div c() }\cr \cr
           &= \frac{𝑐(A=a,B=b)}{𝑐(B=b)} 
\end{aligned}
$$


So for example 


$$
\begin{aligned} 
    P(A=1|B=1) &= \frac{𝑐(A=1,B=1)}{𝑐(B=1)} \cr
                &= \frac{80}{100} = 80\\%
\end{aligned}
$$



Note that, $P$ represents the probability that a randomly selected user, **from among those who voted**, votes in some way. If this sample of users is small or biased, $P$ may not be a good estimate of what an average person actually believes. We will ignore this detail in this article, but address it in [Bayesian Averaging](/distributed-bayesian-reasoning-bayesian-averaging).



## Informed Probabilities


$P(A=1)$ is only 50%, but $P(A=1 \vert B=1)$ is 80%. This means that users who accept claim 𝐵 are more likely to accept claim 𝐴. So 𝐵 apparently is an effective supporting argument for 𝐴. On the other hand $P(A=1 \vert B=0)$ = 25/50 = 50%. Users who reject 𝐵 are not more likely to accept 𝐴. 

Notably, among users who either **accept OR reject 𝐵**, 70% of users accept 𝐴:


$$
\begin{aligned} 
    P(A=1 | B ≥ 0) &= \frac{c(A=1, B ≥ 0)}{c(B ≥ 0)}\cr \cr
        &= \frac{105}{150} = 70\\%
\end{aligned}
$$


While only 50% of users accept 𝐴 overall. Apparently simply **voting on 𝐵** made users more likely to accept 𝐴.

What's happening here is that, among users who voted on $B$, a large number accept $B$ as true, and as we've seen users who accept $B$ are more likely to accept $A$. What makes the group of users who voted on $B$ different is that all of them are **informed about 𝐵**.  Whether they accept it as true or not, they have at least been presented with the claim that (𝐵) *the defendant signed a confession* and had a chance to reject it, or to accept it and revise their belief accordingly. This is not necessarily the case for the larger group of users: perhaps the media coverage of the murder never mentioned any confession, and most users never learned about it until they were asked to vote on claim $B$.

This is just made-up data, but it is meant to illustrate something that is often the case in reality: **arguments can change minds** -- **especially if they provide new information**.

Our goal is to calculate the beliefs of the [Meta-Reasoner](/the-meta-reasoner): a hypothetical **fully-informed** user who shares the knowledge of all the other users. So the opinion of users who voted on 𝐵 is probably a better estimate of a fully-informed opinion.

So we'll call the users who voted on 𝐵 the **informed users**, and our first step is estimating the beliefs of the meta-reasoner will be to represent the opinion of the average informed user with the **informed probability** function $ P_i $:


$$
    P_i(A) = P(A|B≥0)
$$


So 


$$
    P_i(A=1) = 𝑃(A=1|B≥0)
$$


Which we have already calculated to be 70%.


## The Law of Total Probability

The informed opinion on 𝐴 depends on 1) the probability that an informed user actually accepts 𝐵, and 2) the probability that a user who accepts 𝐵 also accepts 𝐴. In fact we can rewrite the equation for $P_i(A)$ in terms of these probabilities. Since the set of users who accept 𝐵 and the set that reject 𝐵 *partition* the set of users who voted on 𝐵, the [law of total probability](https://en.wikipedia.org/wiki/Law_of_total_probability) says that:


$\label{1}$

$$
\begin{aligned} 
    P_i(A)    &= 𝑃(A=1|B≥0) \cr
              &= \sum_{b≥0} P_i(B=b)P_i(A|B=b)\cr \cr
              &=P_i(B=0)P(A|B=0)\cr \cr
              &+P_i(B=1)P(A|B=1)
\end{aligned}
\tag{1}
$$


We have already calculated $P(A=1 \vert B=0)=50\\%$ and $P(A=1 \vert B=1)=80\\%$ above, so it remains only to calculate $P_i(B=1)$:


$$
\begin{aligned} 
    P_i(B=1)
        &= P(B=1|B≥0)\cr \cr
        &= \frac{𝑐(B=1)}{𝑐(B≥0)}\cr  \cr
        &= \frac{100}{150} = 66⅔\\%\cr
\end{aligned}
$$


Plugging these values into $\eqref{1}$, we again get 70%.


$$
\begin{aligned} 
    P_i(A=1) &=P_i(B=0)P(A=1|B=0)\cr \cr
        &+P_i(B=1)P(A=1|B=1) \cr \cr
        &=(1 - 66⅔\\%)(50\\%) + (66⅔\\%)(80\\%) = 70\\%\cr \cr
\end{aligned}
$$


Formula $\eqref{1}$ is important because it shows us exactly how the probability that users accept 𝐵 determines the probability that they accept 𝐴. And critically, it shows us what the probability of accepting 𝐴 **would be** if the probability of accepting 𝐵 were different.

<!--
TODO: show linear function of Pi(A)
-->

## Distributed Reasoning

Now suppose a second group of 10 users holds an argument about whether to accept 𝐵, and during this argument users voted on claim 𝐺, *the signature was forged*. And suppose these users unanimously accept 𝐺 and found it very convincing: only 1/10 users accept 𝐵 after accepting 𝐺.

Clearly, the opinion of the meta-reasoner about 𝐵 will be equal to the opinion of the second group of voters, since this opinion is more informed, reflecting any new information conveyed by 𝐺.

Let's define a function $P_h$ that gives us the beliefs of the meta-reasoner. The beliefs of the meta-reasoner about $B$ is the informed opinion on $B$, which is the opinion of users who also voted on $G$:


$$
\label{2}
\begin{aligned} 
    P_h(B) &= P(B|G ≥ 0)
\end{aligned}
\tag{2}
$$


Let's put the vote counts from the sub-jury in a table:

|          | B=0 | B=1 | B ≥ 0
| -------- | --- | --- | -----
| **𝐺=0**  | 0  | 0    | 0
| **𝐺=1**  | 9  | 1    | 10
| **𝐺≥0**  | **9**  | **1** | **10** 

And now we can calculate:

$$
\begin{aligned} 
    P_h(B=1) &= P(B=1|G ≥ 0)  \cr
             &= \frac{c(B=1, G≥0)}{c(G≥0)} \cr
             &= \frac{1}{10} = 10\\\%
\end{aligned}
$$

Recall that $\eqref{1}$ tells us how belief in $B$ determines the first group of users' belief in $A$. So to calculate the probability that a member of the first jury would accept 𝐴 *if they held the beliefs of the second jury about 𝐵*, we simply substitute of $ P_h(B=b) $ in place of $ P_i(B=b) $ in $\eqref{1}$:

$$
\label{3}
\begin{aligned} 
    P_h(A) &= \left. \sum_{b≥0} P_i(B=b)P(A|B=b) \right\vert_{P_i=P_h}\cr \cr
           &= \sum_{b≥0} P_h(B=b)P(A|B=b) 
\end{aligned}
\tag{3}
$$


Plugging in the numbers:

$$
\begin{aligned} 
    P_h(A=1) =\space&𝑃_h(B=0)𝑃(A=1|B=0) \cr
            +\space&𝑃_h(B=1)𝑃(A=1|B=1)\cr \cr
           =&\space (1 - 10\\%)(50\\%) + (10\\%)(80\\%) = 53\\%
\end{aligned}
$$

The meta-reasoner's belief $𝑃ₕ(A=1)$ is very close to $𝑃(A=1 \vert B=0)=50\\%$ -- the average belief of users who voted on 𝐵 but rejected it -- because a fully-informed user would probably reject 𝐵.



<aside class="custom-aside" markdown="1">

### Front-Door Adjustment

To readers who are familiar with [Judea Pearl](https://en.wikipedia.org/wiki/Judea_Pearl)'s work on [graphical causal models](https://ftp.cs.ucla.edu/pub/stat_ser/r236-3ed.pdf), formula $\eqref{3}$ may look familiar: it is a 
[front-door adjustment](https://medium.data4sci.com/causal-inference-part-xii-front-door-criterion-38bec5172f3e). Given the causal graph (𝐺≥0) → 𝐵 → 𝐴:

$$
\begin{aligned} 
    P_h(A) = P(A|do(G≥0)) = \sum_{b≥0} P_h(b)P(A|b)
\end{aligned}
$$

A derivation of the above equation using the [do-calculus](https://plato.stanford.edu/entries/causal-models/do-calculus.html) is provided in the [Appendix](#derivation-1). The causal graph follows from the conditional independence assumption that we will discuss in the next section. 

Given this causal graph and our probability data, we've used the do-calculus to **simulate an intervention**: we have estimated the probability that a user **would believe** 𝐴 if they had voted on all claims ($A$, $B$, and $G$) in the argument, even though no single user has actually done so!


### Jeffrey's Rule

Formula $\eqref{3}$ is also Jeffrey's Rule: the general rule for Bayesian belief revision for situations where new information may come with uncertainty, as described in the Stanford [Stanford Encyclopedia of Philosophy article on Bayes Theorem](https://plato.stanford.edu/entries/bayes-theorem/#4):

> If a person with a prior such that $ 0 < P(B) < 1 $ has a learning experience whose sole immediate effect is to change her subjective probability for $B$ to $P_h(B)$, then her post-learning posterior for any $A$ should be [substituting our own terms]:
>
> $$ \begin{aligned}  𝑃ₕ(A)  \cr= &𝑃(A \vert B=1)𝑃ₕ(B=1) \cr + &𝑃(A \vert B=0 )(1 - 𝑃ₕ(B=1)) \end{aligned} $$
>

Which is of course again $\eqref{3}$ with the terms of the summation expanded. Note the requirements about the "sole immediate effect" requires a conditional independence assumption, discussed in the next section.



</aside>


## Causal Assumptions

### Conditional Independence

Formula $\eqref{3}$ is only valid if we assume the meta-reasoner forms their belief about (𝐴) *the defendant is guilty* entirely based on their belief about (𝐵) *the defendant signed a confession*. So their belief in (𝐺) *the signature was forged* does not effect their belief in 𝐴 *directly*, but only *indirectly* through 𝐵. In other words 𝐴 is [**conditionally independent**](https://en.wikipedia.org/wiki/Conditional_independence) of $G$ given 𝐵. We discuss the justification for making these causal assumptions in the [Meta-Reasoner](/the-meta-reasoner/#the-causal-model-and-the-justified-opinion).

Unfortunately, we can't make the same sort of assumptions about (𝐶) *the defendant retracted her confession*. 𝐶 does not effect belief in 𝐴 only through 𝐵: learning that the defendant retracted her confession may make less of an impression on a user who never believed the defendant signed a confession in the first place. So the effect of accepting 𝐶 on a user's acceptance of 𝐴 depends on whether or not that user accepts 𝐵.

The reason we can make the conditional independence assumption about 𝐺 and not 𝐶 is that 𝐺 is the premise of a [premise argument](/argument-model#premise-arguments), whereas 𝐶 is the premise of a [warrant argument](/argument-model#warrant-arguments). The difference between premise arguments and warrant arguments is discussed in more detail in the [Argument Model](/argument-model).

<aside class="custom-aside" markdown="1">

### Argument Threads

Below is a graph of our argument using the terminology and notation from the [argument model](/argument-model). The sub-graph in blue comprises a single [argument thread](/argument-model#argument-threads). 


<img src="/assets/images/distributed-bayesian-reasoning/sample-argument-reference-with-notation.svg"
     alt="Graph of Sample Argument Used in This Article"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 450px" />


As discussed in the section [Argument Threads are Dialogs](/argument-model/#argument-threads-are-dialogs) in the argument model, each argument in the argument thread should be interpreted as a claim that the premise of that argument, given acceptance of the premises of any preceding arguments in the thread, is a good reason to accept or reject the root claim. So rather than being conditionally independent, acceptance of 𝐴 given 𝐵 clearly depends on whether the user accepts 𝐶

So while we can create an independent sub-jury to decide whether or not the meta-reasoner accepts 𝐶, we can't use an independent sub-jury to decide how their acceptance of 𝐶 effects their acceptance of 𝐴. We need the main jury to consider the premises of the entire argument thread together, and tell us the probability that the average user -- and thus the meta-reasoner -- would accept 𝐴 given they accept the premises 𝐵 and 𝐶. 


</aside>

<style>
.custom-aside
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

aside h3 {
    margin-top: 0px;
}

</style>


## Formula for a 2-Argument Thread

Our next task is to calculate the opinion of the meta-reasoner after argument 𝐶 has been made. 

First, we need to update our definition of the informed opinion. Previously, we defined the informed opinion as the opinion of users who voted on 𝐵; now that we have a second premise 𝐶 in the [argument thread](/argument-model#argument-threads), we should include 𝐶 in the definition of informed opinion.

However, for users who reject 𝐵, what they think about 𝐶 is irrelevant, because (𝐶) *the defendant retracted her confession* is only argued as a way of convincing people who accept (𝐵) *the defendant signed a confession* that they still shouldn't accept 𝐴. We discuss this important concept in the the section [Argument Threads are Dialogs](/argument-model#argument-threads-are-dialogs) in the argument model.

So we'll define the informed opinion as the opinion of users who either reject 𝐵, or accept 𝐵 and have voted on 𝐶:

$$
\begin{aligned} 
    &P_i(A)    = P(A | (B=0 ∨ (B=1 ∧ C≥0)) )
\end{aligned}
$$

We can then rewrite the formula for $ P_i(A) $ using the law of total probability and some probability calculus. The derivation is similar to the derivation of $\eqref{1}$ and is shown in the [appendix](#derivation-2):



$$
\label{4}
    \begin{aligned} 
    P_i(A)    =~ &P_i(B=0) P(A|B=0) \cr
                &\begin{aligned}
                   +~ P_i(B=1)  &\sum_{c≥0} P_i(C=c|B=1)  \cr
                                &× P(A|B=1,C=c)
                \end{aligned}
    \end{aligned}
    \tag{4}
$$

Now, suppose a third sub-jury holds a sub-trial about whether to accept 𝐶, giving us $P_h(C=c)$. We can then plug in the opinions of the sub-juries $ P_h(B=b) $ and $ P_h(C=c) $ in place of $ P_i(B=b) $ and $P_i(𝐶=𝑐$\|$B=1)$ in $\eqref{4}$:

$$
\label{5}
    \begin{aligned} 
    P_h(A)    =~ &P_h(B=0) P(A|B=0) \cr
                &\begin{aligned}
                   +~ P_h(B=1)  &\sum_{c≥0} P_h(C=c)  \cr
                                &× P(A|B=1,C=c)
                \end{aligned}
    \end{aligned}
    \tag{5}
$$


This gives us us the posterior belief of the meta-reasoner $ P_h(A) $ as a function of the prior probability function $ P $ and the evidence from the sub-juries $ P_h(B=b) $ and $ P_h(C=c) $. 

Using the shorthand $ F[P, P_h(B=b), P_h(C=c)] $ to refer to the formula in $\eqref{5}$, we illustrated this calculation in the chart below:

<img src="/assets/images/distributed-bayesian-reasoning/argument-thread-with-formulas.svg"
     alt="Argument Thread"
     style="display: block; margin-left: auto; margin-right: auto; width: 700px" />


To show a sample calculation, suppose we obtain the following probabilities for users that have voted on 𝐴 and 𝐵, and 𝐶.

|𝐵  |𝐶     |  𝑃(𝐴\|𝐵,𝐶)
| --|----- | ----        
| 0 | -1   | 50% 
| 1 | 0    | 80% 
| 1 | 1    | 65% 

And suppose that the beliefs from the sub-juries are $P_h(B=1)=80\\%$ and $P_h(𝐶=1) = 60\\%$. Plugging these into $\eqref{5}$:

$$
    \begin{aligned} 
    P_h(A=1) = ~ &(1-80\\%)×50\\% \cr
                &+ 80\\%×(1-60\\%)×80\\% \cr
                &+ 80\\%×60\\%×65\\%\cr \cr
                = ~ &66.8\\%
    \end{aligned}
$$

Intuitively, this result reflects the fact that, although $B$ is an effective argument ($P(A=1 \vert B=1)=80\\%$) and the sub-jury mostly accepts it ($P_h(B=1)=80\\%$), 𝐶 is a fairly effective counter-argument ($P(A=1 \vert B=1,C=1) = 65\\%$).


## Formula for Long Threads

To generalize $\eqref{5}$, we first rewrite it in the more easily-generalizable form:

$$
    \begin{aligned} 
             P_h(A) =  ~ &\sum_{b≥0} P_h(B=b) × \textbf{ if } b=0 \textbf{ then } P(A|B=0) \textbf{ else }   \cr
               &~~ \sum_{c≥0} P_h(C=c|B=1) × P(A|B=1,C=c)\cr \cr
    \end{aligned}
$$

Now suppose underneath the claim α there is a thread with 𝑛 premises $ β = \{β_1, β_2, ... ,β_n\} $. Then:

$$
\label{6}
\begin{aligned} 
 P_h&(α=1) =\cr
    &\sum_{b_1≥0} P_h(β_1=b_1) × \textbf{ if } b_1=0 \textbf{ then } P(α=1|β_1=0) \textbf{ else }   \cr
    &~~\sum_{b_2≥0} P_h(β_2=b_2) × \textbf{ if } b_2=0 \textbf{ then } P(α=1|β_1=1, β_2=0) \textbf{ else }   \cr
    &\space\space\space...  \cr
    &\space\space\space\space\space\space \sum_{b_n≥0} P_h(β_n=b_n) × P(α=1|β_1=1, β_2=1, ... , β_n=1)
\end{aligned}
\tag{6}
$$

Note this function $P_h$ is recursive. The recursion terminates when it reaches a **terminal claim** in the argument graph -- a claim without any premise arguments underneath it -- in which case β will be ∅ and the function will therefore return 

    
$$
    P_i(α=1) = P(α=1 \vert ∅) = P(α=1)
$$

or the raw probability that a user accepts the terminal claim α.


## Next in this Series

We can now calculate the posterior beliefs of the meta-reasoner for fairly complex argument trees, comprising arbitrarily long argument threads, and arbitrarily deep nesting of juries and sub-juries. But what about cases where there are multiple premise arguments under a claim (each starting a thread), or even multiple [warrant arguments](/argument-model#warrant-arguments) under a premise argument?

We will address this issue, as well as the problem of sampling error, in the article on [Bayesian Averaging](/distributed-bayesian-reasoning-bayesian-averaging)

## Appendix


### Derivation 1


Let's define a new variable $J$ that indicates that a user has participated in the sub-jury and voted on G:

$$
    J ≝ G ≥ 0
$$

Note also that all participants in the sub-jury vote on 𝐵, so

$$
    J ⟹ G ≥ 0 ⟹ B ≥ 0
$$

Our causal assumptions are that: 

1. simply voting on 𝐵 (and thus being informed of the arguments for/against 𝐴) effects probability of accepting 𝐴 and.

2. 𝐵 is the **only** variable that directly effects the probability of accepting 𝐴 (the [conditional-independence](#conditional-independence# assumption).

These assumptions give us this causal graph:

$$
    𝐽 → 𝐵 → 𝐴
$$

We previously defined 

$$
    P_h(B) = P(B|G ≥ 0) = P(B|J)
$$

We now want to calculate

$$
    P_h(A) = P_i(A|do(J))
$$

That is, the probability that a user who voted on B **would** accept $A$ if they voted on $G$ (even though no user has actually done so).

$$
\begin{aligned} 
    P_h(A) &= P_i(A|do(J))  \cr  \cr
            &= \sum_{b} P_i(B=b|J) &&\text{front-door adj. formula} \cr
            &~~~~~~~~~× \sum_{j'}P_i(A|J=j',B=b)P_i(J=j') \cr \cr
            &= \sum_{b} P_i(B=b|J)P_i(A|B=b) && \cr \cr
            &= \sum_{b≥0} P(B=b|J)P(A|B=b) &&\text{definition of } P_i \cr \cr
            &= \sum_{b≥0} P_h(B=b)P(A|B=b) &&\text{definition of } P_h \cr \cr
\end{aligned}
$$

Which is $\eqref{3}$.


### Derivation 2

Given this definition for $P_i$

$$
\begin{aligned} 
    P_i(A)    = P(A | B=0 ∨ (B=1 ∧ C≥0) )
\end{aligned}
$$

We can rewrite $P_i(A)$ as:

$$
\begin{aligned} 
    P_i(A)&=  \cr
            &\begin{aligned}
                &= P_i(A|b≥0) ~~~~ &&\text{definition of }P_i\cr \cr
                &= \sum_{b \geq 0} P_i(B=b)P_i(A|B=b)  ~~~~&&\text{law of total prob}\cr \cr
                &= P_i(B=0)P(A|B=0) ~~~~&&\text{definition of }P_i \cr
                &\space\space\space\space+ P_i(B=1)P(A \vert B=1, C ≥ 0)\cr \cr
                &=    P_i(B=0) P(A|B=0)
            \end{aligned} \cr
            &\space\space\space\space\begin{aligned}
                    + P_i(B=1) &\sum_{c≥0} P(C=c \vert B=1) ~~~~&&\text{law of total prob.} \cr
                          &× P(A \vert B=1,C=c) 
            \end{aligned} 
\end{aligned}
$$



Which is $\eqref{4}$.


<!-- NOTES

Concepts from Argument Model:
    joint significance
    argument threads are dialogs
        presume acceptance
    definition of claim
    sample argument graph

-->
