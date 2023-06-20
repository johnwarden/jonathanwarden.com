---
layout: single
title:  "Distributed Bayesian Reasoning - Bayesian Averaging"
slug: distributed-bayesian-reasoning-bayesian-averaging
date:   2021-11-22 00:00:00 +0200
toc: true
weight: 112
series: ['Distributed Bayesian Reasoning']

---

This article is part of the series on distributed Bayesian reasoning. It assumes you have read the previous article on the [Basic Math](/distributed-bayesian-reasoning-math).

<!--more--> 

## The Problem: Small Samples

In [Basic Math](/distributed-bayesian-reasoning-math), we used the informed probability $ P_i $ as the prior beliefs of the meta-reasoner. We calculated this as the ratio:

$$
    P_i(𝐴=1|𝐵=1) = \frac{P(𝐴=1,𝐵=1|B≥0)}{P(𝐵=1|B ≥ 0)} = \frac{c(𝐴=1,𝐵=1)}{c(𝐵=1) }
$$

But this is actually a little Naive. Suppose only one person actually voted on both 𝐴 and 𝐵 and accepts both. Then $𝑐(𝐴=1, 𝐵=1) = 𝑐(𝐵=1) = 1$, and this ratio is 100%. Is this really a good estimate of the probability that the meta-reasoner would accept 𝐴 given they accepted 𝐵?

Certainly not. A single vote from a single user is not a great deal of information. We need a more sophisticated way of estimating the priors of the meta-reasoner based on the evidence we have in the form of arguments and votes. 

The Bayesian approach to answering this question requires us to have **priors**: we actually need to **start** with an estimate of this probability -- or rather, a distribution of possible probabilities -- even before we have any data! Then we can use the rules of Bayesian belief updating to combine our priors with our data to come up with a posterior belief.

## The Beta-Bernoulli Model

It turns out, we are actually dealing with a textbook example of a problem that can be solved with a simple Bayesian hierarchical model. The solution, using a beta-Bernoulli distribution, is amply described elsewhere (I learned about them from [this book](https://www.amazon.com/Doing-Bayesian-Data-Analysis-Tutorial/dp/0124058884)). Here is the solution:

Let:

- ω = our **prior** estimate of the probability that the average juror accepts 𝐴 before getting any vote data
- κ = our **prior** estimate of the concentration of likely values around ω (high κ means low variance)
- 𝑁 = $c(A >= 0)$ = the number of users who have voted on 𝐴
- z = $c(A=1)$ = the number of those users who also agree with 𝐴

Then our **posterior** estimate of the probability that the average user accepts 𝐴 is given they have voted on it is:

$$
\label{0}
\begin{aligned}
    \frac{ω(κ - 2) + 1 + z}{κ + N}
\end{aligned} \tag{0}
$$

What should we use as our prior ω? That depends on the context. If this method is being implemented in a social platform, then this can be based on historical data. For example if in the past, the average accept/reject ratio for arguments submitted to the platform was 80%, then having nothing else to go on, 80% is a good estimate of ω. Our estimate of κ can also be made using historical data.

What we have done here is sometimes called [Bayesian Averaging](https://en.wikipedia.org/wiki/Bayesian_average#:~:text=A%20Bayesian%20average%20is%20a,available%20data%20set%20is%20small.). The above formula essentially gives us a weighted average of our prior ω and the observed ratio z/𝑁, with our data z/𝑁 getting higher weight the larger the value of N relative to κ.



## The Bayesian-Average Probability Function

When calculating values of 𝑃 up to this point, we have just taking ratios of counts from our votes table (the 𝑐 function). For example, the formula for 𝑃(𝐴=a) is just:

$$
    P(𝐴=a) = \frac{c(𝐴=a)}{c()}
$$

Where c() is the total number of voters. To use a Bayesian approach to estimating probabilities, instead of taking a ratio, we plug these same two counts into $\eqref{0}$.

Let's define a new function 𝑃ᵥ that does this for us.

So where, by definition

$$
    P(α) = \frac{c(α)}{c()}
$$

We have instead:

$$
    P_v(α) = \frac{ω(κ - 2) + 1 + c(α)}{κ + c()}
\tag{1}\label{1}
$$

And where by definition of conditional probability:

$$
    P(α|β) = \frac{c(α,β)}{c(β)}
$$

We have instead

$$
    P_v(α |\vert β) = \frac{ω(κ - 2) + 1 + c(α,β)}{κ + c(β)}
\tag{2}\label{2}
$$    

Now let's compute an actual value of 𝑃ᵥ(𝐴=1). First, we need to choose priors. Let's suppose that historically, on average 80% voters accept root claims initially. So ω=80%. And let's suppose the variation in this distribution can be represented by κ=10. So

$$
\begin{aligned}
    P_v(A=1) &= \frac{ω(κ - 2) + 1 + c(A=1)}{κ + c()}\cr
             &= \frac{(80\\%)(10-2) + 1 + 500}{10 + 1000} ≊ 50.23\\%
\end{aligned}
$$



In this case, the large amount of votes overwhelms our relatively weak prior, and so our result is very close to $𝑃ᵢ(𝐴=a) = 50\\%$.

## Two-Level Bayesian Averaging

Reviewing where we are going with this, recall from the [Basic Math](/distributed-bayesian-reasoning-math) article that the justified opinion formula in the case of an argument tree with a single premise argument is:


$$
\label{3}
P_h(𝐴=1) = \sum_{b=0}^{1} P_i(𝐴=1|𝐵=b)P_h(𝐵=b)
\tag{3}
$$

Now we are saying that $ 𝑃ᵢ(𝐴=1 \vert 𝐵=b) $ may not be a good estimate that the average person would accept/reject A given they accepted/rejected B. So instead, we want to use Bayesian averaging and use the formula for $ 𝑃ᵥ(A=1 \vert B=b)$ in place of $𝑃ᵢ(𝐴=1 \vert 𝐵=b)$. So substituting $\eqref{2}$ into $\eqref{3}$

$$
\label{4}
P_h(𝐴=1) = \sum_{b=0}^{1} \frac{ω(κ - 2) + 1 + c(A=a,B=b)}{κ + c(B=b)}P_h(𝐵=b)
\tag{4}
$$


But what are our priors ω and κ? 

Recall that we have just used Bayesian averaging to estimate of the probability that the average person accepts 𝐴: $𝑃ᵥ(𝐴=1)=50.23\\%$. This seems like an reasonable prior for our estimate of $𝑃ᵥ(𝐴=1 \vert 𝐵=b)$. Before considering the 150 users who voted on 𝐵, we have a large amount of data telling us the average user has a roughly even chance of accepting 𝐴, and we have no prior reason to believe that accepting/rejecting 𝐵 either increases or decreases this probability. Unless we have strong evidence showing accepting/rejecting 𝐵 changes the probability that people accept/reject 𝐴, we should assume it doesn't. 

However if we use $𝑃ᵥ(𝐴=1)$ as a prior for $𝑃ᵥ(𝐴=1 \vert 𝐵=b)$, there is a subtle problem: we will be "double counting". We are counting votes of users for whom 𝐴=1 and 𝐵=b as evidence for estimating $𝑃ᵥ(𝐴=1)$, and then counting the same votes as evidence for estimating $𝑃ᵥ(𝐴=1 \vert 𝐵=b)$. So to avoid double counting, our prior should actually be $𝑃ᵥ(𝐴=1 \vert 𝐵≠b)$.

The priors for $𝑃ᵥ(𝐴=1 \vert 𝐵≠b)$, on the other hand, can be the same priors we used to calculate $𝑃ᵥ(𝐴=1)$, because we don't have anything to go on besides historical data. So let's ω=80% and κ=10. Then let's start with 𝐵=1, and calculate:

$$
\begin{aligned}
    P_v(A=1|B≠1) &= \frac{ω(κ - 2) + 1 + c(A=1,B≠1)}{κ + c(B≠1)}\cr
                 &= \frac{80\\%(10 - 2) + 1 + 420}{10 + 900} ≈ 46.96%
\end{aligned}
$$

Now we can set $ω=𝑃ᵥ(𝐴=1 \vert 𝐵≠1)$ as the prior for calculating $𝑃ᵥ(𝐴=1 \vert 𝐵=1)$.

What is our prior estimate of κ? We might think that it should be proportional to the number of people who voted on 𝐴, but this is mistaken. A large number of votes on 𝐴 provide strong evidence for estimating ω = 𝑃ᵥ(𝐴=a). But our estimate for κ is based on our prior expectations about **the degree to which people are influenced by arguments.** This information can come from observation of actual variance in the case of past arguments. If this is historical very high, then κ should be low, and vice versa. 

For simplicity, let's use the same prior κ=10 that we used before. 

We can now finally calculate:

$$
\begin{aligned}
    P_v(𝐴=1|B=1)    &= \frac{𝑃ᵥ(𝐴=1|B≠1)(κ - 2) + 1 + c(A=1,B=1)}{κ + c(B=1)}\cr
                    &≈ \frac{(46.96\\%)(10 - 2) + 1 + 80}{10 + 100} ≈ 77.05\\%
\end{aligned}
$$


This is slightly lower than $𝑃ᵢ(𝐴=1 \vert 𝐵=1) = 80%$. This is because we still have a reasonably large number of votes on 𝐵, and these votes provide strong evidence for a posterior value of 80% that overpower the prior estimate.

Clearly, we can extend this reasoning to long argument threads, though we will not do this here.


## Further Development

This document is a work in progress -- these models have not been fully developed. In fact, we are looking for collaborators. If you are an expert in Bayesian hierarchical models and causal inference, please contact collaborations@deliberati.io.

