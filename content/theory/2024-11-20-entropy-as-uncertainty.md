---

layout: single
title: "Entropy as a Measure of Uncertainty"
slug: entropy-as-uncertainty
toc: true
toc_sticky: true
weight: 49
aliases:
- /entropy-as-a-measure-of-uncertainty/

---

## Measuring Uncertainty

How do you measure **uncertainty**?

That may seem like an odd question. But let's just dive right into it, because it leads us down an interesting path to the definition of **entropy**.

### I'm 99% Certain

We can start with one common way people express *certainty*. You might say "I'm 99% certain it will rain today". This, of course, implies that you're 1% *uncertain*. Or if you say "I'm 50% certain it will rain", then you're 50% uncertain.

So one obvious definition of uncertainty is the inverse of certainty, or $1 - \text{certainty}$.

Of course, how do you define certainty? If you are 99% certain that it will rain, then you are 1% certain it won't rain! So is there a lot of uncertainty, or a lot of certainty?

In this case, it makes more sense to say there is little uncertainty. If you are 99% sure what the outcome will be, you have a lot of certainty. It's only when you think it's just as likely to rain as not that there is a lot of uncertainty.

So if there are just two possibilities, it makes sense to measure uncertainty based on the one that is *most probable*. Any definition that decreases as the probability of the most probable outcome increases will do. As we'll see shortly, the inverse probability works nicely.

**Definition 1**

$$
  \text{uncertainty} = \frac{1}{p}
$$

Where $p$ is the probability of the most probable outcome.

## The Number of Possibilities

Now what if there is more than **one** possibility? Imagine a murder has been committed. We don't know who did it, so there's uncertainty. If there are only **two** people who could have done it (say, Colonel Mustard and Professor Plum) then it seems there's little uncertainty. With **ten** possible suspects uncertainty increases. On the other hand if there's only **one** person who could have done it then there's no uncertainty at all.

So another straightforward measure of uncertainty might be **the number of possibilities**. Or to use the conventional terminology of probability theory, the number of **possible outcomes**.

**Definition 2**

$$
  \text{uncertainty} = n
$$

Where $n$ is the number of (equally probable) possible outcomes.

## Probability vs. Possible Outcomes

Okay we have two possible definitions of uncertainty so far. Definition 1 is based on the probability when there are two possible outcomes, and Definition 2 is based on the number of (equally probably) possible outcomes.

There is a relationship between probability and the number of possible outcomes. If there are $n$ equally-probable outcomes, then the probability of each outcome is $p = \frac{1}{n}$. Conversely, $n = \frac{1}{p}$.

Substituting this into Definition 2, we get:

$$
  \text{uncertainty} = \frac{1}{p}
$$

Which is the same as **Definition 1**!

So we now have a definition of uncertainty that makes sense both when:

- there are multiple equally-probable outcomes
- there are only two (not necessarily equally-probable) possible outcomes


### Examples:

- 2 equally-probable outcomes: $p = \frac{1}{n} = \frac{1}{2}$, and uncertainty is $\frac{1}{(1/2)} = 2$.
- 1000 equally-probable outcomes. $p = \frac{1}{n} = \frac{1}{1000}$, and uncertainty is $\frac{1}{(1/1000)} = 1000$.
- 75% chance of rain: uncertainty $\frac{1}{0.75} \approx 1.33$.
- 99% chance of rain: uncertainty $\frac{1}{0.99} \approx 1.01$.

So the uncertainty associated with a 99% chance of rain is equal to the uncertainty associated with only $n \approx 1.01$ possible outcomes. The uncertainty associated with **a** 50% chance of rain is equal to that associated with $n = 2$ equally-probable outcomes. And the uncertainty associated with a 75% chance of rain is equal to that associated with $n \approx 1.33$ possible outcomes.

Now we are actually very close to the actual definition of Shannon entropy. We just have two more steps:

1. convert it to a log scale.
2. generalize for cases where there are more than two possible outcomes that are not equally probable.

## Surprisal: Log-Scale Uncertainty

If we take the log of our metric, we get something known in information theory as **surprisal**.

**Definition 4: Surprisal**

$$
  \text{surprisal} = \log\left(\frac{1}{p}\right) = -\log(p)
$$

We can use any base, but I will use base 2 for this article.

### Advantages of Logs

There are a couple of benefits to measuring uncertainty on a log scale.

First, because when there is only **one** possible outcome, it seems intuitive that uncertainty should be zero. And sure enough, $log(1) = 0$!

Second, working with logs can make the math easier to deal with. For example, going back to our murder mystery, suppose there are two equally-probable murder suspects. Uncertainty, measured as surprisal, is $log(2) = 1$. And suppose there are four possible murder weapons. That means the uncertainty about the murder weapon is $log(4) = 2$.

To get the total number of **possibilities**, we need to count the number of culprit-weapon **combinations** (Professor Plum with the lead pipe, etc). This means we need to multiply:

$$
  n = 2 \times 4 = 8
$$

So total surprisal is $log(8) = 3$.

But we could have gotten that by adding the individual uncertainties:

$$
  log(2) + log(4) = 1 + 2 = 3 = log(8)
$$

So, sticking to log space, uncertainty acts more as a quantity that we can add up.

A log scale is especially useful when the number of possible outcomes is very large. Suppose there are 1,000,000 suspects. Multiplying this by the number of possible outcomes for weapons, motives, times of death, etc., can yield trillions of combinations.

If there are a trillion possible outcomes, instead of saying there's "1,000,000,000,000 possible-outcomes-worth" of uncertainty, we say uncertainty is $log(10^{12}) \approx 39.86$.

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."
>
> -- Claude Shannon

Okay, so now we are using **surprisal** to measure uncertainty. We are closing in on Shannon entropy. But before we take the next step, I want to briefly explain the relationship between *uncertainty* and *information*.

Suppose I know who the murderer is. But you don't -- for you there are still two possibilities. How many bits of information do I need to provide to you to tell you who did it? Just one. I might send you a "1" for Professor Plum and "0" for Colonel Mustard for example. I need to give you 1 bit of information to resolve your 1 bit of uncertainty about the murderer.

How many bits do I need to tell you who the murder weapon is? We said above there are four possibilities, and a 2-bit number can encode four possibilities. So I need to provide 2 bits of information to resolve your 2 bits of uncertainty about the weapon.

So "uncertainty" and "information" are two sides of the same coin. Every time you receive one bit of information, you can look at it as resolving one bit of uncertainty. For example, suppose I am sending you a byte of information, one bit at a time. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8$ bits. When you find out the value of the first bit, you have cut the number of possible outcomes in half to $2^7 = 128$, which means uncertainty is now $log(128) = 7$ bits. Each bit of information reduces uncertainty by 1 bit.

## Uncertainty for Unequal Probabilities

Okay, our final step is to deal with situations where there are multiple possible outcomes, but they are not equally probable.

We can't just use surprisal ($\log\left(\frac{1}{p(x)}\right)$), because there are multiple values for p.

Using the surprisal of the most probable outcome doesn't quite work either. Say the most probable outcome is 60%. If there is only **one** other possible outcome, then there is much less uncertainty than if there were **ten** more possible outcomes.

So what if we used a **weighted average**? Instead of using only the surprisal of the most probable outcome, we took the surprisal of all possible outcomes, weighted by their probability?

## Entropy as Weighted Average Uncertainty

This gives us the following measure of uncertainty, which -- tada! -- is exactly the definition of Shannon entropy.

**Definition 5: Shannon Entropy**

$$
\begin{aligned}
H(X) &= \sum_{x} p(x) \cdot \text{surprisal}(x) \cr
     &= \sum_{x} p(x) \cdot \log(\frac{1}{p(x)})
\end{aligned}
$$

If all $n$ outcomes are equally probable, then $p(x) = \frac{1}{n}$ for all $x$, which case Shannon entropy is equal to surprisal ($\log(\frac{1}{p(x)})$):

$$
\begin{aligned}
H(X) 
&= \sum_{x} p(x) \cdot \log(\frac{1}{p(x)}) \cr
&= \sum_{x} \frac{1}{n} \cdot \log(n) \cr
&= n \cdot \left( \frac{1}{n} \cdot \log(n) \right) \cr
&= \log(n) \cr
&= \log(\frac{1}{p(x)})
\end{aligned}
$$


## Encoding for Unequal Probabilities

Now we said earlier that the number of bits of uncertainty should be equal to the number of bits required to resolve that uncertainty. And it turns out, the entropy formula tells us exactly that. And in his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon proved that there always exists an efficient **encoding scheme** where communicating the actual outcome requires, on average, a number of bits equal to the entropy.

To resolve your uncertainty about the actual outcome, I need to communicate the actual outcome to you. To do this we need to come up with some sort of encoding scheme beforehand, by assigning each possible outcome to a sequence of bits. Although we can use exactly $\log(n)$ bits when there are $n$ equally probable outcomes, when the outcomes aren't equally probable, we'll use a *different number of bits* to encode different outcomes.

Suppose there are three possible outcomes: A (50%), B (25%), and C (25%). I want to communicate the actual outcome to you efficiently. The most efficient encoding scheme is:

-   A: `0`
-   B: `10`
-   C: `11`

If I send `0`, you know it's A. If I send `1`, you will wait for the next bit, which will tell you whether it is B or C.

## Entropy: Expected Value of Surprisal

The expected number of bits to communicate the actual outcome is

$$
\begin{aligned}
    &= 0.50 \cdot (\text{bits for A}) + 0.25 \cdot (\text{bits for B}) + 0.25 \cdot (\text{bits for C}) \cr
    &= \sum_{x} p(x) \cdot \log(\frac{1}{p(x)}) \cr
\end{aligned}
$$

Which is the definition of Shannon entropy. So Shannon entropy can be understood as the **expected value of surprisal**.



## Conclusion: Entropy as Uncertainty

In the case of equally probable outcomes, the log of the number of possible outcomes is a good measure of uncertainty. This is also equal to the number of bits of information required to communicate the actual outcome.

**Shannon entropy** generalizes this measure when outcomes are not equally probable. When there is uncertainty about an outcome, **Shannon entropy** gives us the expected number of bits required to communicate the actual outcome using an optimal encoding scheme.

Information can thus be seen as the resolution of uncertainty. Entropy is the expected number of bits of information required to resolve all uncertainty.