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

How do you measure *uncertainty*?

That may seem like an odd question, but let's just dive right into it, because it leads us down an interesting path to the definition of **entropy**.

### I'm 99% Certain

We can start with one common way people express *certainty*. I might say "I'm 99% certain it will rain today". This, of course, implies that I'm 1% *uncertain*.

On the other hand, if I say "I'm 50% certain it will rain", then I'm 50% uncertain.

So one obvious definition of uncertainty is the opposite of certainty, or $1 - \text{certainty}$.

Of course, how do you define certainty? If you are 99% certain that it will rain, then you are 1% certain it won't rain! So are you certain or uncertain?

You are in fact certain. If you are 99% sure that *anything* will happen, you are pretty certain. If you think there's a 40% chance that it will rain, then you are actually 60% certain it will *not* rain! To handle this, let's measure uncertainty as the probability of the *most probable outcome*. That means certainty can't go lower than 50%, when something is just as likely to happen as not.

So a first crack at a definition of certainty might simply be:

**Definition 1**

$$
  \text{uncertainty} = 1 - p
$$

Where $p$ is the probability of the most probable outcome. The maximum possible value of uncertainty is $0.5$ when $p = 0.5$.

## The Number of Possibilities

Now what if there is more than **one** possibility? Imagine a murder has been committed. We don't know who did it, so there's uncertainty. If there are only **two** people who could have done it (say, Colonel Mustard and Professor Plum) then uncertainty is not very high. With **ten** possible suspects the uncertainty increases. On the other hand if there's only **one** person who could have done it then there's no uncertainty at all.

So another straightforward measure of uncertainty might be **the number of possibilities**. Or to use the conventional terminology of probability theory, the number of **possible outcomes**.

**Definition 2**

$$
  \text{uncertainty} = n
$$

Where $n$ is the number of equally probable outcomes.

## Probability vs. Possible Outcomes

There is a relationship between probability and the number of possible outcomes. If there are $n$ equally-probable outcomes, then the probability of each outcome is $p = \frac{1}{n}$. Conversely, $n = \frac{1}{p}$.

And so, by **Definition 2**, uncertainty becomes:

$$
  \text{uncertainty} = \frac{1}{p}
$$

So we can use $\frac{1}{p}$ for measuring uncertainty in the case of multiple equally-probable outcomes. But this could also be a good measure of uncertainty when there are only **two** (not necessarily equally-probable) possible outcomes. We used $1-p$ for **Definition 1** but really $\frac{1}{p}$ works just as well, because it increases as $p$ decreases and vice versa.

So this means we can come up with a single definition that works both when:

- there are multiple equally-probable outcomes
- there are only two (not necessarily equally-probable) possible outcomes

**Definition 3**

$$
  \text{uncertainty} = \frac{1}{p}
$$

Where $p$ is the probability of the *most probable outcome*.

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
  \text{surprisal} = log\left(\frac{1}{p}\right) = -log(p)
$$

We can use any base, but I will use base 2 for this article.

### Advantages of Logs

There are a couple of benefits to measuring uncertainty on a log scale.

First, because when there is only **one** possible outcome, it seems intuitive that uncertainty should be zero. And sure enough, $log(1) = 0$!

Second, uncertainty should be something we can "add up". For example, going back to our murder mystery, suppose there are two equally-probable murder suspects. Uncertainty, measured as surprisal, is $log(2) = 1$.

And suppose there are four possible murder weapons. That means the uncertainty about the murder weapon is $log(4) = 2$.

To get the total number of **possibilities**, we need to count the number of culprit-weapon **combinations** (Professor **Plum** with the lead pipe, etc). This means we need to multiply:

$$
  n = 2 \times 4 = 8
$$

So total surprisal is $log(8) = 3$.

But we could have gotten that by adding the individual uncertainties:

$$
  log(2) + log(4) = 1 + 2 = 3 = log(8)
$$

So, sticking to log space, uncertainty acts more as a quantity that we can add up.

Finally, a log scale is useful when the number of possible outcomes is very large. Suppose there are 1,000,000 suspects. Multiplying this by the number of possible outcomes for weapons, motives, times of death, etc., can yield trillions of combinations.

If there are a trillion possible outcomes, instead of saying there's "1,000,000,000,000 possible-outcomes-worth" of uncertainty, we say there are $log(10^{12}) \approx 39.86$ bits of uncertainty.

*Bits?* Where did that come from? Bits are a unit of **information**, not uncertainty! But in fact information and uncertainty are closely related and can be measured on the same scale.

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."
>
> -- Claude Shannon

Suppose I know who the murderer is. But you don't -- for you there are still two possibilities. How many bits of information do I need to provide to you to tell you who did it? Just one. I might send you a "1" for Professor Plum and "0" for Colonel Mustard. I need to give you 1 bit of information to resolve your 1 bit of uncertainty about the murderer.

How many bits do I need to tell you who the murder weapon is? We said above there are four possibilities, and a 2-bit number can encode four possibilities. So again, I need to provide 2 bits of information to resolve your 2 bits of uncertainty about the weapon.

"Uncertainty" and "information" are two sides of the same coin.

So every time you receive one bit of information, you can look at it as resolving one bit of uncertainty. For example, suppose I am sending you a byte of information, one bit at a time. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8$ bits. When you find out the value of the first bit, you have cut the number of possible outcomes in half to $2^7 = 128$, which means uncertainty is now $log(128) = 7$ bits. Another 7 more bits eliminates the remaining 7 bits of uncertainty.

## Uncertainty for Unequal Probabilities

If there are more than two possible outcomes, using the surprisal of the most probable outcome is actually a pretty good *approximation* of uncertainty. But it's not quite right. Say the most probable outcome is 60%. If there is only **one** other possible outcome, then there is much less uncertainty than if there were **ten** more possible outcomes.

So what if we used a **weighted average**? Instead of using only the surprisal of the most probable outcome, we took the surprisal of all possible outcomes, weighted by their probability?

## Entropy as Weighted Average Uncertainty

This gives us the following measure of uncertainty:

**Definition 5: Shannon Entropy**

$$
H(X) = \sum_{x \in \text{possible outcomes}} p(x) \cdot \text{surprisal}(x) = \sum_{x \in \text{possible outcomes}} p(x) \cdot log\left(\frac{1}{p(x)}\right)
$$

Tada! This is the definition of **Shannon entropy** (often denoted $H(X)$ for a random variable $X$).

If all $n$ outcomes are equally probable, then $p(x) = \frac{1}{n}$ for all $x$. It's easy to see that this just reduces to $log(n)$:

$$
\begin{aligned}
H(X) 
&= 
\sum_{x ∈ \text{possible outcomes}} x \cdot p(x) \cdot log\left(\frac{1}{p(x)}\right) \cr
&= \sum_{x ∈ \text{possible outcomes}} p(x) \frac{1}{n} \cdot log(n) \cr
&= n \cdot \left( \frac{1}{n} \cdot log(n) \right) \cr
&= log(n)
\end{aligned}
$$

Now we said that the number of bits of uncertainty should be equal to the number of bits required to resolve that uncertainty. And it turns out, this formula gives us exactly that. And in his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon proved that there always exists an efficient **encoding scheme** where communicating the actual outcome requires, on average, a number of bits equal to the entropy.

## Encoding for Unequal Probabilities

To resolve your uncertainty about the actual outcome, I need to communicate the actual outcome to you. To do this we need to come up with some sort of *encoding* scheme beforehand, by assigning each possible outcome to a sequence of bits. Although we can use exactly $log(n)$ bits when there are $n$ equally probable outcomes, when the outcomes aren't equally probable, we'll use a *different number of bits* to encode different outcomes.

Suppose there are three possible outcomes: A (50%), B (25%), and C (25%). I want to communicate the actual outcome to you efficiently. The most efficient encoding scheme is:

-   A: `0`
-   B: `10`
-   C: `11`

If I send `0`, you know it's A. If I send `1`, you will wait for the next bit, which will tell you whether it is B or C.

## Entropy: Expected Value of Surprisal

The expected number of bits to communicate the actual outcome is the **Shannon entropy** for this probability distribution:

$$
\begin{aligned}
    &= 0.50 \cdot (\text{bits for A}) + 0.25 \cdot (\text{bits for B}) + 0.25 \cdot (\text{bits for C}) \cr
    &= H = \sum_{x ∈ \text{possible outcomes}} p(x) \cdot log\left(\frac{1}{p(x)}\right)\cr
    &= 0.50 \cdot log(\frac{1}{0.50}) + 0.25 \cdot log(\frac{1}{0.25}) + 0.25 \cdot log(\frac{1}{0.25}) \cr
    &= 0.50 ⋅ (1) + 0.25 \cdot (2) + 0.25 \cdot (2) \cr
    &= 0.5 + 0.5 + 0.5 \cr
    &= 1.5 \text{ bits}
\end{aligned}
$$


## Conclusion: Entropy as Uncertainty

In the case of equally probable outcomes, the log of the number of possible outcomes is a good measure of uncertainty. This is also equal to the number of bits of information required to communicate the actual outcome.

**Shannon entropy** generalizes this measure when outcomes are not equally probable. When there is uncertainty about an outcome, **Shannon entropy** gives us the expected number of bits required to communicate the actual outcome using an optimal encoding scheme.

Information can thus be seen as the resolution of uncertainty. Entropy is the expected number of bits of information required to resolve all uncertainty.