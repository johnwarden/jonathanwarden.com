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

How do you measure "uncertainty"?

That may seem like an odd question. But let's just dive right into it, because starting down this path of inquiry will lead us step by step to the definition of the fascinating concept of **Shannon entropy**.

### "I'm 99% Certain"

We can start with one common way people express *certainty*. You might say "I'm 99% certain it will rain today". This, of course, implies that you're 1% *uncertain*. So one obvious definition of uncertainty is the inverse of certainty, or $1 - p$, where $p$ is certainty expressed as a percentage.

But if you are 99% certain that it will rain, then you are 1% certain that it won't rain! You are certain about one thing, and uncertain about it's opposite. So are you certain, or uncertain? 

A better definition of uncertainty would make sense no matter how you frame it. We can get this by always taking the *most probable* outcome to represent certainty. So the belief that there's a 1% chance of rain implies a belief that there's a 99% chance of no rain. Taking the larger probability, we'll say this implies 99% certainty.

So one sensible definition of uncertainty is:

**Definition 1**

$$
  \text{uncertainty} = 1 - p
$$

Where $p$ is the probability of the most probable outcome.

<!--
Although $1 - p$ (of the most probable outcome) is a sensible definition of uncertainty, here's another interesting definition:

**Definition 1**

$$
  \text{uncertainty} = \frac{1}{p}
$$

So if you think there's a 99% chance that it (will/won't) rain, uncertainty would be $\frac{1}{.99} ≈ 1.01$.

So this measure approaches 1.0 from above as uncertainty decreases, and increases as uncertainty increases.


So there's one possible measure of uncertainty. But there are others.
-->

## The Number of Possibilities

Definition 1 works pretty well when there are only two possibilities (e.g. something will happen or not). But what if there are more than two possibilities? 

Imagine a murder has been committed, and there are **two** equally likely suspects (say, Colonel Mustard and Professor Plum). So we are uncertain. But if there are **ten** equally likely suspects, we are even more uncertain. On the other hand if there's only **one** person who could have done it then there's no uncertainty at all.

So another straightforward measure of uncertainty might be **the number of possibilities**. Or to use the conventional terminology of probability theory, the number of **possible outcomes**.

**Definition 2**

$$
  \text{uncertainty} = n
$$

Where $n$ is the number of possible outcomes.

## Probability vs. Possible Outcomes

There is a relationship between probability and the number of possible outcomes. If there are $n$ equally-probable outcomes, then the probability of each outcome is $p = \frac{1}{n}$. Conversely, $n = \frac{1}{p}$.

So Definition 2 can be rewritten:

$$
  \text{uncertainty} = n = \frac{1}{p}
$$

So Definition 1 and Definition 2 are both defined in terms of $p$. And in both cases, $p$ is the probability of the *most probable outcome* (if all outcomes are equally probable, then they are all the most probable)!

So this definition makes sense both when:

- there are multiple equally-probable outcomes
- there are only two (not necessarily equally-probable) possible outcomes

So for example:
- If there is a 50% chance of rain, then uncertainty is $\frac{1}{0.5} = 2$ (the same as the uncertainty of two equally-probable outcomes).
- If there is a 99% chance of rain, then uncertainty is $\frac{1}{0.99} \approx 1.01$. 
- If there are 1000 equally-probable outcomes, uncertainty is $\frac{1}{(1/1000)} = 1000$.

This measure approaches 1.0 as uncertainty disappears (e.g. the number of outcomes reduces to 1, or the probability of one outcome approaches 100%), but can be arbitrarily large.

## Almost There

Now we are actually very close to the actual definition of Shannon entropy. We just have two more steps:

1. convert it to a log scale.
2. generalize for cases where there are more than two possible outcomes that are not equally probable.

## Surprisal: Log-Scale Uncertainty

If we take the log of our metric, we get something known in information theory as **surprisal**.

**Definition 3: Surprisal**

$$
  \text{surprisal} = \log\left(\frac{1}{p}\right) = -\log(p)
$$

We can use any base, but software engineers generally prefer base 2.

### Advantages of Logs

There are a couple of benefits to measuring uncertainty on a log scale.

First, because when there is only **one** possible outcome, it seems intuitive that uncertainty should be zero. And sure enough, $log(1) = 0$!

Second, working with logs allows you to take *sums* of uncertainties. For example, going back to our murder mystery, suppose there are two equally-probable murder suspects. Uncertainty about the culprit, measured as surprisal, is $log(2) = 1$. And suppose there are four possible murder weapons. That means the uncertainty about the murder weapon is $log(4) = 2$. Adding these up, we get uncertainty $1 + 2 = 3$.

We could have gotten to the same result by counting the total number of **possibilities** -- the number of culprit-weapon **combinations** (Professor Plum with the lead pipe, etc), by multiplying 2 (suspects) by 4 (weapons) = 8 (possibilities). Then surprisal is $log(8) = 3$.

But simply adding uncertainties was easier.

A log scale is especially useful when the number of possible outcomes is very large. Suppose there are 1,000,000 suspects. Multiplying this by the number of possible outcomes for weapons, motives, times of death, etc., can yield trillions of combinations.

If there are a trillion possible outcomes, instead of saying there's "1,000,000,000,000 possible-outcomes-worth" of uncertainty, we say uncertainty is $log(10^{12}) \approx 39.86$.

## Uncertainty for Unequal Probabilities

Okay, our final step is to deal with situations where there are multiple possible outcomes, but they are not all equally probable.

We can't just use surprisal ($\log\left(\frac{1}{p}\right)$), because there are multiple values for $p$.

Using the surprisal of the most probable outcome doesn't quite work either. Say the most probable outcome is 50%. Surprisal would be ($\log\left(\frac{1}{.5}\right) = 2$), *no matter how many other possible outcomes there are*. But uncertainty should increase with the number of possible outcomes. 

So what if we used a **weighted average**? We could weigh the surprisal of each possible outcome by its probability. So our uncertainty measure would be influenced largely be the surprisal of the most probable outcome -- which is good -- but the surprisal of the remaining outcomes would still contribute.

## Entropy as Weighted Average Surprisal

This gives us the following measure of uncertainty, which -- tada! -- is exactly the definition of Shannon entropy.

**Definition 4: Shannon Entropy**

$$
\begin{aligned}
H(X) &= \sum_{x} p(x) \cdot \text{surprisal}(x) \cr
     &= \sum_{x} p(x) \cdot \log(\frac{1}{p(x)})
\end{aligned}
$$

The formula for Shannon entropy can also be understood as the formula for the **expected value of surprisal**.

$$
\begin{aligned}
H(X) &= \sum_{x} p(x) \cdot \text{surprisal}(x) \cr
     &= \sum_{x} p(x) \cdot \log(\frac{1}{p(x)}) \cr
     &= 𝐸 \log(\frac{1}{p(X)})
\end{aligned}
$$


### Properties of Shannon Entropy

Here's a chart showing Shannon entropy in the case of 2 possible outcomes. It shows entropy as a function of the probability of one of the outcomes.

![chart of Shannon entropy as a function of p(X)](https://upload.wikimedia.org/wikipedia/commons/thumb/2/22/Binary_entropy_plot.svg/1920px-Binary_entropy_plot.svg.png)

Shannon entropy has some desirable properties that we've previously discussed.

**1: It approaches zero as the probability of the most probable outcome approaches 1**

For example, in the case of a 99% chance of rain, Shannon entropy is:

$$
\begin{aligned}
     &= .99 \cdot \log(\frac{1}{.99})
     &+ .01 \cdot \log(\frac{1}{.01})
     &= 0.081
\end{aligned}
$$

Which is pretty close to zero because there is not a lot of uncertainty.

What's more, it's easy to see that the entropy of 1% chance of rain will be the same as the entropy of 99% chance of rain! We don't have to treat the most probable outcome as something special.

**2: it is maximized when each outcome is equally probable**

In the case of 2 possible outcomes, it is maximized when the probability of each outcome is 50%. 

In the case of more than 2 outcomes, Shannon entropy is also maximized when they are all equally probable.

**3: it increases as the number of possible outcomes increases**

If all $n$ outcomes are equally probable, then $p(x) = \frac{1}{n}$ for all $x$. In this case Shannon entropy is just equal to the surprisal: $\log(\frac{1}{p(x)}) = log(n)$, which increases with the number of possibilities.

<!--

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
-->

<!--

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

-->

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."
>
> -- Claude Shannon

So now we have a nice way to actually quantify uncertainty that ticks a lot of intuitive boxes. But Shannon entropy is also a measure of **information**. What is the relationship between uncertainty and information?

Suppose I know who the murderer is. But you don't -- for you there are still two possibilities. How many bits of information do I need to provide to you to tell you who did it? Just one. I might send you a "1" for Professor Plum and "0" for Colonel Mustard for example. I need to give you 1 bit of information to resolve your 1 bit of uncertainty about the murderer.

How many bits do I need to tell you who the murder weapon is? We said above there are 4 possible weapons, and a 2-bit number can encode four possibilities. So I need to provide 2 bits of information to resolve your 2 bits of uncertainty about the weapon.

So "uncertainty" and "information" are two sides of the same coin. Every time you receive one bit of information, you can look at it as resolving one bit of uncertainty. For example, suppose I am sending you a byte of information, one bit at a time. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8$ bits. When you find out the value of the first bit, you have cut the number of possible outcomes in half to $2^7 = 128$, which means uncertainty is now $log(128) = 7$ bits. Each bit of information reduces uncertainty by 1 bit.

## Efficient Encoding

Now let's suppose the 256 possible values are *not* equally-probable: 99% of the time, the value was zero. The remaining values are all equally probable. I could devise an encoding schema that, on average, took much less than 8 bits. For example, if the value was zero, I could send a zero, and if not, I could send you a 1 followed by the value. Sometimes it would take 1 bit to communicate the value, sometimes it would take 9, but the average would be closer to 1.

In his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon probed that, no matter what the probabilities are, it is possible to devise an encoding scheme such that, on average, the number of bits required to communicate a value is equal to the entropy of the associated probability distribution.


## Conclusion: Entropy as Uncertainty

So entropy can be understood as a measure of uncertainty. It is simply a weighted average of the negative log of the probabilities of each possible outcome. If they are all equally probable, it will just be equal to the log of the number of possible outcomes. And as the probability of any one outcome approaches 100%, entropy approaches zero. 

Entropy is measured in bits. Information is also measured in bits. And the number of bits of *information* required to resolve all uncertainty is equal to the number of bits of entropy.
