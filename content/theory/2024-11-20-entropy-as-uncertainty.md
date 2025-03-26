---

layout: single  
title: "Entropy as a Measure of Uncertainty"  
toc: true  
toc_sticky: true  
weight: 49  

---


## Measuring Uncertainty

How do you measure *uncertainty*? 

That may seem like an odd question, but let's just dive right into it, because it leads us down an interesting path to the definition of **entropy**.

## The Number of Possibilities

Imagine a murder has been committed. We don't know who did it, so there's uncertainty. If there are only two people who could have done it (say, Colonel Mustard and Professor Plum), the uncertainty is limited. With ten possible suspects, the uncertainty increases. On the other hand if there's only one person who could have done it, there's no uncertainty at all.

Thus, a straightforward measure of uncertainty would be **the number of possibilities**. Or to use the conventional terminology of probability theory, the number of **possible outcomes**.

## The Degree of Probability

This works well unless the possible outcomes aren't equally probable. For example, if there are two suspects but we're 95% sure it's Colonel Mustard, there's little uncertainty. If we are 99.9% sure, there is even less. If we are 100% sure, there's no uncertainty -- which is equivalent to having only one possible outcome. 

But even with ten possible outcomes, if one is 99.99% likely, uncertainty is minimal.  

## Criteria for a Measure of Uncertainty

So we want a measure of uncertainty that:

- is zero if there is only one possible outcome
- increases with the number of possible outcomes
- approaches zero as the probability of one of the possible outcomes approaches 100%

Below, we'll see how these criteria are met in the formula for Shannon entropy.  

## Multiplying Possible Outcomes

Let's start by considering simple scenarios where there are $n$ equally probable outcomes. We'll measure entropy simply as the number of possible outcomes. For example, if there are two murder suspects, there are two possible-outcomes-worth of uncertainty. 

Now, suppose the murder weapon is unknown, but there are also only two possible weapons (the candlestick or the lead pipe). This adds another two possible-outcomes-worth of uncertainty.  

The number of possible outcomes isn't additive -- it's multiplicative. There are $2 \times 2 = 4$ possible combinations of suspect and weapon, so the overall uncertainty is four possible outcomes.  

## Log Scale

Now intuitively, it feels like uncertainty should be something we can "add up". This points to using the log of the number of possible outcomes as a measure of uncertainty.

So the uncertainty about the suspect becomes $log(2) = 1$, and uncertainty about the weapon also becomes $log(2) = 1$. The total uncertainty becomes $log(2) + log(2) = log(4) = 2$.

And we said if there is only one possible outcome, we want our measure of uncertainty should be zero. And sure enough, $log(1) = 0$! 

A log scale is especially useful when the number of possible outcomes is very large. Suppose there are 1,000,000 suspects. Multiplying this by the number of possible outcomes for weapons, motives, times of death, etc., can yield trillions of combinations.  

If there are a trillion possible outcomes, instead of saying there's "1,000,000,000,000 possible-outcomes-worth" of uncertainty, we say there are $log(1,000,000,000,000) = 39.86$ bits of uncertainty.  

*Bits?* Where did that come from? Bits are a unit of **information**, not uncertainty! But in fact information and uncertainty are closely related and can be measured on the same scale.

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."  
> -- Claude Shannon  

Suppose I know who the murderer is, and you've narrowed it down to two suspects. How many bits of information do I need to provide to tell you who did it? Just one. I need to provide you with a second bit of information to tell you what the murder weapon is. Resolving your two bits of uncertainty requires a total of two bits of information from me. "Uncertainty" and "information" are two sides of the same coin. 

So every time you receive a byte of information, you can look at it this way. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8$ bits. When you find out the value of the first bit, you have cut the number of possible outcomes in half to $2^7 = 128$. Which means uncertainty is now $log(128) = 7$ bits. So each bit of information resolves 1 bit of uncertainty. 

### Key Concepts: Equally Probable Outcomes

When there are $n$ equally probable outcomes:

- The probability of each outcome is $p = \frac{1}{n}$
- The number of bits required to communicate the actual outcome is:
	 $$
		\begin{aligned}
		 	&  && log(n) \newline
		 	&= ~&&log(\frac{1}{p_i}) \newline
		 	&= -&&log(p_i)
		\end{aligned}
	 $$

And entropy can be thought of as both a *measure of uncertainty* about an outcome, and the number of bits of information required to resolve all uncertainty. They are the same thing.

## Uncertainty for Unequal Probabilities

Now let's go back to the question of how to measure uncertainty when outcomes are not equally probable.

Previously we said that our measure of uncertainty should approach zero as the probability of one of the possible outcomes approaches 100%. So we could just use $1 - p_i$, where $p_i$ is the most probable outcome.

But suppose there are two possible outcomes, each with a 50% probability. We already said that uncertainty in this case is $log(2) = -log(.5)$. 

So why not use $-log(p_i)$, where $p_i$ is the most probable outcome. This checks two boxes: it is equal to $log(2)=1$ when $p_i$ equals 50%, and approaches zero as $p_i$ approaches 100%. 

But if there are more than two possible outcomes, just looking at the most probable outcome ignores some information. What about using a weighted average over all possible outcomes?

## Entropu as 

But it turns out that the *probability-weighted average* of $-log(p_i)$ *still* approach zero if any of the probabilities approaches 100%. 

This gives us the following measure of uncertainty:

$$
\sum_{x \in \text{possible outcomes}} p(x) \cdot -log(p(x))
$$

This metric is called **entropy**. And it's actually a generalization of the formula for the case of $n$ equally probable outcomes ($p_i = \frac{1}{n}$ for all $i$):

$$
\sum \frac{1}{n} \cdot -log\left(\frac{1}{n}\right) = n ⋅ \frac{1}{n} ⋅ log(n) = log(n)
$$

## Encoding for Unequal Probabilities

And it turns out, this generalized formula also gives us the number of bits required to communicate the actual outcome even when outcomes are not all equally probable.

To communicate the actual outcome to you, we need to come up with some sort of *encoding* scheme beforehand. To do this we assign each possible value to a sequence of bits. Although we can use exactly log(n) bits when there are n equally probable, when the values aren't equally probable, we'll use a *different number of bits* to encode each value values.

Suppose there are three possible values: A (50%), B (25%), and C (25%). I want to communicate the correct value to you efficiently. The most efficient encoding scheme is:  

- A: 0  
- B: 10  
- C: 11  

If I send "0," you know it's A. If I send "1," you will wait for the next bit, which will tell you whether it is B or C.

We'll find that the number of bits required for each value is equal to the negative log of its probability: $-log(0.50) = 1$ bit for A, and $-log(0.25) = 2$ bits for either B or C.

And in his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon proved that there always exists an efficient encoding where communicating a value with probability $p_i$ requires $-log(p_i)$ bits.

## Entropy: Expected Value

In the example encoding scheme above, the expected number of bits to communicate the correct value is:  

$$
0.50 \cdot 1 + 0.25 \cdot 2 + 0.25 \cdot 2 = 1.5
$$

Generalizing, this formula is:  

$$
\sum_{x \in \text{possible outcomes}} p(x) \cdot -log(p(x))
$$

Shannon called this measure **entropy**. For the special case where all possible outcomes are equally probable ($p = \frac{1}{n}$), this reduces to:  

$$
\sum \frac{1}{n} \cdot -log\left(\frac{1}{n}\right) = n ⋅ \frac{1}{n} ⋅ log(n) = log(n)
$$

## Conclusion: Entropy as Uncertainty

So near the beginning of this essay, we argued that in the case of equally probable outcomes, the log of the number of possible outcomes is a good measure of uncertainty. Then we showed that this is also equal to the number of bits of information required to communicate the correct outcome. 

Shannon Entropy generalizes this measure when outcomes are not equally probable. It tells us the expected number of bits required to communicate the correct outcome using efficient coding. But it also has all the properties we want from a good measure of uncertainty:

- is zero if there is only one possible outcome
- increases with the number of possible outcomes
- approaches zero as the probability of one of the possible outcomes approaches 100%

And finally these two different ways of looking at entropy: uncertainty, and the expected number of bits required to communicate the correct value, are tied together by the idea of *information as the resolution of uncertainty*.
