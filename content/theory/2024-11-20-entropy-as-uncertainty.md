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

So every time you receive a byte of information, you can look at it this way. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8~bits$. When you find out the value of the first bit, you have cut the number of possible outcomes in half to $2^7 = 128$. Which means uncertainty is now $log(128) = 7~bits$. So each bit of information resolves 1 bit of uncertainty. 

## Unequal Probabilities

If there are $n$ equally probable outcomes, each has a probability $p = \frac{1}{n}$, and the uncertainty is:  

$$
log(1/p) = -log(p)
$$

But what if the possible outcomes aren't equally probable?

Well it turns out, if possible outcome $i$ has probability $p_i$, we still say that the uncertainty associated with that possible outcome $-log(p_i)$. That's because this is the number of bits of information I must provide to you to tell you that $i$ is the correct value. 

To understand why this is so, I will need to explain the concept of variable-length encoding. But first, let's review review the important concepts introduced so far.

### Key Concepts

- For the specific case when there are $n$ equally probable outcomes:
	- The probability of each outcome is $p = \frac{1}{n}$
	- The number of bits required to communicate the actual outcome is:
		 $$
			\begin{aligned}
			 	&  && log(n) \newline
			 	&= ~&&log(1/p_i) \newline
			 	&= -&&log(p_i)
			\end{aligned}
		 $$
- For the general case where outcomes are not equally probable:
	- The number of bits required to communicate that the actual outcome is possibility $i$ is still:
		$$
			-log(p_i)
		$$


## Variable-Length Encoding

Is a probability gets smaller, its negative log gets bigger. So it takes more bits to communicate improbable values. What exactly does this mean?

If we come up with some sort of *encoding* scheme for me to communicate values to you, we'll need to assign each possible value to a sequence of bits. If there are 256 equally probable values, then we'll use 8 bits to encode each value. But when the values aren't equally probable, we'll use a *different number of bits* to encode different values, and the most **efficient** encoding scheme (the one that minimizes the expected number of bits) will use more bits for less probable values.

Suppose there are three possible values: A (50%), B (25%), and C (25%). I want to communicate the correct value to you efficiently. The most efficient encoding scheme is:  

- A: 0  
- B: 10  
- C: 11  

If I send "0," you know it's A. If I send "1," you will wait for the next bit, which will tell you whether it is B or C.

And sure enough, the number of bits required for each value is equal to the negative log of its probability: $-log(0.50) = 1$ bit for A, and $-log(0.25) = 2$ bits for either B or C.

And in his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon proved that there always exists an efficient encoding where communicating a value with probability $p_i$ requires $-log(p_i)$ bits.

## Definition Entropy

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
