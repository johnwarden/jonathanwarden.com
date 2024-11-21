---

layout: single  
title: "Entropy as a Measure of Uncertainty"  
toc: true  
toc_sticky: true  
weight: 49  

---

## The Number of Possibilities

What is the most intuitive way to quantify uncertainty?  

Imagine a murder has been committed. We don't know who did it, so there's uncertainty. If there are only two people who could have done it (Colonel Mustard and Professor Plum), the uncertainty is limited. With ten possible suspects, the uncertainty increases. If there's only one person who could have done it, there's no uncertainty.  

Thus, a straightforward measure of uncertainty would be proportional to **the number of possibilities**.  

This works well unless the possibilities aren't equally probable. For example, if there are two suspects but we're 95% sure it's Colonel Mustard, there's less uncertainty. If we are 99.9% sure, the uncertainty is even lower. If we are 100% sure, it's equivalent to having only one possibility -- no uncertainty. Even with ten possibilities, if one is 99.99% likely, uncertainty is minimal.  

So we want a measure of uncertainty that **is zero if there is only one possibility**, **increases with the number of possibilities** but **decreases as probabilities become more extreme**. 

Below, we'll see how these criteria are met in the formula for Shannon entropy.  

## Multiplying Possibilities

Let's start by considering simple scenarios where there are $n$ equally probable possibilities. We'll measure entropy simply as the number of possibilities. If there are two murder suspects, there are two possibilities-worth of uncertainty. 

Now, suppose the murder weapon is unknown, but there are also only two possibilities (the candlestick or the lead pipe). This adds another two possibilities-worth of uncertainty.  

The possibilities aren't additive -- they're multiplicative. There are $2 \times 2 = 4$ possible combinations of suspect and weapon, so the overall uncertainty is four possibilities.  

## Log Scale

Now intuitively, it feels like uncertainty should be something we can "add up". And further, we have already said if there is only 1 possibility, uncertainty should be zero. Uncertainty measured as the log of the number of possibilities meets these criteria.

So the uncertainty about the suspect becomes $log(2) = 1$, and uncertainty about the weapon also becomes $log(2) = 1$. The total uncertainty becomes $log(2) + log(2) = log(4) = 2$.

A log scale is especially useful when the number of possibilities is very large. Suppose there are 1,000,000 suspects. Multiplying this by the number of possibilities for weapons, motives, times of death, etc., can yield trillions of combinations.  

If there are a trillion possibilities, instead of saying there's "1,000,000,000,000 possibilities-worth" of uncertainty, we say there are $log(1,000,000,000,000) = 39.86$ bits of uncertainty.  

*Bits?* Where did that come from? Bits are a unit of **information**, not uncertainty! But in fact information and uncertainty are closely related and can be measured on the same scale.

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."  
> -- Claude Shannon  

Suppose I know who the murderer is, and you've narrowed it down to two suspects. How many bits of information do I need to provide to tell you who did it? Just one. The same applies to the murder weapon. Resolving your two bits of uncertainty requires two bits of information.  "Uncertainty" and "information" are two sides of the same coin. 

So every time you receive a byte of information, you can look at it this way. Initially there are $2^8 = 256$ possible values for that byte. So your uncertainty is $log(256) = 8~bits$. When you find out the value of the first bit, you have cut the number of possibilities in half to $2^7 = 128$. Which means uncertainty is now $log(128) = 7~bits$. So each bit of information resolves 1 bit of uncertainty. 

## Unequal Probabilities

If there are $n$ equally probable possibilities, each has a probability $p = 1/n$, and the uncertainty is:  

$$
log(1/p) = -log(p)
$$

But what if the possibilities aren't equally probable?

If possibility $i$ has probability $p_i$, the uncertainty associated with that possibility is still $-log(p_i)$. 

And it turns out, this is also the number of bits of information I must provide to you to tell you that $i$ is the correct value. For example, if $p_i=.25$, I need $-log(.25) = 4$ bits of information to communicate this value, no matter what the probabilities of the other values are. I need more bits to communicate improbable values, and less bits to communicate probable values. 

But how does this work? We must have some sort of *encoding* scheme, that uses a *different number of bits* to encode different values.

In his seminal 1948 paper, "A Mathematical Theory of Communication," Claude Shannon proved that there always exists an efficient encoding scheme such that, on average, communicating a value with probability $p_i$ requires $-log(p_i)$ bits.

## Variable-Length Encoding

Suppose there are three possible values: A (50%), B (25%), and C (25%). After I find out which is the correct value, I want to communicate it to you efficiently. So we come up with an efficient encoding scheme that minimizes the *expected* number of bits I need to communicate it to you. We do this by assigning fewer bits to more probable possibilities:  

- A: 0  
- B: 10  
- C: 11  

If I send "0," you know it's A. If I send "1," you need another bit to distinguish B from C.

And sure enough, the number of bits required for each value is equal to the negative log of its probability: $-log(0.50) = 1$ bit for A, and $-log(0.25) = 2$ bits for either B or C.

The expected number of bits to communicate the correct value is:  

$$
0.50 \cdot 1 + 0.25 \cdot 2 + 0.25 \cdot 2 = 1.5
$$

Generalizing, this formula is:  

$$
\sum_{x \in \text{possibilities}} p(x) \cdot -log(p(x))
$$

Shannon called this measure **entropy**. It simultaneously measures the number of bits required to specify a value, and the amount of uncertainty we have about that value.

Shannon also proved this holds for any probability distribution.  

For the special case where all possibilities are equally probable ($p = 1/n$), this reduces to:  

$$
\sum \frac{1}{n} \cdot -log\left(\frac{1}{n}\right) = n ⋅ \frac{1}{n} ⋅ log(n) = log(n)
$$

## Conclusion

Shannon entropy generalizes a measure of uncertainty as the log of the number of possibilities. Information corresponds to the bits needed to eliminate uncertainty by specifying which is the correct value.  

In the special case of equal probabilities, entropy simplifies to $log(n)$. For unequal probabilities, Shannon's entropy calculates the average bits needed to identify the correct value using the most efficient encoding scheme.  
