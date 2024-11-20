---

layout: single  
title: "Entropy as Uncertainty"  
toc: true  
toc_sticky: true  
weight: 49  

---

## Uncertainty as the Number of Possibilities

What is the most intuitive way to quantify uncertainty?  

Imagine a murder has been committed. We don't know who did it, so there's uncertainty. If there are only two people who could have done it (Colonel Mustard and Professor Plum), the uncertainty is limited. With ten possible suspects, the uncertainty increases. If there's only one person who could have done it, there's no uncertainty.  

Thus, a straightforward way to quantify uncertainty is **the number of possibilities**.  

This works well unless the possibilities aren't equally probable. For example, if there are two suspects but we're 95% sure it's Colonel Mustard, there's less uncertainty. If we are 99.9% sure, the uncertainty is even lower. If we are 100% sure, it's equivalent to having only one possibility -- no uncertainty. Even with ten possibilities, if one is 99.99% likely, uncertainty is minimal.  

In general, **uncertainty increases with the number of possibilities** but **decreases as probabilities become more extreme**. Below, we'll see how these ideas are formalized in Shannon entropy.  

## Multiplying Possibilities

Consider scenarios with $n$ equally probable possibilities. If there are two suspects, there are two possibilities-worth of uncertainty.  

Now, suppose the murder weapon is unknown, with two possibilities (the candlestick or the lead pipe). This adds another two possibilities-worth of uncertainty.  

The total uncertainty isn't additive—it's multiplicative. There are $2 \times 2 = 4$ possible combinations of suspect and weapon, so the overall uncertainty is four possibilities.  

## Log Scale

To make uncertainty additive, we use a logarithmic scale. The uncertainty about the suspect is $log(2) = 1$, and for the weapon, it's also $log(2) = 1$. The total uncertainty is $log(4)$, which equals $1 + 1$.  

A log scale is especially useful when the number of possibilities is large. Suppose there are 1,000,000 suspects. Multiplying this by possibilities for weapons, motives, times of death, etc., can yield trillions of combinations.  

If there are a million possibilities, instead of saying there's "1,000,000 possibilities-worth" of uncertainty, we measure it as $log(1,000,000) = 19.93$ bits of uncertainty.  

*Bits?* Where did that come from? Bits are a unit of **information**, not uncertainty! But in fact information and uncertainty are closely related and can be measured on the same scale.

## Information as the Resolution of Uncertainty

> "Information is the resolution of uncertainty."  
> -- Claude Shannon  

Suppose I know who the murderer is, and you've narrowed it down to two suspects. How many bits of information do I need to provide to tell you who did it? Just one. The same applies to the murder weapon. Resolving your two bits of uncertainty requires two bits of information.  "Uncertainty" and "information" are two sides of the same coin. 

So every time you receive a byte of information, you can look at it this way. You start out not knowing what some value is. You just know know it's an 8-bit value. There are $2^8 = 256$ possible values. So your uncertainty is $log(256) = 8~bits$. When you find out the value of the first bit, you have cut the number of possibilities in half to $2^7 = 128$. Which means uncertainty is now $log(128) = 7~bits$. So each bit of information reduces your uncertainty by one bit, until you have received all $8$ bits, and no uncertainty remains.


## Unequal Probabilities

If there are $n$ equally probable possibilities, each has a probability $p = 1/n$. The uncertainty is:  

$$
log(1/p) = -log(p)
$$

But what if the possibilities aren't equally probable?

If possibility $i$ has probability $p_i$, the uncertainty for that possibility is $-log(p_i)$. This represents the information required to resolve that uncertainty.  

Why? If I know the correct possibility, I need on average $-log(p_i)$ bits to communicate it to you. Claude Shannon proved this in his seminal 1948 paper, "A Mathematical Theory of Communication," breaking open the field of information theory.

## Encoding

To understand why, suppose there are three possibilities: A (50%), B (25%), and C (25%). Before telling you which one it is, we come up with an efficient encoding scheme that minimizes the *expected* number of bits I need to communicate with you. We do this by assigning fewer bits to more probable cases:  

- A: 0  
- B: 10  
- C: 11  

If I send "0," you know it's A. If I send "1," you need another bit to distinguish B from C.  

The bits required depend on the outcome: $1 = -log(0.50)$ for A, and $2 = -log(0.25)$ for B or C.  

The expected number of bits is:  

$$
0.50 \cdot 1 + 0.25 \cdot 2 + 0.25 \cdot 2 = 1.5
$$

Generalizing, the formula for expected uncertainty is:  

$$
\sum_{x \in \text{possibilities}} p(x) \cdot -log(p(x))
$$

Shannon proved this holds for any probability distribution.  

For equally probable possibilities ($p = 1/n$):  

$$
\sum_{x} \frac{1}{n} \cdot -log\left(\frac{1}{n}\right) = log(n)
$$

## Conclusion

Shannon entropy generalizes uncertainty as the number of possibilities, measured logarithmically. Information corresponds to the bits needed to eliminate uncertainty, specifying one possibility from many.  

In the special case of equal probabilities, entropy simplifies to $log(n)$. For unequal probabilities, Shannon's formula calculates the average bits needed to identify the correct possibility using the most efficient encoding scheme.  
