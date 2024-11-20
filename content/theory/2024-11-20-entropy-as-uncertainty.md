---

layout: single
title:  "Entropy as Uncertainty"
toc: true
toc_sticky: true
weight: 49

---

## Uncertainty as the Number of Possibilities

What is the most straightforward, intuitive way to quantify uncertainty you could think of?

Suppose a murder has been committed. We don't know who did it. So there's uncertainty. But there are only 2 people who *could* have done it (Colonel Mustard or Professor Plum). So the uncertainty is limited. If there were 10 possible suspects, there would be more uncertainty. If there was only one person who could have done it, there would be no uncertainty.

So obviously one way of quantifying uncertainty is **the number of possibilities**.

This would actually be a pretty good measure, except what if not all possibilities are equally probable?

Let's say there are only two possible suspects, but we are 95% sure it was Colonel Mustard. So there's not that much uncertainty. If we were 99.9% sure there would be even less uncertainty. And if we were 100% sure, that's the equivalent of there only being 1 remaining possibility, and thus no uncertainty.

Even if there are 10 possibilities, if we are 99.99% sure it's the first guy, then there is not much uncertainty.

So in general, uncertainty **increases with the number of possibilities**, but **decreases as the probabilities approach the extremes**.

Below we'll see how we these concepts are generalized in the formula for Shannon Entropy.

## Multiplying Possibilities

Let's start with considering scenarios where there are n equally-probable possibilities. For these scenarios, let's tentatively measure uncertainty as the number of possibilities. So if there are 2 possible suspects then there are 2 possibilities-worth of uncertainty.

Now suppose the murder weapon is also unknown, but there are also 2 possibilities (the candlestick or the lead pipe). That's an additional 2 possibilities-worth of uncertainty.

Now what is the *total* uncertainty? We could just add: 2 possible murderers + 2 possible weapons = 4 possibilities. But that's not right. Possibilities don't add, they multiply. There are now 2 suspects x 2 weapons = 4 possible ways the murder could have happened. So our overall uncertainty is now 4.

## Log Scale

To make uncertainty additive, we can use a log scale. Uncertainty about the murderer is $log(2) = 1$, and uncertainty about the weapon is $log(2) = 1$, and total uncertainty is $log(4)$, which is just $1 + 1$.

A log scale is also convenient when the total number of possibilities is very big. Suppose we haven't narrowed down the list of suspects: anyone in the city could have done it. If we multiply $1,000,000$ possible suspects times the number of possible murder weapons, possible motives, times of death, etc., then unique possibilities can explode to billions, trillions, or more.

So if there are a million possibilities, instead of saying there are $1,000,000$ possibilities-worth of uncertainty, we measure it as $log(1,000,000) = 19.93$ bits of uncertainty.

*Bits*? Where did that come from? Isn't the bit a measure *information*?. Well yes, we can actually measure both uncertainty and information in units of bits. The two concept are very closely related.

## Information is the Resolution of Uncertainty

> "Information is the Resolution of Uncertainty."
>
> -- Claude Shannon 

Suppose *I* know who the murder is, and I know that you have narrowed it down to two suspects. How many bits of information do I need to provide you with to tell you which one it is? Just one, obviously. Same for the murder weapon. So I need to provide you a total of 2 bits of information to resolve your total 2 bits worth of uncertainty.

"Uncertainty" and "information" are just two sides of the same coin, or as Claude Shannon put it: “*Information is the resolution of uncertainty.*”

So just as "dollars" can be used to measure an increase, decrease, or total value of your bank account, "bits" can be used to measure both uncertainty and the increase or decrease in uncertainty.

So every time you receive a byte of information, you can look at it this way. You start out not knowing what some value is. You just know know it's an 8-bit value. There are $2^8 = 256$ possible values. So your uncertainty is $log(256) = 8~bits$. When you find out the value of the first bit, you have cut the number of possibilities in half to $2^7 = 128$. Which means uncertainty is now $log(128) = 7~bits$. So each bit of information reduces your uncertainty by one bit, until you have received all $8$ bits, and no uncertainty remains.

If there are $n$ possibilities, all equally probable, then each possibility has a probability of $1/n$. If start out knowing just the probability p. We can convert that to $n$ by taking the inverse, then then take the log of that. So uncertainty is:

$$
	log(1/p) = -log(p)
$$

## Differing Probabilities

Now suppose not all possibilities are equally probable? 

Well, given possibility $i$ has probability $p_i$, we can still calculate $-log(p_i)$. We can consider this to be the uncertainty associated just with item $i$. This is because if I know which possibility is the correct one, then in order to communicate this to you, I will need to provide you with, on average, $-log(p)$ bits of information.


Now that final claim might seem like a bit of a leap. And it is. It's not obvious, but it has been proven to be the case. 

## Encoding

To develop your intuition of why this is the case, consider a scenario where there are 3 possibilities. There is a 50% chance of A, a 25% chance of B, and a 25% chance of C. To communicate this to you with as few bits as possible, we will need to come up with an efficient encoding scheme; one that requires, *on average*, or *on expectation*, the fewest possible bits.

The most efficient encoding requires only 1 bit for the most probable case (A), and 2 bits for the others:

	A: 0
	B: 10
	C: 11

So if I send you an 0, we're done! You know it's A. If I send you a 1, you need to wait for the next bit to know whether it's B or C.

The number of bits required to communicate the correct possibility depends on which one it it is. It takes $1 = -log(.50)$ if its an A, and $2 = -log(.25)$ if its a B or a C.

What is the *expected* number of bits required? It's

$$
	.50 ⋅ 1 + .25 ⋅ 2 + .25 ⋅ 2 = 1.5 
$$


Which can be generalized as:

$$
	∑_{x \in possibilities} p(x) * -log(p(x))
$$


Now our hero, Claude Shannon, has proved that the above formula is true *no matter what the probability distribution*! 

This means that, if possibility $x$ has probability $p(x)$, it will contribute, $p(x) ⋅ -log(p(x))$ to the total number of bits required, on average, to communicate which is the correct possibility, using the most efficient possible encoding scheme. Or in other words, it contributes that amount to the total uncertainty.

When all $n$ possibilities are equally probable, then:

$$
	∑_{x \in possibilities} 1/n ⋅ -log(1/n) = -log(1/n) = log(n)
$$


## Conclusion

Shannon entropy can be thought of as a generalization of the idea of uncertainty as the number of possibilities, measured on a log scale. And information can be understood as the number of bits required to eliminate uncertainty, or in other words to communicate one of the possibilities. 

In the the special case where all possibilities are equally probable, we get the familiar relationship where $log(n)$ bits are encode a value with $n$ possibilities. If they are not equally probable, then Shanon's formula gives us a general formula for the number of bits required to communicate one possibility using the most efficient possible encoding scheme.

