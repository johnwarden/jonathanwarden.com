---

layout: single
title:  "Necessity and Sufficiency"
toc: true
toc_sticky: true
tags: ['Argumentation Theory']
weight: 72
sidebar:
  - title: "In This Series"
    nav: "bayesian-argumentation"
  - nav: "bayesian-argumentation-related"
    title: "Related Articles"
series: ['Bayesian Argumentation']
canonical_url: https://jonathanwarden.com/necessity-and-sufficiency/

---

## Argument and Information

In the [previous essay](/relevance) in this series, we introduced the idea of **relevance**, and said that a premise is relevant to the conclusion iff $P(A \vert B) > P(A \vert \bar{B})$.

Consider the argument (𝐴) *this is a good candidate for the job* because (𝐵) *he has a pulse*. Having a pulse may not be a very **persuasive** reason to hire somebody, but it is probably quite **relevant**, because if the candidate did **not** have a pulse, the subject would probably be much less likely to want to hire him. That is $P(A \vert B) > P(A \vert \bar{B})$.

So to be **persuasive**, the premise must not only be relevant: it must actually change the probability that the subject accepts the conclusion.

Why isn't 𝐵 *he has a pulse* persuasive to the subject? Because presumably, *he has a pulse* does not tell the subject anything he didn't already know, or at least didn't already assume was probably true. **An argument can only change the beliefs of a Bayesian reasoner if it provides new information**.

 <aside class="custom-aside" markdown="1">

To understand why only new information can change the beliefs of a Bayesian reasoner, consider the standard rule for Bayesian belief revision. When a Bayesian reasoner learns that 𝐵 is true, their posterior belief in the probability of 𝐴 is updated to equal what they previously believed the probability of 𝐴 **would be** if they believed 𝐵. Mathematically, the posterior belief, denoted $P'$, is updated according to the well-known formula for Bayesian belief revision:

$$
    P'(A) = P(A|B)
$$

But if 𝐵 is not new information -- that is, if the subject already believed that $P(B) = 1$, then they must necessarily have *already* updated their belief in $A$, so that the prior belief $P(A) = P(A \vert B)$. So the posterior $P'(A)$ and the prior $P(A)$ are both equal to $P(A \vert B)$. 

On the other hand, if the subject had, for some reason, previously assumed that the candidate did *not* have a pulse, and then subsequently learned that he did, this discovery might considerably brighten the candidate's prospects. 

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


## Necessity and Sufficiency

So the effect of learning that the premise is true or false depends on the subject's **prior** degree of acceptance of the premise. 

If the subject already accepts the premise and conclusion, and they wouldn't accept the conclusion if they didn't accept the premise, then the premise is **necessary**. If the subject does not already accept the premise or the conclusion, but they would accept the conclusion if they did accept the premise, then the premise is **sufficient**.

### Examples: Necessary but not Sufficient:

Consider the following examples. You probably agree that these premises all seem like they should be necessary but not sufficient.

- *This is a good candidate* **because** *he has a pulse*.
- *Tweety can fly* **because** *he has wings*.
- *My car will start* **because** *it has a battery*.

Notice also that these premises all seem, *a priori*, probable, and that is why the conclusions seem *a priori* probable. Knowing nothing about Tweety except his name, you are probably not surprised that he is a bird, and that's why you aren't surprised that Tweety can fly. If you rejected the premise that Tweety was a bird, you would likely reject the conclusion that he can fly. So the premise is necessary.

### Examples: Sufficient but not Necessary:

For the following examples, you probably agree that the premises all seem like they should be (fairly) sufficient for the conclusion, but not necessary. 

- *This is not a good candidate* **because** *he doesn't have a pulse*.
- *John can fly* **because** *John is a bird*.
- *The world will end next year* **because** *it will be hit by a giant asteroid*.

Notice also that these premises all seem, *a priori*, improbable, or surprising, and that is why the conclusions seem *a priori* improbable. Knowing nothing about John except his name, you probably didn't expect him to be a bird, and that's why you didn't expect him to be able to fly. But if you accept the premise that John is a bird, it is easy to accept the conclusion that he can fly. So the premise is sufficient.

## Partial Acceptance of the Premise

So for a premise to be necessary, the subject must accept it to some degree: it must be *a priori* probable. For it to be sufficient, they must reject it to some degree: it must be *a priori* improbable. 

In many cases, the subject neither completely accepts nor completely rejects the premise. In such cases it can be **both sufficient and necessary**.

### Examples: Both Necessary and Sufficient

For the following examples, you probably agree that the premises seem like they are both somewhat sufficient and somewhat necessary. 

- *This is an above-average candidate* **because** *she scored above average on the skills test*.
- *It is nighttime* **because** *it is dark outside*.
- *The economy is doing poorly* **because** *stock prices are falling*.

Notice also that these premises all seem, *a priori*, roughly equally likely to be true or false. The subject neither completely accepts nor completely rejects the premise, and that is why they neither completely accept nor completely reject the conclusion. If they completely rejected the premise, the conclusion would seem less likely. So the premise is somewhat necessary. If they completely accepted the premise, the conclusion would seem more likely. So the premise is somewhat sufficient.

## Quantifying Necessity and Sufficiency

<!--

Now what if the subject learned that these premises were definitely true, or definitely false? This would
 probably be almost sufficient to accept or reject the premise, respectively.



Now, a Bayesian agent may only partially accept the premise or conclusion. And yet their partial acceptance of the premise may be necessary for their partial acceptance of the conclusion. For example, the subject may believe that the candidate **almost definitely** has a pulse, and that they are **probably** a good candidate, but almost definitely would not be if they didn't have a pulse. So necessity is a matter of degree.


For example, the subject may believe that the candidate **probably** has a pulse, and that they are **probably** a good candidate, but almost definitely would not be if they didn't have a pulse. So necessity is a matter of degree.


So if the subject learned that premises were true, this would be probably sufficient to accept the premise.
 So if the subject learned that these premises were **not** true, this would probably be sufficient to reject the premise.

In fact, a premise can be *just a little bit* sufficient/necessary. The belief that *Tom Cruise is in Top Gun II* might be necessary for the subject's belief that Top Gun II is a little more likely than average to be a good movie. And learning that *Tim liked Top Gun II* may be sufficient to conclude that it may be worth going to see. 

-->

We can quantify the degree to which a premise is necessary by considering how much the subject's belief in the conclusion would decrease if they rejected the premise. Likewise we can quantify sufficiency as how much their belief in the conclusion would increase if they accepted the premise.

#### Necessity

The **necessity** of 𝐵 to 𝐴 is:

$$
    N(A,B) = P(A) - P(A|\bar{B})
$$

#### Sufficiency

The **sufficiency** of 𝐵 to 𝐴 is:

$$
    S(A,B) = P(A|B) - P(A)
$$

### Identities


#### Necessity = Relevance × Acceptance

Clearly, a premise must be relevant to be either necessary or sufficient. 

Further, for a premise to be necessary, the subject must to some degree accept it (otherwise learning it was false it would not be new information). 

In fact, necessity is just the product of relevance and acceptance of the premise ([proof](#proof-1)).

$$
\begin{aligned}
    N(A,B) &= R(A,B)P(B) \cr
\end{aligned}
$$

#### Sufficiency = Relevance × Rejection

And for a premise to be sufficient, the subject must not completely accept it (otherwise, learning it was true would not be new information). In fact, sufficiency is just the product of relevance and rejection of the premise ([proof](#proof-2)):

$$
\begin{aligned}
    S(A,B) &= R(A,B)P(\bar{B})
\end{aligned}
$$

#### Necessity + Sufficiency = Relevance

It follows that as long as the premise is not completely accepted or rejected, it is **both necessary and sufficient**. 

What's more, a premise cannot be relevant if it is completely accepted or rejected. For example, if the subject completely accepted or rejected the premise ($P(B)$ equals 0 or 1), relevance would be undefined, because either $P(A \vert B)$ or $P(A \vert \bar{B})$ would be undefined. For example, if $P(B)=0$, then $P(A \vert B) = P(A,B)/P(B) = P(A,B)/0$, which is undefined.

This means that **as long as the premise is relevant, it will be both necessary and sufficient** to some degree. In fact, relevance is **the sum of necessity and sufficiency**. So $P(B)$ just partitions relevance into components of necessity and sufficiency. **The more the premise is accepted, the more necessary and the less sufficient**, and vice versa.

$$
\begin{aligned}
    R(A,B) &= P(B)R(A,B) + (1-P(B))R(A,B)\cr
            &= N(A,B) + S(A,B)
\end{aligned}
$$

This relationship is illustrated in the chart below:

<!-- This image is generated using R. Source: necessity-and-sufficiency-chart.R -->
<img src="necessity-and-sufficiency.png"
     alt="Necessity and Sufficiency Chart"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 800px" />

So when we say that *he has a pulse* is necessary but not sufficient, we mean that it is very necessary, and not very sufficient. Likewise, *he is the most qualified candidate ever* may be quite sufficient, but certainly not very necessary.

<!--
A premise can have a high degree of both sufficiency and necessity in the case that the probability of accepting the premise is close to even, and relevance is therefore split fairly evenly between sufficiency and necessity. For example, consider the case where the subject for some reason thinks there is an even chance that the candidate has a pulse.


-->


#### Rejection of the Premise

To the degree that the premise is necessary for the conclusion, the rejection of the premise is sufficient for rejection of the conclusion. Thus *he **doesn't** have a pulse* is sufficient for the conclusion *this is **not** a good job candidate* ([proof](#proof-3)). 

$$
    N(A,B) = S(\bar{A},\bar{B})
$$


<style>
.proof
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

</style>

It follows trivially that, to the degree that the premise is sufficient for the conclusion, rejection of the premise is necessary for rejection of the conclusion. 

$$
    S(A,B) = N(\bar{A},\bar{B})
$$


## Summary 

- **Necessity**: $N(A,B) = P(A) - P(A \vert \bar{B}) = P(B)R(A,B)$
- **Sufficiency**: $S(A,B) = P(A \vert B) - P(A) = P(\bar{B})R(A,B)$
- The greater the prior acceptance of the premise, the more necessary it is for the conclusion
    - Necessity = Relevance × Acceptance: $N(A,B) = P(A) - P(A \vert \bar{B}) = R(A,B)P(B)$
- The greater the prior acceptance of the premise, the less sufficient it is for the conclusion
    - Sufficiency = Relevance × Rejection: $S(A,B) = P(A \vert B) - P(A) = R(A,B)P(\bar{B})$
- Relevance = Necessity + Sufficiency: $R(A,B) = N(A,B) + S(A,B)$
- The more necessary the premise for the conclusion, the more sufficient the rejection of the premise for rejection of the conclusion, and vice versa.
    $N(A,B) = S(\bar{A},\bar{B})$ and $S(A,B) = N(\bar{A},\bar{B})$



## Next in this Series

So we have thoroughly explored the idea of necessity and sufficiency, and how they relate to relevance and the degree of acceptance of the premise.

But an important question remains. Will the subject accept the premise? In the next essay in this series, we explore this question and define the ideas of [**informativeness and persuasiveness**](/informativeness-and-persuasiveness). 


## Proofs

The proofs below use the following equality, which is derived in the [previous essay](/relevance#relevance-as-slope).

$$
\label{1}
P(A) = P(A|\bar{B}) + P(B)R(A,B) 
\tag{1}
$$


### Proof 1

**Necessity = Relevance × Acceptance**

$$
\begin{aligned}
    N(A,B) &= P(A) - P(A|\bar{B}) \cr 
            &= ( P(A|\bar{B}) + R(A,B)P(B) ) - P(A|\bar{B}) &&\text{Formula }\eqref{1} \cr
            &= R(A,B)P(B)
\end{aligned}
$$

### Proof 2

**Sufficiency = Relevance × Rejection**

**Proof:**

$$
\begin{aligned}
    S(A,B) &= P(A|B) - P(A) \cr
            &= P(A|B) - ( P(A|\bar{B}) + R(A,B)P(B) ) &&\text{Formula }\eqref{1}\cr
            &= ( P(A|B) - P(A|\bar{B}) ) - R(A,B)P(B) \cr
            &= R(A,B) - R(A,B)P(B)\cr
            &= R(A,B)(1 - P(B))\cr
            &= R(A,B)P(\bar{B})
\end{aligned}
$$

### Proof 3

$$
    N(A,B) = S(\bar{A},\bar{B})
$$

**Proof:**

$$
\begin{aligned}
    N(A,B) &= P(A) - P(A|\bar{B}) \cr
            &= (1 - P(A|\bar{B})) - (1 - P(A)) \cr
            &= P(\bar{A}|\bar{B}) - P(\bar{A}) \cr
            &= S(\bar{A},\bar{B})
\end{aligned}
$$

