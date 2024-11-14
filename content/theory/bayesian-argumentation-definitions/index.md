---
layout: single
title:  "Bayesian Argumentation Definitions"
slug: bayesian-argumentation-definitions
toc: true
tags: ['Argumentation Theory']
series: ['Bayesian Argumentation']
weight: 75
canonical_url: https://jonathanwarden.com/bayesian-argumentation-definitions/

---

## Bayesian Argumentation: Summary of Definitions

Below is a summary of all the terms and equations defined in the essays in this series, followed by a detailed example.

*For an argument with premise 𝐵 and conclusion 𝐴, and a subject whose beliefs are represented by probability measure P...*

- **Relevant**: The premise is **relevant** to the conclusion (or, the argument is relevant) **iff** $P(A \vert B) ≠ P(A \vert \bar{B})$
    - Otherwise, the premise is **irrelevant** to the conclusion  (or, the argument is irrelevant)
        - Irrelevance implies statistical independence of A and B.

- **Support**: The premise **supports** the conclusion **iff** $P(A \vert B) > P(A \vert \bar{B})$

- **Oppose**: The premise **opposes** the conclusion **iff** $P(A \vert B) < P(A \vert \bar{B})$
    - If 𝐵 supports 𝐴, then 𝐵 opposes $\bar{A}$

- **Relevance**: The relevance of the premise to the conclusion is $R(A,B) = P(A \vert B) - P(A \vert \bar{B})$

- **Conditional Relevance**: *Given some third premise 𝐶*: $R(A,B \vert C) = P(A \vert B,C) - P(A \vert \bar{B},C)$

- **Corelevant**: The premises 𝐵 and 𝐶 are corelevant to the conclusion 𝐴 iff: $R(A,B \vert C) ≠ R(A,B \vert \bar{C})$

- **Corelevance**: $CR(A;B,C) = R(A,B \vert C) - R(A,B \vert \bar{C}) = R(A,C \vert B) - R(A,C \vert \bar{B})$

- **Necessity**: The necessity of the premise to the conclusion is $N(A,B) = P(A) - P(A \vert \bar{B}) = P(B)R(A,B)$

- **Sufficiency**: The sufficiency of the premise for the conclusion is $S(A,B) = P(A \vert B) - P(A) = P(\bar{B})R(A,B)$

- **Testimony Event**: The event, directly observed by the subject, that the arguer asserted the premise in support of the conclusion.

- **Post-Argument Belief**: *Given the testimony event I*: $P_i(∙) = P(∙ \vert I)$
    - e.g. $P_i(B) = P(B \vert I)$ is the post-argument belief in 𝐵.

- **Informative**: The assertion of the premise is **informative** (the argument is informative) **iff** $P_i(B) > P(B)$

- **Informativeness**: The informativeness of the argument is $P_i(B) - P(B)$

- **Persuasive**: The argument is **persuasive** **iff** $P_i(A) > P(A)$
    - Alternatively, the argument is **persuasive** if the argument is relevant and informative

- **Persuasiveness**: The persuasiveness of the argument is $P_i(A) - P(A)$


<!--

- **Necessity for Relevance**: The **necessity of claim 𝐶 for the relevance of premise 𝐵 to conclusion 𝐴** is the difference (absolute, percent, relative entropy, etc.) between $R(A,B)$ and $R(A,B \vert \bar{C})$$

- **Warrant**: The warrant is the premise with the highest necessity for the relevance of the premise to the conclusion. 
    - Given a set Σ of claims that the subject believes in, the warrant is:
    $ \text{arg}\,\max\limits_{S ∈ Σ}\,-R(A,B \vert \bar{S}) $$
-->


## Key Equations

And here is a summary of key equations in this series: $\label{1}$

- Jeffrey's Rule: $$P'(A) = P(A \vert \bar{B}) + P'(B)R(A,B)\tag{1} $$
- Relevance of Rejection of Premise/Conclusion: $R(A,B) = -R(A,\bar{B}) = -R(\bar{A},B) = R(\bar{A},\bar{B})$
- Symmetry of Corelevance: $CR(A;B,C) = CR(A;C,B)$
- Necessity = Relevance × Acceptance: $N(A,B) = P(A) - P(A \vert \bar{B}) = R(A,B)P(B)$
- Sufficiency = Relevance × Rejection: $S(A,B) = P(A \vert B) - P(A) = R(A,B)P(\bar{B})$
- Relevance = Necessity + Sufficiency: $R(A,B) = N(A,B) + S(A,B)$
- Sufficiency/Necessity of Rejection of Premise/Conclusion: $N(A,B) = S(\bar{A},\bar{B})$ and $S(A,B) = N(\bar{A},\bar{B})$
- Persuasiveness = Relevance × Informativeness: $ P_i(A) - P(A) = (P_i(B) - P(B))R(A,B) $


## Numerical Example

The following example illustrates all of the concepts introduced in this series.

Suppose the priors of the subject are modeled by the probability measure 𝑃 given in this table: 

| a | b | P(a,b)     |
| - | - | ---------- |
| $\bar{A}$ | $\bar{B}$ |  .25       |
| $\bar{A}$ | 𝐵 |  .10       |
| 𝐴 | $\bar{B}$ |  .25       |
| 𝐴 | 𝐵 |  .40       |

The marginal probabilities are:

$$
\begin{aligned}
    P(A) &= P(A,B) + P(A,\bar{B}) = .40 + .25 = .65 \cr
    P(B) &= P(A,B) + P(\bar{A},B) = .40 + .10 = .50
\end{aligned}
$$

And the conditional probabilities:

$$
\begin{aligned}
    P(A|B) &= \frac{P(A,B)}{P(B)} = \frac{.4}{.5} = .8  \cr
    P(A|\bar{B}) &= \frac{P(A,\bar{B})}{P(\bar{B})} = \frac{.25}{(1 - .5)} = .5
\end{aligned}
$$

**Relevance**


Which lets us calculate the relevance:

$$
    R(A,B) = P(A|B) - P(A|\bar{B}) = .8 - .5 = .3
$$

**Necessity and Sufficiency**

The necessity of 𝐵 to 𝐴 is:

$$
    N(A,B) = P(A) - P(A|\bar{B}) = .65 - .5 = .15
$$

And the sufficiency of 𝐵 to 𝐴 is:

$$
    S(A,B) = P(A|B) - P(A) = .8 - .65 = .15
$$

Notice that relevance is the sum of necessity and sufficiency:

$$
    R(A,B) = N(A,B) + S(A,B) = .15 + .15 = .3
$$

And that necessity is relevance times acceptance:

$$
    N(A,B) = R(A,B)P(B) = .3 \times .5 = .15
$$

And that sufficiency is relevance times rejection:

$$
    N(A,B) = R(A,B)(1 - P(B)) = .3 \times (1 - .5) = .15
$$



<!--
The necessity expressed as information gain would be (using log base 2):

$$
\begin{aligned}
    &P(A) log(\frac{P(A)}{P(A|\bar{B})}) + P(\bar{A}) log(\frac{P(\bar{A}))}{P(\bar{A}|\bar{B}}) \cr 
    &=  0.8 log(\frac{0.8}{0.3}) + (1-0.8) log\frac{(1-0.8)}{(1-0.3)} \cr
    &= .25 \text{ bits of information}
\end{aligned}
$$


p = .5
q = .77
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))

import math
p = .8
q = .3
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))

-->
**Post-Argument Belief**

Now suppose the assertion of 𝐵 in support of 𝐴 causes the subject to increase their belief in 𝐵 from $P(B)=50\\%$ to $P_i(B)=90\\%$.

The subject's post-argument belief in 𝐴 will be, according to formula $\eqref{1}$:

$$
\begin{aligned}
    P_i(A)  &= P(A|\bar{B}) + P_i(B)R(A,B)  \cr
            &= .5 + .9 \times .3  \cr
            &= .77
\end{aligned}
$$

This is slightly less than $P(A \vert B)=.8$ because the subject still harbors some doubt about 𝐵.



**Informativeness**

The **informativeness**  is:

$$
    P_i(B) - P(B) = 0.9 - 0.5 = 0.4
$$

<!--
$$
\begin{aligned}
        &P_i(B) log(\frac{P_i(B)}{P(B)}) + P_i(\bar{B}) log(\frac{P_i(\bar{B})}{P(\bar{B})})  \cr
        &= P_i(B) log(\frac{.99}{.5}) + P_i(.01) log(\frac{.01}{.5}) \cr
        &= 0.53 \text{ bits of information}

\end{aligned}
$$
-->

<!--
import math
p = .90
q = .5
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))
-->

**Persuasiveness**

And the persuasiveness is:

$$
    P_i(A) - P(A) = 0.77 - 0.65 = 0.12
$$

Notice that persuasiveness is equal to relevance times informativeness:

$$
   P_i(A) - P(A) = R(A,B)(P_i(B) - P(B)) = 0.3 × (0.9 - 0.5) = 0.12
$$

**Post-Argument Necessity and Sufficiency**

If **after** the argument the subject were to learn **additional** information causing them to reject 𝐵, the **new** posterior would be $P_i(A \vert \bar{B}) = P(A \vert \bar{B}) = .5$. 

The post-argument necessity is therefore:

$$
    N_i(A,B) = P_i(A) - P_i(A | \bar{B}) = .77 - .5 = .27
$$

And if the subject were to learn additional information causing them to accept $B$ completely, then new posterior would be $P_j(A) = P_i(A \vert B) = P(A \vert B) = .8$.

The post-argument sufficiency is therefore:

$$
    S_i(A,B) = P_i(A \vert B) - P_i(A) = .8 - .77 = .03
$$


<!--

Persuasiveness expressed as information gain would be:

$$
\begin{aligned}
    &P_i(A) log(\frac{P_i(A)}{P(A)}) + P_i(\bar{A}) log(\frac{P_i(\bar{A})}{P(\bar{A})}) \cr 
    &=  0.8 log(\frac{0.8}{0.5}) + (1-0.8) log\frac{(1-0.8)}{(1-0.5)} \cr
    &= 0.32\text{ bits of information}
\end{aligned}
$$
-->

<!--
p = .5
q = .77
p*math.log2(p/q) + (1-p)*math.log2((1-p)/(1-q))
-->


<style>
.sample-distribution {
    table-layout: auto; 
    display: table;
    width: 100%;
    max-width: 250px;
    margin: 25px auto;
} 

.example
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

.example h3 {
    margin-top: 0px;
}


</style>






