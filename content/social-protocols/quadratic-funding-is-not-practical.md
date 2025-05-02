---
title: "8 Reasons Why Quadratic Funding is Not Practical"
slug: quadratic-funding-is-not-practical
date: "2025-05-02"
math: true
weight: 1
---


# 8 Reasons Why Quadratic Funding Is Not Practical


Quadratic funding has received a lot of attention recently as a mechanism for funding public goods -- especially in the cryptocurrency space. QF is appealing because it is theoretically optimal under certain assumptions. The problems is that these assumptions don't ever hold in reality. 

The theory behind QF is robust and elegant. And the authors of the original paper are clear about the assumptions and they do not claim that these assumptions are likely to hold in reality.

Unfortunately practitioners of QF have been a bit too enthusiastic in embracing it, implementing it in settings where theory actually predicts poor results. 

Here is a list of the assumptions that have to hold for quadratic funding to have its desirable theoretic properties. When these assumptions don't hold, the theory predicts that QF is very **inefficient**. And in most realistic settings these assumptions obviously do not hold.

**List of Assumptions:**

- There is no Wealth Inequality
- Deficits are "Free"
- Contributors are Selfish
- Equilibrium
- Sufficient Budget
- Diminishing returns
- Perfect Knowledge
- Independence (no collusion)

## There is no Wealth Inequality

> Our interest here is in maximization of dollar-equivalent value rather than achieving an equitable distribution of value (we assume that an equitable distribution of basic resources has been achieved in some other manner, such as an equal initial distribution of resources)
> 
> - Buterin, Hitzig, Weyl in the original quadratic funding paper [^1]

Obviously we don't live an a world with an equitable distribution of basic resources. This means large contribution amounts don't necessarily imply greater utility -- they may simply imply greater wealth. 

This is a problem if the goal is to optimize social welfare.

Consider this example. Ten wealthy art patrons each contribute €1,000,000 to buy more artwork for the local public art museum. The total funding per the QF formula is $(10×\sqrt{1,000,000})\^2$=€100,000,000. Most of this comes from a subsidy of €90,000,000.

100 poor people each contribute €100 to their underfunded local police department. Total funding per the QF formula is $(100×\sqrt{100})\^2$=€1,000,000. The subsidy amount is $990,000.

Intuitively, this seems very wrong. The art museum gets more than 10x the subsidy, yet there are 10 times as many people who would benefit from the police department.

If deficits are being funded by taxpayer money, then quadratic funding can function as a mechanism for transferring wealth from the poor to the rich.

## Deficits are Free

The optimality of QF assumes that contributors don't care about deficits. But as the original QF paper states "...once we account for the deficit, the QF mechanism does not yield efficiency..."

The money to fund deficits come from *somewhere*. If the deficit are paid by a charitable contribution, there is an opportunity cost for contributors -- that money is no longer available to fund other charitable projects that may benefit them. If deficits are paid by the government, then contributors may may ultimately end up paying for the deficits with increased future taxes. 

If the tax burden is spread out among millions of people, it might feel like the deficit spending is essentially free. But consider the above case of the $90,000,000 in subsidies to the public art museum. This creates a lot of dollar-valued utility for some very wealth art patrons. But the cost is paid by the average taxpayer. Depending on the progressiveness of the tax scheme, the wealthy can end up getting more money out of the government than they put in. 

## Contributors are Selfish

Ironically, if the individual contributors have motives that are at least partially altruistic, QF will result in fund projects beyond the point that maximizes social welfare. See the proof Appendix A in [Beyond Collusion Resistance: Leveraging Social Information for Plural Funding and Voting]

The entity funding the deficit is, presumably, just trying to maximize social welfare. But if the individual contributors are also just trying to maximize social welfare then, ironically, QF provides no benefit *even under otherwise ideal conditions*.

When individuals make contributions for purely altruistic reasons, they don't directly experience the utility themselves. And yet the logic of the QF mechanism assumes that they have. This results in utility being "double counted" -- QF assumes the utility to each beneficiary of the charity is vicariously experienced by each contributor. This double-counting results results in over-allocation of funds, which results in *net decrease in social welfare*.

To understand this, consider the following two scenarios: one with selfish contributors, and one with altruistic contributors. 

Suppose three wealthy art patrons each contribute €1,000,000 to the local public art museum via quadratic funding. Their contributions are *not* altruistic: they each expect to experience €6,000,000 worth of individual utility from enjoying the additional €9,000,000 of art. The total funding per the quadratic funding formula is $( 3×\sqrt{1,000,000} )^2$ = €9,000,000.  So the net social welfare created is:

$$
\begin{aligned}
  \text{Social Welfare} 
  &= \text{Total Utility} - \text{Total Funding} \newline
  &= 3 × 6,000,000 - 9,000,000 \newline
  &= 9,000,000
\end{aligned}
$$

Now imagine a nearly identical scenario, but the contributors are charities funding a cancer research project. We use the same utility functions, but these are now a function of *how many lives they expect to save*. Each charity knows that if they allocate funds efficiently, every €100,000 can save 1 life, and they estimate €9,000,000 of funding for this project would save about 60 lives. Thus their utility is €100,000 x 60 = €6,000,000.

So the total social welfare created is:

$$
\begin{aligned}
  \text{Social Welfare} 
  &= \text{Total Utility} - \text{Total Funding} \newline
  &= 6,000,000 - 9,000,000 \newline
  &= -3,000,000
\end{aligned}
$$

What happened here? We went from €9,000,000 of social welfare created in the first scenario to €3,000,000 of social welfare destroyed in the second scenario! But the utility functions are the same?

The difference, of course, is that in the second scenario we didn't multiply the €6,000,000 of utility by three. Each of the charities did not independently experience €6,000,000 worth of "utility".

Now of course since these organizations are trying to maximize social welfare, they would not each contribute €1,000,000. Instead, once *somebody* contributed enough to maximize social welfare, everybody else would stop contributing. Thus the QF funding mechanism is moot. Even if all other assumptions hold, **the amount contributed under quadratic funding is the same as the amount contributed under private contributions**!.


## Equilibrium

In QF funding round there is a theoretical **equilibrium**. At equilibrium, each individual contributes the amount that maximizes their own utility *given what everybody else is contributing*. 

So each individual's optimal contribution *depends on what other individuals are contributing*. But how do individuals know how much others are going to contribute? Well in reality, *they don't*: this assumption simply doesn't plausibly hold in any realistic setting.

In order for this equilibrium to actually be realized, one of two things have to be the case:

1. Utility functions are common knowledge (the [complete information](https://en.wikipedia.org/wiki/Complete_information) assumption). Every contributor knows the utility functions of *every other contributor*, and can use this knowledge to independently calculate the equilibrium contributions of everyone else, essentially simulating the above *tatonnement* process. But this is a complex calculation that realistically would have to be done by a computer.
2. There is an equilibrium discovery process where voters iteratively adjust contribution depending on what other people are contributing, until an equilibrium is reached. To my knowledge, QF funding tools don't provide tools for this process. And it would require a lot of time and effort from contributors and would be susceptible to timing-based manipulation (e.g. waiting to the last minute to contribute).

If voters aren't able to discover the equilibrium contribution, what do they do? They must somehow guess how much others will contribute. But since they don't know the utility functions of every other possible contributor, and have no idea what other people know, then this is just a wild guess, based on signals such as the popularity of a project. 

A study on [Quadratic Funding with Incomplete Information](https://globalprioritiesinstitute.org/wp-content/uploads/Luis-V.-M.-Freitas-Wilfredo-L.-Maldonado-Quadratic-Funding-with-Incomplete-Information.pdf) gives a framework for estimating the inefficiency of the QF mechanism depending on the shape of contributors utility functions. 


## Sufficient Budget

Funding caps break quadratic funding. There must be sufficient funds to subsidize the deficit, or there is no longer a unique equilibrium. Lack of a unique equilibrium means instead of a single global optimal outcome, there are many possible outcomes where everybody would be contributing the optimal amount given what everyone else is contributing. But some of these outcomes are better for some individuals and worse for others.

This situation is sometimes described as a "free-rider" problem, because everybody hopes that the final equilibrium is one where they don't pay anything (they get a free ride). But since people don't know what other people are going to pay, they end up just guessing or hoping. The results will almost certainly not be socially optimal.

To avoid funding caps, the entity organizing the QF funding round knows approximately what the equilibrium contributions will be beforehand. Knowing this, they must ensure they have sufficient money set aside to subsidize the deficit. If they do not have sufficient funds, then can use a generalization of QF called Capital Constrained Quadratic Funding, where they choose *in advance* a fraction of the deficit that they will subsidize. But they must then be able to subsidize this fraction of the deficit.


## Diminishing Returns

QF only works if contributors utility functions are 1) monotonically increasing -- any increase in funding results in increased utility -- and 2) concave -- decreasing marginal utility.

These assumptions don't hold, for example, for fixed-price projects. Using QF for a fixed-price project would require some coordination mechanism to ensure the actual project price gets funded -- no more, no less -- but even given this mechanism, there may be multiple equilibria -- different ways the costs of the project are divided among contributors. This once again creates a messy free-rider problem, where people ultimately contribute based on wild guesses and total funding amounts are unlikely to be socially optimal.

## Perfect Knowledge

Another assumption required for optimality is that contributors have sufficient knowledge of projects.

Specifically, they must **know about all projects**, and know their utility functions for all of them. Otherwise, they obviously can't make optimal contributions.

Further, they must know how much funding each project has received via other funding mechanisms, because their optimal contribution depends on how much funding the project is receiving without their contribution.


## Independence (no collusion)

Collusion and fraud are probably the most well-understood problem with QF. The QF paper assumes each individual acts completely independently: there is no bribery or coordination of any sort. The authors recognized what happens if this is not the case:

> ...if the size of this group is greater than $`1/α`$ and the group can perfectly coordinate, there is no limit (other than the budget) to how much it can steal."
> 
> - Buterin, Hitzig, Weyl in the original quadratic funding paper [^1]

There has been some good research on collusion-resistant variants of QF such as [Connection-Oriented Cluster Match (COCM)](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4311507). Although collusion resistant mechanism can reduce the amount an attacker can steal, they necessaril sacrifices optimality as well as the unique equilibrium -- two of the main reasons for using QF in the first place. This is something I hope to write more on in a followup post.





