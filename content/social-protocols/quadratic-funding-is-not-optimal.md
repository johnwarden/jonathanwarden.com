---

title: "8 Reasons Why Quadratic Funding Is Not Optimal"
slug: quadratic-funding-is-not-optimal
date: "2025-05-02"
math: true
weight: 1
---

## Introduction

Quadratic funding has received a lot of attention recently as a mechanism for funding public goods—especially in the cryptocurrency space. QF is appealing because it is theoretically optimal under certain assumptions[^1]. 

The problem is that these assumptions don’t ever hold in reality.

The theory behind QF is sound and elegant, and the authors of the [original paper](https://scholar.harvard.edu/files/hitzig/files/buterin_hitzig_weyl_draft.pdf) are clear about the assumptions. They don't claim they are likely to hold in reality, and warn about the consequences when they don't hold. Unfortunately, practitioners have sometimes been too enthusiastic, implementing QF in settings where theory actually predicts poor results.

Below is a list of the assumptions that must hold for quadratic funding to have its desirable theoretical properties. When these assumptions fail, the theory predicts outcomes that are far from optimal.

**List of Assumptions:**

* [Wealth equality](#wealth-equality)
* [Free subsidies](#free-subsidies)
* [Selfish contributors](#selfish-contributors)
* [Equilibrium Discovery](#equilibrium-discovery)
* [Sufficient budget](#sufficient-budget)
* [Diminishing returns](#diminishing-returns)
* [Perfect knowledge](#perfect-knowledge)
* [Independent agents](#independent-agents)


Below is a brief explanation of these assumptions and what happens when they don't hold.

## Wealth Equality

> Our interest here is in maximization of dollar-equivalent value rather than achieving an equitable distribution of value (we assume that an equitable distribution of basic resources has been achieved in some other manner, such as an equal initial distribution of resources)
> 
> —Buterin, Hitzig & Weyl[^1]

Obviously, we don’t live in a world with an equitable distribution of basic resources. 

Under wealth equality, larger contributions imply greater marginal utility. But in reality, larger contributions often just imply greater wealth.

Consider these two example:

* Ten wealthy art patrons each contribute €1,000,000 to the local public art museum. 

  - Total Contributions: $10 \times €1{,}000{,}000 = €10{,}000{,}000$ 
  - QF allocates: $(10 \times \sqrt{1{,}000{,}000})^2 = €100{,}000{,}000$
  - Subsidy: €90,000,000

* One hundred lower‑income individuals each contribute €100 to replace lead pipes in their neighborhood

  - Total Contributions: $100 \times €100 = €10{,}000$ 
  - QF allocates: $(100 \times \sqrt{100})^2 = €1{,}000{,}000$
  - Subsidy: €990,000.

Intuitively, this seems wrong: the art museum receives a far larger subsidy, yet many more people benefit from replacing the lead pipes, and the utility-per-individual is arguably much higher as well.

## Free Subsidies

> ...once we account for the deficit, the QF mechanism does not yield efficiency.
> 
> —Buterin, Hitzig & Weyl[^1]


The optimality of QF assumes the subsidy that pays for the deficit is "free" to the contributors. But in reality the subsidy is usually indirectly paid for by contributors -- through increased taxes or the opportunity cost of that subsidy money not being spent on something else.

In the wealth equality section we show how wealthy contributors can disproportionately benefit from QF subsidies. So if it is the average citizen that is funding these subsidies through taxes, then QF can become a mechanism for transferring wealth from poor to rich. 

## Selfish Contributors

Ironically, if contributors are altruistic, QF can overfund projects, which can significantly *decrease* social welfare. 

When individuals make contributions for purely altruistic reasons, they don't directly experience the utility themselves. And yet the optimality of QF assumes that all utility is direct utility, benefiting the contributor only. Inefficiency of QF under altruistic motives is proven in Appendix A in [Connection-Oriented Cluster Matching Paper](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4311507)[^2].

To understand this problem, consider the following two scenarios:

**Selfish Scenario**

Three art patrons each contribute €1,000,000 to the local public art museum. Per the QF formula, total funding is $(3 \\times \\sqrt{1{,}000{,}000})^2 = €9{,}000{,}000$.

They each expect to experience €6,000,000 worth of individual utility from enjoying the additional €9,000,000 of art.

**Net social welfare**:

  - Total Utility  = 3 × €6,000,000 = €18,000,000
  - Total Funding  = €9,000,000
  - Social Welfare (Utility - Funding) = €9,000,000

**Altruistic Scenario**

A nearly identical scenario, but now it is three separate charities that each contribute €1,000,000 to a cancer research project. Again per the QF formula, total funding is $(3 \\times \\sqrt{1{,}000{,}000})^2 = €9{,}000{,}000$.

The charities base their funding decisions not on direct personal utility, but on *how many lives they expect to save*. Each charity knows that if they allocate funds efficiently, every €100,000 can save one life. And they estimate that €9,000,000 of funding for this particular project would save about 60 lives

**Net social welfare**:

  - Total Utility  = 60 × €100,000 = €6,000,000
  - Total Funding  = €9,000,000
  - Social Welfare (Utility - Funding) = –€3,000,000

So there has been a net **decrease** of social welfare.

These two scenarios look similar: same contribution amounts, same total funding amounts. And in both cases, each contributor sees €6,000,000 of "utility" for the €9,000,000 of funding. But in the selfish scenario, total utility is 3 times higher, because the utility is experienced independently by each contributor, whereas utility of saved lives is experienced only once by each of the cancer patients -- the three contributors don't each experience that utility (other than feeling good about those lives being saved, but that's not the kind of utility we're trying to maximize).

Of course if these organizations were trying to maximize social welfare, then they wouldn't contribute so much -- it would be better to stop contributing once the project reached the socially optimal funding level.

But of course, the purpose of using QF mechanism is to achieve the socially optimal funding level\! So for people just trying to maximize social welfare, participating in QF funding rounds as a contributor may not make sense.

## Equilibrium Discovery

QF assumes an **equilibrium** where each contributor picks the optimal contribution for themselves, given what everyone else is contributing. 

This means that in order to know how much they should contribute to a project through QF, people have to know how much other people are contributing. For example, suppose there's a small open source software project that really benefits me, but it already has €1,000,000 in total funding without my contribution: I might think that €1,000,000 is sufficient to build the software and thus there's little marginal utility in my contributing any more. On the other hand, if nobody else was contributing anything at all, and therefore the software wouldn't get built without my help, I might feel motivated to make a sizeable contribution.

But how do people know what everyone else is contributing? Well in reality, **they don't**, at least not before the funding round is over.

So how do they know that their contribution is optimal? They have no idea, actually.

This is perhaps the least understood issue with QF. I believe many people assume that because there is a theoretical equilibrium, contributors will necessarily **discover the equilibrium**.

But actually discovering the equilibrium requires either:

1.  [**Complete information**](https://en.wikipedia.org/wiki/Complete_information): In game theory, complete information is when everyone knows the utility functions of *every one else* (and knows that everyone knows this, etc.), and can use this knowledge to calculate the equilibrium. If everyone assumes that everyone else makes the same calculation and reasons the same way, then everyone will know how much everyone will contribute and thus how much they should contribute themselves. Obviously, this does not happen in any actual setting where QF has been used.

2.  **An equilibrium discovery process** where people iteratively adjust their contribution depending on what other people are contributing, until an equilibrium is reached. But unlike settings such as markets, where equilibrium is discovered naturally as a result of countless individual decisions and adjustments by buyers and sellers, there is **no natural process for realizing the equilibrium in QF**.

Without complete information or some equilibrium discovery process, contributors can only guess how much others will contribute based on signals such as popularity—leading to inefficiencies. See [“Quadratic Funding with Incomplete Information”](https://globalprioritiesinstitute.org/wp-content/uploads/Luis-V.-M.-Freitas-Wilfredo-L.-Maldonado-Quadratic-Funding-with-Incomplete-Information.pdf)[^3] for formal bounds on inefficiency.


## Sufficient Budget

Without enough subsidy to cover every project’s theoretical deficit, QF ceases to have one guaranteed best outcome and instead admits many equilibria—none of which reliably maximizes welfare.

So how do people choose how much to contribute? With a unique, socially optimal equilibrium, we at least theoretically have a situation (e.g. under complete information, wealth equality, etc.) where this equilibrium is discovered. If there are many equilibria, it's hard to even theorize about what will happen. People can only guess and hope. The results will almost certainly not be socially optimal.

To avoid the multiple-equilibria situation caused by funding caps, the entity organizing the QF must know what the equilibrium contributions will be beforehand or have virtually unlimited funds, so they can guarantee they have sufficient budget to subsidize the deficit.

If they do not have sufficient funds, then can use a generalization of QF called Capital Constrained Quadratic Funding, where they choose in advance a fraction of the deficit that they will subsidize. This of course sacrifices optimality, and the organizer still must be able to subsidize this fraction of the deficit, which means **they must still know in advance what the deficit will be**.

## Diminishing Returns

QF only works if contributors utility functions are 1) monotonically increasing -- any increase in funding results in increased utility -- and 2) concave -- decreasing marginal utility.

These assumptions don't hold, for example, for fixed-price projects. Using QF for a fixed-price project would require some coordination mechanism to ensure the actual project price gets funded -- no more, no less -- but even given this mechanism, there may be multiple equilibria -- different ways the costs of the project are divided among contributors. As with the case of budget caps, multiple equilibria result in unpredictable and suboptimal results.


## Perfect Knowledge

Another assumption required for optimality is that contributors have sufficient knowledge of projects.

Specifically, they must **know about all projects**, and know their utility functions for all of them. Otherwise, they obviously can't make optimal contributions.

Further, they must know how much funding each project has received via other funding mechanisms, because their optimal contribution depends on how much funding the project is receiving without their contribution.

In practice, because of lack of perfect knowledge, projects that are able to generate the most awareness (or hype) often receive the most contributions.

## Independent Agents

> ...if the size of this group is greater than 1/α and the group can perfectly coordinate, there is no limit (other than the budget) to how much it can steal.
>
> —Buterin, Hitzig & Weyl[^1]

The QF paper assumes each contributor acts as an independent rational agent: there is no coordination of any sort -- no bribery or other form of collusion is possible.  And of course the authors are frank about what happens if this is not the case.

The problems of collusion and fraud are probably the most well-understood problem with QF, and have been been written about extensively. Many platforms have experienced with collusion-resistant variants of QF and other methods of addressing these problems. I won't go into more detail here.

## Conclusion

QF is only socially optimal if all the above assumptions hold. But social optimality is a high bar. If QF falls short of achieving the absolute maximum theoretically possible social welfare, is it still pretty good?

If many of these assumptions fail, then results will probably be very far from optimal. While QF might still offer advantages over simpler mechanisms in certain contexts, there's an emerging consensus that without at least mitigating against collusion, its practical benefits are probably limited.

Some work has been done on collusion resistant variants of QF, such as [Connection-Oriented Cluster Matching](https://www.google.com/search?q=\(https://papers.ssrn.com/sol3/papers.cfm%3Fabstract_id%3D4311507\)). COCM also addresses the fact that contributors are not always selfish. But these improvements come at the cost of the single-equilibrium and theoretical optimality properties of pure QF. It's unclear how to evaluate the net benefits of such variants. 

I am not aware of work on variants of QF designed to address the other assumptions described in this paper: wealth inequality, costly subsidies, failure to realize the equilibrium, and imperfect knowledge of projects.

Unless at least some of these assumptions hold, I suspect that there are probably mechanisms for public goods funding that produce better outcomes than pure QF. Here's a great [overview of the public goods funding landscape](https://splittinginfinity.substack.com/p/the-public-goods-funding-landscape) and [List of papers on public goods funding mechanisms](https://harsimony.wordpress.com/2022/02/10/list-of-public-goods-funding-mechanisms/) by Sam Harmsimony.



[^1]:
    Vitalik Buterin, Zoë Hitzig & E. Glen Weyl. "Cardinal Voting and Quadratic Voting." *SSRN*, 2017.

[^2]:
    Miller, Joel and Weyl, Eric Glen and Erichsen, Leon, Beyond Collusion Resistance: Leveraging Social Information for Plural Funding and Voting (December 24, 2022). Available at SSRN: https://ssrn.com/abstract=4311507 or http://dx.doi.org/10.2139/ssrn.4311507

[^3]:
    Freitas, L.M., Maldonado, W.L. Quadratic funding with incomplete information. Soc Choice Welf 64, 43–67 (2024). https://doi.org/10.1007/s00355-024-01512-7