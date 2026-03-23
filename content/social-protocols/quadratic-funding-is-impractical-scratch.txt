
Social welfare is impossible to accurately measure. If we could measure it, the clash between theory and empirical reality would be obvious. But in the absence of unambiguous empirical evidence that QF *doesn't* increase social welfare, people have continued to use QF based on the hope that the theory approximates reality, and that QF, if implemented correctly, is approximately optimal -- not realizing that it is, theoretically, far from optimal.

I believe this will ring true to many people who have been involved with QF funding efforts like gitcoin, giveth (other examples), and have the gut feeling that the funds aren't being distributed in a way that optimizes social welfare. I attended the Schelling Point conference in Denver last week, and the questions about the optimality of quadratic funds and methods for evaluating impact are definitely in the zeitgeist. But a lot of work remains to develop variants of QF, or other mechanisms, that increase social welfare given a more realistic set of assumptions.

The following is a list of the assumptions beind QF. In the following suctions, I discuss how far from optimal the results may be when these assumptions don't hold.


## Deficits are Free

The optimality of QF assumes that contributors don't care about deficits. But as the original QF paper states "...once we account for the deficit, the QF mechanism does not yield efficiency..."

The money to fund deficits come from *somewhere*. If the deficit are paid by a charitable contribution, there is an opportunity cost for contributors -- that money is no longer available to fund other charitable projects that may benefit them. If deficits are paid by the government, then contributors will  may may ultimately pay for the deficits with decreased spending on other public goods or increased future taxes. 

If the tax burden is spread out among millions of people, it might feel like the deficit spending is essentially free. But consider the above case of the $90,000,000 in subsidies to the public art museum. This creates a lot of dollar-valued utility for some very wealth art patrons. But the cost is paid by the average taxpayer. Depending on the progressiveness of the tax scheme, the average taxpayer may be relatively poor. Quadratic funding combined with wealth inequality and a regressive taxes results can result in an effective mechanism for transferring wealth from poor people to rich people. 


## Individuals Act Independently

Collusion and fraud are probably the most well-understood problem with QF. In the original paper, the authors write:

> ...if the size of this group is greater than $`1/α`$ and the group can perfectly coordinate, there is no limit (other than the budget) to how much it can steal."
> 
> - Buterin, Hitzig, Weyl in the original quadratic funding paper [^1]

There has been some good research on collusion-resistant variants of QF such as connection-oriented cluster matching (COCM) [reference]. You probably should not even think about using a variant of QF that is not collusion-resistant.

Unfortunately, collusion-resistance necessary sacrifices optimality as well as the unique equilibrium -- two of the main reasons for using QF in the first place.

### Suboptimality of COCM

The basic idea behind collusion-resistant mechanisms such as COCM is *grouping* users and treating entire groups as single individuals. But this undermines much of the benefit of QF. It not only considerably decreases optimality, it can results in multiple equilibria -- which is problematic. And in the worst case it can remove the incentive for anyone to contribute at all.

To illustrate, consider a group of 100 similar residents of a neighborhood plagued by crime. They all have identical utility functions valuing public goods related to public safety: improved street lighting, increased police funding, etc. Suppose under QF, each resident contributes $10 to improved street lighting. If there is no actual collusion, then the funding amount will be $`(100*√10)^2 = \$100,000`$. This amount theoretically creates a lot of social welfare because, although it might not create $100,000 worth of utility to any single resident, it creates more than $1,000 of utility for each of the 100 residents (and how do you know that is the case? The QF paper has an elegant proof).

But if they are all treated as a single group under COCM, then the funding amount will be $`(\sqrt(100×10))^2 = \$1,000`$, which is just the sum of the individual contributions. So there will be no subsidy -- the mechanism just reduces to private contributions. 

And because contributors supposedly know how the mechanism works and are making contributions that maximize their utility, they would *not* each contribute $10. Rather, the *group*, if acting as a unit, would contribute some optimal amount. *But there will be an infinite number of ways that the burden of the contribution could be shared among group members*. So *there is longer a single equilibrium*. 

Further, as is the case for public goods funded by private, individual contributions, the possible equilibria may include one where nobody contributes anything. And some cases, this will be the only equilibrium.

Now as argued in the COCM paper, the very interconnectedness that makes this group of contributors have similar utility also probably 



Let's be more precise. Suppose the utility function for each contributor is 20*sqrt(F). Under regular QF, each voter's optimal contribution can be found where the derivative of this function is equal to 1, which happens to be at F=10. The group's utility is (20*100)sqrt(F), and the group's optimal contribution is F=1,000.  

However, if there is in fact no collusion, the equilibrium contribution for each individual is zero. To see this, suppose everybody contributed $10. The utility for each individual is 2*sqrt(1000) - 10 = 53.25. But if  one of the user decides not to contribute, their utility becomes 2*sqrt(990) = 62.93. So they are better off not contributing. This is of course the same for everyone.

Of course 


TODO: no longer have incentive to contribute....

### Loss of Single Equilibrium in COCM

One of the under-appreciated theoretical qualities of QF. It's not just that it's theoretically optimal. It also solves the "free-rider problem" by creating a unique equilibrium.

Under private contributions, if many people have the same utility function, then 


Users can't hope that others will contribute such that they don't have to. If other people contribute *more*, my equilibrium contribution will be less.

it has a unique equilibrium! Given everybody acts rationally and contributes an amount that maximizes their utility given what other people are contributing, there is only one possible outcome!

There are many ways of breaking this equilibrium. And when this happens, it's very difficult to analyze or predict how this will play out. Individuals will hope to "free-ride" and will contribute (or not) based on what they expect others to contribute, which depends and what they expect other expect of them, and so on. It becomes a very kind of ultimatum game, with no nice game-theoretical solution concept.

## Contributors Can Find the Equilibrium

But unfortunately, even when there is a unique equilibrium, the assumption that *contributors can discover the equilibrium* is implausible.

In equilibrium, each individual's optimal contribution *depends on what other individuals are contributing*. So how do individuals know how much others are going to contribute? Well in reality, *they don't*: this assumption simply doesn't plausibly hold in most settings where QF has been implemented. 

In order for this equilibrium to actually be realized, one of two things have to be the case:

1. There is an equilibrium discovery process. Such a process, also called *tatonnement*, are how market prices reach equilibrium: agents iteratively adjust their behaviors depending on what other people are doing, until an equilibrium is reached. But in a QF funding round, there is no "market" -- it is a one shot thing. Voters decide much they are going to contribute and contribute. To my knowledge, no QF funding provides a process for equilibrium discovery, where voters can adjust their provisional contributions in response to others's contributions. And such a process would actually not work very well: it would require a lot of time and effort from contributors and would be susceptible to timing-based manipulation. Using a mechanism where voters report their *utility function* and the mechanism automatically chooses a contribution on behalf of a contributor might be more practical. 

2. Voters can also theoretically discover the equilibrium if utility functions are common knowledge (the [complete information](https://en.wikipedia.org/wiki/Complete_information) assumption). Every contributor knows the utility functions of *every other contributor*, and can use this knowledge to independently calculate the equilibrium contributions of everyone else, essentially simulating the above *tatonnement* process. But this is a complex calculation that realistically would have to be done by a computer.

If voters aren't able to discover the equilibrium contribution, what do they do? They must somehow guess how much others will contribute. For example, when a project has a certain amount of hype, this might be a signal that a lot of people will contribute. 

What are the consequences for an individual of contributing an amount that does not maximize their utility, given what others are contributing? The following chart can give us an idea. It shows, for a contributor with the arbitrary utility function 10*F^.7, the optimal contribution as a function of the T, which is sum of square roots of all other voters' contributions. If T=0 (nobody else contributes), the voter maximizes their utility by contributing 26.83. But if T=51.8 (meaning total funding F=T²=2683), the optimal contribution is only 4.12.

On the other hand, the consequences of making a non-optimal contribution may or may not be all that great. The following chart shows the net utility for the same voter in the scenario that they contributed a fixed amount of 26.83, regardless of what others contribute, vs their net utility when they make the optimal contribution. There is actually very little difference. A paper on [Quadratic Funding with Incomplete Information](https://globalprioritiesinstitute.org/wp-content/uploads/Luis-V.-M.-Freitas-Wilfredo-L.-Maldonado-Quadratic-Funding-with-Incomplete-Information.pdf) makes some more thorough estimates of the inefficiency of QF in the absence of perfect information in a variety of settings.

## The Organizer Knows What The Deficit Will Be

Funding caps break quadratic funding. There must be sufficient funds to subsidize the deficit, or there is no longer a unique equilibrium.

For example, suppose there are three individuals who each contribute $100. Per the QF formula, the total funding will be $`(3√100)^2 = 900`$. This requires a subsidy of $`\900-3×100 = 600`$.

But suppose there is a subsidy cap of $200. Now, any of the three individuals would be better withdrawing their contribution and free-riding on the contribution of the other two. With just two $100 contributions, the funding amount will $`(2√100)^2 = 400`$, using up the entire subsidy budget of $`400 - 2×200 = 200`$. 

So then what happens? Who contributes, and how much? Unfortunately, there is no longer a single equilibrium outcome. 

If one of the three individuals has some reason to *expect* that two other individuals will contribute $100 each, for some reason, then they won't contribute. But why would they expect this?

What actually might happen is beyond the scope of this article. But what definitely *won't* happen is the optimal outcome.




To avoid funding caps, the entity organizing the QF funding round knows approximately what the equilibrium contributions will be beforehand. Knowing this, they must ensure they have sufficient money set aside to subsidize the deficit. If they do not have sufficient funds, then can use a generalization of QF called Capital Constrained Quadratic Funding, where they choose *in advance* a fraction of the deficit that they will subsidize. But they must then be able to subsidize this fraction of the deficit.

What happens if the operator cannot do so? Then there is a **subsidy cap**. And when this happens, there is *no longer a single equilibrium*, and the elegant theory behind QF collapses into chaos.


- EITHER: The organizer knows in advance what everyone will contribute and has sufficient funds to cover the deficit 
- OR: the organizer has unlimited budget (e.g. the federal government)



- Contributor Have a Lot of Knowledge
  - Knowledge About Other Contributors: 
    - Everybody Knows what Everybody Else Will Contribute: Otherwise, they cannot know what their own optimal contribution is at equilibrium.
  - Knowledge of Alternative Project Funding Sources
    - Contributors must know the *marginal utility* of contributing to a project, but projects have diminishing returns, so this requires knowing *total funding levels* from alternative sources.
  - Complete Knowledge of All Possible Projects
    - QF assumes that all members of society *know* how much utility they will obtain from each project. But in reality, for most projects, most people don't even know the project exists, let alone how much it might benefit them personally. So QF rounds end up being akin to popularity contests. The amount of funding a project receives scales not with the number of people that will benefit from the public good (the point of QF), but with the number of people that know about the project and the opportunity to contribute the quadratic funding round. Overall social welfare is *not* theoretically maximized in these settings. 
- Operator has a lot of knowledge
    - The Operator Knows what Everybody will Contribute (or has unlimited deficit-spending ability): Otherwise, subsidies are capped, and there is no longer a single equilibrium.
- Contributors have sufficient funds
- Operator has sufficient funds.
- Projects are not fixed-cost. 

  Treating a group of people with similar contribution patters as a single person 

- Projects can't contribute to themselves.
- Diminishing returns on projects
- Rationality: This is not really a problematic assumption. Economists don't mean "rational" in the way we mean it colloquially. Basically it just means that people respond to incentives: if you design a mechanism that lets people get free money, then they will use it to get free money. This assumption *does* seem to closely match empirical reality.





---


Quadratic Funding is optimal in idealized settings. More specifically, it theoretically maximizes the overall dollar-valued utility gain for society, given certain assumptions about the participants and the implementation. 

But when these assumptions don't hold, how optimal is the outcome? In many settings where quadratic funding is implemented, a different set of assumptions holds. And given these assumptions, the quadratic funding mechanism is theoretically not optimal.

For example, we know that agents are not *atomic*: individuals don't act in isolation, and groups will be incentivized to cooperate in mutually-beneficial ways. Given this assumption, quadratic funding will theoretically produce results that are far from optimal. 

The particular problem of collusion is [well-known] and work is being done on collusion-resistant QF. But there other assumptions that probably don't plausibly hold in most settings. And when they don't hold, the results are also, theoretically, less than optimal. 

In this series of articles, I discuss about these other assumptions, and how much social welfare is lost when they don't hold.




----



There is tricky free-riding problem. 


Some voters would not contribute at all depending on how much they expect other people to contribute. So there's a free-riding problem, a game of expectations.


....





  And I am not aware of any research showing what kind of theoretical properties these new mechanisms have. 


## When Matching POol is Finite

## Different Wealth Levels
  dollar-valued utility
  
## Can't prove humanity
  any organization that receives fund bernefits from contributing to themselves
    becuase utility is linaer

# When citizens care about deficit

  from original paper: "This analysis suggests that once we account for the deficit, the QF mechanism does not yield
efficiency.""

# IF contirbutors dont' have equal information about projects




## Common Knowledge: 

### Everybody Knows what Everybody Else Will Contribute

In QF funding round there is a theoretically *equilibrium* where each individual contributes the amount that maximizes their own utility *given what everybody else is contributing*. So their contribution *depends on what other individuals are contributing*.

So how do individuals know how much others are going to contribute? Well in reality, *they don't*: this assumption simply doesn't plausibly hold in most settings where QF has been implemented. 

In order for this equilibrium to actually be realized, one of two things have to be the case:

1. There is an equilibrium discovery process, also called *tatonnement*, where voters iteratively adjust contribution depending on what other people are contributing, until an equilibrium is reached. To my knowledge, QF funding tools don't provide tools for this process. And it would require a lot of time and effort from contributors and would be susceptible to timing-based manipulation. Using "virtual agents" that do this automatically on behalf of a contributor might be more practical. 
2. Utility functions are common knowledge (the [complete information](https://en.wikipedia.org/wiki/Complete_information) assumption). Every contributor knows the utility functions of *every other contributor*, and can use this knowledge to independently calculate the equilibrium contributions of everyone else, essentially simulating the above *tatonnement* process. But this is a complex calculation that realistically would have to be done by a computer.

If voters aren't able to discover the equilibrium contribution, what do they do? They must somehow guess how much others will contribute. For example, when a project has a certain amount of hype, this might be a signal that a lot of people will contribute. 

What are the consequences for an individual of contributing an amount that does not maximize their utility, given what others are contributing? The following chart can give us an idea. It shows, for a contributor with the arbitrary utility function 10*F^.7, the optimal contribution as a function of the T, which is sum of square roots of all other voters' contributions. If T=0 (nobody else contributes), the voter maximizes their utility by contributing 26.83. But if T=51.8 (meaning total funding F=T²=2683), the optimal contribution is only 4.12.

On the other hand, the consequences of making a non-optimal contribution may or may not be all that great. The following chart shows the net utility for the same voter in the scenario that they contributed a fixed amount of 26.83, regardless of what others contribute, vs their net utility when they make the optimal contribution. There is actually very little difference. A paper on [Quadratic Funding with Incomplete Information](https://globalprioritiesinstitute.org/wp-content/uploads/Luis-V.-M.-Freitas-Wilfredo-L.-Maldonado-Quadratic-Funding-with-Incomplete-Information.pdf) makes some more thorough estimates of the inefficiency of QF in the absence of perfect information in a variety of settings.

### The Operator Knows what Everybody will Contribute

Funding caps break quadratic funding. There must be sufficient funds to subsidize the deficit (the difference between what contributors pay in and funding that is paid out), whatever it is.

This implies that the person running the QF program (the operator) knows approximately what the equilibrium contributions will be beforehand. Knowing this, they must ensure they have sufficient money set aside to subsidize the deficit. 

If they do not have sufficient funds, then can use a generalization of QF called Capital Constrained Quadratic Funding, where they choose *in advance* a fraction of the deficit that they will subsidize. But they must then be able to subsidize this fraction of the deficit.

What happens if the operator cannot do so? Then there is a **subsidy cap**. And when this happens, there is *no longer a single equilibrium*, and the elegant theory behind QF collapses into chaos.

For example, suppose there are three individuals who each contribute $100. Per the QF formula, the total funding will be $`(3√100)^2 = 900`$. This requires a subsidy of $`\900-3×100 = 600`$.

But suppose there is a subsidy cap of $200. Now, any of the three individuals would be better withdrawing their contribution and free-riding on the contribution of the other two. With just two $100 contributions, the funding amount will $`(2√100)^2 = 400`$, using up the entire subsidy budget of $`400 - 2×200 = 200`$. 

So then what happens? Who contributes, and how much? If one of the three individuals has some reason to *expect* that two other individuals will contribute $100 each, for some reason, then they won't contribute. But why would they expect this?

What actually might happen is beyond the scope of this article. But what definitely *won't* happen is the optimal outcome.

## Altruistically Motivated Contributions

The entity funding the deficit is, presumably, just trying to maximize social welfare. But if the individual contributors are also just trying to maximize social welfare then, ironically, social welfare won't be maximized.

When individuals make contributions for purely altruistic reasons, they don't experience the utility themselves. And yet the logic of the QF mechanism assumes that they have. This results in utility being "double counted", which ultimately results in over-allocation and *net decrease in social welfare*.

To understand this, let's consider an example. First, let's consider a non-altruistic example. Suppose three wealthy art patrons each contribute $1,000,000 to the local public art museum via quadratic funding. Their contributions are *not* altruistic: they each expect to experience $6,000,000 worth of utility from enjoying the additional $9,000,000 of art. 

The total funding is per the quadratic funding formula is `$( 3*\sqrt{1,000,000} )^2 = $9,000,000`$.  So the net social welfare created is:


$$
  \text(Social Welfare)
  = \text{Total Utility} - \text{Total Funding}
  = 3 × $6,000,000 - $9,000,000 = $9,000,000
$$

Now imagine a nearly identical scenario, but the contributors are charities funding a cancer research project. We use the same utility functions, but their these are now a function of *how many lives they expect to save*. Each of them knows that if they allocate funds efficiently, every $100,000 can save 1 life, and they estimate $9,000,000 of funding for this project would save about 60 lives. Thus their "utility" is $100,000 x 60 = $6,000,000.

So the total social welfare created is:

$$
  \text(Social Welfare)
  \text{Total Social Utility} - \text{Total Funding}
  = $6,000,000 - $9,000,000 = -$3,000,000
$$

What happened here? We went from $9,000,000 of social welfare created in the first scenario to $3,000,000 of social welfare destroyed in the second scenario! But the utility functions are the same?

The difference, of course, is that in the second scenario we didn't multiply the $6,000,000 of utility by three. Each of the charities did not independently experience $6,000,000 worth of "utility".

Now of course since these organizations are trying to maximize social welfare, they would not each contribute $1,000,000. Instead, they would determine the funding amount that maximized social welfare, and would stop contributing once this amount was reached. 



## Different Wealth Levels



## Fixed-Price Projects


## Contributors Don't Know What Others Will Contribute


## When Matching POol is Finite

## Different Wealth Levels
  dollar-valued utility
  
## Can't prove humanity
  any organization that receives fund bernefits from contributing to themselves
    becuase utility is linaer

# When citizens care about deficit

  from original paper: "This analysis suggests that once we account for the deficit, the QF mechanism does not yield
efficiency.""

# IF contirbutors dont' have equal information about projects


Givith does the thing where they do percent of pool:
  https://docs.giveth.io/quadraticfunding











CHART

. The following chart shows the *individual efficiency* of the same user as 



, and just have to make a wild guess. Often, the projects that get funded are those that have a certain amount of hype or dedicated following

 Worst, when a project has a large group of supporters, the

So in reality, rather than contributing the optimal amount, contributors make a guess



So they have to guess, but they have little basis for the guess, and the implications of guessing wrong are significant.

For example, suppose an individual has utility in the form:
  
  FUNCTION

If there are no other contributors, then my optimal contribution amount will be $100. But what if there is 1 other person who also is contributing $100? What if there are 100 other people each contributing $100? 

We can compute their optimal contribution amount as a function of the sum of square roots of other contributions. For this particular example, the function looks like this:







If they were the only contributor, then the optimal contribution would be $100. 


To understand this, consider this scenario. There is some public good, and I know that if I had to fund it all by myself, I would only be willing to contribute $100. This implies that derivative of my concave utility function for that public good is equal to 1 when the funding amount is $100.

  V'($100) = 1

Under quadratic funding, *if I know that there are no other contributors*, then my optimal contribution amount will also be $100. But what if there is 1 other person who also is contributing $100? What if there are 100 other people each contributing $100?

Probably, my optimal contribution will be very different.



  U(F) = beta * ln(1+F)
  U'(F) = beta / (1+F) = beta / (1 + (T + c)^2)
  dF/dc = (T + c)/sqrt(c)
  dU/dc = U'(F) * dF/dC = beta / (1 + (T + c)^2) * ( (T + c)/sqrt(c) ) = beta * (T + c) / ( (1 + (T + c)^2) * sqrt(c) )

  beta / ( sqrt(c) * (T + c) ) = 1

  log(beta) - 1/2 log(c) - log(T- + c) = 0
  
  beta = 200

  sqrt(c) * (200 + c) = 1



  U(F) = beta * F^(1 - eta) / (1 - eta)
  U'(F) = beta / F^eta
  dF/dc = (T + sqrt(c))/sqrt(c)
  dU/dc = U'(F) dF/dC = beta / (T + sqrt(c))^(2*eta) * (T + sqrt(c))/sqrt(c) = ( beta * (T + sqrt(c))^(1 - 2*eta) ) / sqrt(c)

  ( beta * (T + sqrt(c))^(1 - 2*eta) ) / sqrt(c) = 1


  # and we know beta = c0^eta
  # so solve for 
  # c0^eta*(T+sqrt(c1))^(1-2eta)/sqrt(c1) = 1
  # eta = ( 1/2*log(c1) - log(T1 + sqrt(c1)) ) / ( log(c0) - 2*log(T1 + sqrt(c1))) 


beta = T1^(2*eta - 1)

Let's look at the implications of this. If we know an individual's utility function for some public good, we can compute their optimal contribution amount as a function of other users contributions.

Let's set up a scenario and look at how much utility is lost if the user


Let's define the following

  T: the sum of square roots of other individual's contributions.
  c: the individual's contribution
  F: = (T + c)^2: the total funding amount for the public good
  V(F): the individual's utility function for the public good 

The utility function should be a smooth, monotone, concave function that is zero when F is zero. V(F) = log(F+1) fits the bill. 

So the user wants to maximizes:   

    V( (T + c)^2 ) - c





# Participatory Budgeting and Public Goods Funding

I have spent the last couple of months studying mechanisms for **participatory budgeting** and **public goods funding**. Here I describe some of what I've learned. The intended audience for this post is somebody like me of 2 months ago.

## 1. There Has Been A Lot Of Research In This Space

There is an extensive amount of academic literature related to public goods funding and the social choice problem of participatory budgeting. Some starting points:

- [Overview of the public goods funding landscape](https://splittinginfinity.substack.com/p/the-public-goods-funding-landscape) by Sam Harmsimony. 
- [List of papers on public goods funding mechanisms](https://harsimony.wordpress.com/2022/02/10/list-of-public-goods-funding-mechanisms/) also by Sam Harmsimony. 


## 2. Public Goods Don't Have to be Literally Public

Next, mechanisms for public goods funding can actually be applied to many *shared good*: from actually public goods such as open source software projects, and neighborhood parks, to private but shared goods like an HOA's private community pool, or the assets held by a DAO.

## Participatory Budgeting vs. Public Goods Funding.

Second, both "participatory budgeting" and "public goods funding" refer to mechanisms for deciding how much money goes to some public good. But some public goods funding mechanisms involve *voluntarily contributions*, whereas *participatory budgeting* usually refers generally to processes where individuals don't contribute funds, they just vote on how the organization (a government, DAO, HOA, charity, etc) spends its money.

And some mechanisms use *both* voluntary individual contributions and shared organizational funds.

So there are three families of public goods funding mechanisms. And I've found that the mechanisms in the the distinct categories are quite different. 

   
    |----individual contributions----|

                         
                        |------------shared funds-----------|


    |--------CCM--------|---QF, VCG---|---majority voting---|


### The "Free Rider" Problem

Many public goods that are worth funding don't get funded, because no single individual benefits enough to fund it themselves. For example, a public park might create more value to the community than it would cost, but parks are too expensive for any one person to build. That's the dilemma.

But that's what taxes are for, right? Indeed, taxation might be the oldest solution to the public goods funding dilemma. 

People can also form coalitions with conditional commitments: each member contributes $1,000 for the park if at least 1,000 other people did (with some mechanism to enforce honoring commitments).

But sometimes people just voluntarily and unconditionally donate to public goods: massive cathedrals have been funded by contributions from their congregations. People collectively donate huge amounts to charities that fund public goods. 

### Altruism and Reputation


Indeed it seems to me that most of the literature on public goods funding seems to ignores two of the main incentives for contributing to public good: charity, and social pressure. They lump both of these under *utility*, but I think that the incentive of **intrinsic utility** (my enjoyment of the park) must be treated separately from the **altruistic utility** (the good feelings I get know that other people are enjoying the park I built) and **reputational utility**.

### Theoretical Optimality

th theoretically optimal2 and 

But neither of these families of mechanisms guarantee money is spent in an optimal way -- that is, they don't maximize the total value created for the community. So that's why people have come up with "hybrid" mechanisms involving both individual contributions, such as quadratic funding and Vickrey–Clarke–Groves.

Some of these mechanisms are claimed to be *optimal* theoretically -- they result in *the exact right amount* of money being funded to each public good. But this is only the case under some very, very narrow assumptions. As I'll discuss below, some of these assumptions probably do not hold in many real-world applications.






Another is just waiting for rich person to pay for it. Another age-old mechanism, I would think, is conditional commitment: 



Funding of public goods is a classic collective action problem. With private goods, individuals buy something if its value to them is greater than the cost. For example, I might spend $1,000 on a new computer if it is worth at least $1,000 to me.

Now it may be that a new public park down the street would also be worth $1,000 to me. But parks cost more than $1,000. Suppose a new park would cost $1,000,000 to build. Probably, no single individual values the park at $1,000,000, and so it doesn't get built via voluntary contributions. But if there are at least 1,000 people to which the park would be worth at least $1,000, they would benefit if they somehow coordinated to collectively fund the park. 

This is the dilemma. How does the park get funded? 

There are obvious solutions of course. A government or HOA could tax the citizens/members, and use some process for deciding how to spend the money. Such mechanisms fall in the pure "shared funds" area of the above diagram. Or people could get together voluntarily and *conditionally commit* to contributing $1,000 for the park if at least 1,000 other people did (with some mechanism to enforce honoring commitments, such as escrow). These mechanisms fall under the pure "individual contributions" area of the above diagram.

But neither of these families of mechanisms guarantee money is spent in an optimal way -- that is, they don't maximize the total value created for the community. So that's why people have come up with "hybrid" mechanisms involving both individual contributions, such as quadratic funding and Vickrey–Clarke–Groves.

Some of these mechanisms are claimed to be *optimal* theoretically -- they result in *the exact right amount* of money being funded to each public good. But this is only the case under some very, very narrow assumptions. As I'll discuss below, some of these assumptions probably do not hold in many real-world applications.


### Quadratic Funding and Vickrey–Clarke–Groves are Similar

One surprising thing I learned is that Quadratic Funding, which is quite new and hot, is actually very similar to the Vickrey–Clarke–Groves mechanism, which dates from the 60s. 

Both are theoretical optimal given certain assumptions: that is, they both result in a funding amount that maximizes social welfare. Which implies that they produce the *exact same funding amount*! They only differ in how much each individual ends up paying, and how much of a deficit there is (VCG has a lesser deficit).

But I have come to the conclusion that VCG is the superior mechanism, because QF makes the highly implausible assumption of complete information, whereas VCG is strategyproof. I'll write about this in another essay.

Unfortunately, both VCG and QF are highly vulnerable to collusion. 

Suppose I contribute $100 to my favorite project. If I am the only contributor, the final funding under QF would be exactly $` (√100)^2 = \$100 `$. But if I give $1 to each of 100 people and convince them to contribute on my behalf, then the total funding would be $` (100*√1)^2 = \$10,000 `$! $9,900 of that would be a deficit paid for from shared funds. This is clearly not acceptable.


## Participatory Budgeting with Fixed Budget

When there are no individual contributions, and public goods are funded through shared funds (government, HOA, DAO, etc), then what remains is a *social choice function* about how to spend on goods.

In the sample case when there is only one possible good to be funded, and a group needs to decide how much to allocate, there is one pretty good solution. Everybody proposes an amount (within the budget constraint), and the median is chosen. This mechanism is simple and strategyproof (voters have no incentive to lie), pareto efficient. It does not maximize social welfare, but there is no mechanism that does without transfers or individual contributions (like VCG or QF).

Unfortunately, when there are multiple goods to be funded and *fixed or finite budgets* make the problem way, way more complicated. It becomes a **multi-dimensional** social choice mechanism, and there are all sorts of pesky impossibility theorems in the game theoretical literature telling us that such mechanisms can't be strategyproof without being highly limited in some way. 

## Negotiation

However, I did a bunch of simulations and learned that....









# Vickrey–Clarke–Groves is superior than Quadratic Funding


### Quadratic Funding and Vickrey–Clarke–Groves are Similar

Quadratic Funding, which is quite new, is actually very similar to the Vickrey–Clarke–Groves mechanism, which dates from the 60s. It too a while for me to realize how similar they are.

Both involve individual contributions with the deficit coming from shared funds. Neither can be used for goods with fixed costs: goods get funded at some level between 0 and infinity, depending on the choices of the individual contributors. Both mechanisms make the same assumptions about how much utility individuals receive from projects: they assume any amount of funding is valuable to contributors, and that the value grows as funding increases, but with diminishing returns (technically, quasilinear preference with smooth and concave utility).

They also assume that there is no collusion or side payments -- people make a decision about how much to contribute without paying each other to make contributions on their behalf. This is an important assumption, because it doesn't hold in the real world.

Both are theoretical optimal given these and some other assumptions I will discuss below: that is, they both result in a funding amount that maximizes social welfare. Which implies that they produce the *exact same funding amount*! They only differ in how much each individual ends up paying, and how much of a deficit there is (VCG has a lesser deficit). But even individual payments are generally quite similar. In fact for a certain class of utility functions (functions in the form w*sqrt(x)), the results are are completely identical.

### The Biggest Problem with QF

But QF makes more assumptions than VCG. The most problematic assumption is that each individual can calculate their own optimal contribution. But the optimal contribution for each individual depends on the contributions *of all other individuals*. But how do individuals know what other individuals are going to contribute! 

Theoretically, a rational individual can figure this out **if they know the utility functions of all other users**. They then must use some calculus to calculate the funding level that maximizes overall utility and then calculate their own contribution.  They must also believe that all other contributors have done the same calculation, and come to the same conclusion.

In reality, this is not going to happen.

### Equilibrium Discovery Processes

One might consider a solution where voters use some online system where they enter provisional contributions, and adjust these based on the provisional contributions of others. At some point, an equilibrium is eventually reached, at which point provisional contributions become commitments. But there are practical issues here regarding timing: people need to wait for others to update their contribution, so the process can take an indefinite amount of time, and if there is a time limit, people can lie about provisional contributions, manipulate results by waiting for the last minute to enter their true contributions.

### Dynamic Quadratic Funding and VCG

Consider a variant of the quadratic funding mechanism, where instead of deciding how much to contribute, voters instead reported a *utility function* to the mechanism, as well as some maximum contribution amount (held in escrow). The mechanism then calculates the optimal funding amount and the contributions of each individual. 

Voters would not need to know anything about the utility of other users, nor would they need to do any calculations. 

Such a mechanism, which I call "Dynamic Quadratic Funding" (DQF), is almost identical to VCG. They produce identical funding amounts. They only differ in how contributions are broken down between individuals. 

### Criticisms of VCG

One of the main criticisms of VCG has always been that requiring voters to specify their entire utility function is asking too much from them. But I think eliciting utility functions is far more practical than requiring participants to calculate their own contribution. Instead of expecting voters to know the utility functions of every member of society, it only expects them to know their own. And instead of expecting them to calculate the equilibrium, the mechanism calculates it for them.

Expecting voters to specify their utility functions seems plausible. These mechanisms already assumes voters utility functions take pretty limited forms. Many plausible utility functions fit into a few "families" such as "isoelastic" utility functions of the form $`w×x^β`$. These can be fully specified using just two parameters. A UI could help users estimate these by soliciting a few datapoints about how much they would be willing to contribute at different funding levels.

By asking voters for their full utility functions, VCG becomes **strategyproof**. That is, no matter what other individuals do, they maximize their utility by reporting their true utility function to the mechanism. This is a hugely desirable property.


### Collusion


### Links

A Budget-Balanced Incentive-Compatible Schema for Social Choice: https://www.researchgate.net/publication/37441953_A_Budget-Balanced_Incentive-Compatible_Scheme_for_Social_Choice

List of Public Goods Funding Mechanisms: https://harsimony.wordpress.com/2022/02/10/list-of-public-goods-funding-mechanisms/

Learn about second-best mechanisms: https://cheaptalk.org/jeffs-intermediate-micro-course/

# Free Money

# Alpha

----



When there is no fixed budget -- e.g. when a government can borrow and spend as much as it wants -- then its just a question of agreeing


When it's a question of simply agreeing on a number, between 0 and infinity, about how 



In other words, the problem of how to divide a finite sum of money between m competing projects creates 



 participatory budgeting

One of the most When there is a fixed or finite budget


Participatory budgeting problem is 



Pure participatory budet




TO ADD: instead of expecting voters to know the utility functions of every member of society, we only expect them to know their own. And instead of expecting them to calculate the equilibrium, the mechanism calculates it for them.


https://forum.effectivealtruism.org/posts/kHDjtqSiSohZAQyjG/some-thoughts-on-quadratic-funding
mechanisms to fund open source: https://www.matthewthom.as/blog/fund-open-source/
  Remember that there is no incentive compatible efficient mechanism that collects more revenue than VCG.

  There is no incentive compatible efficient mechanism that raises more total funds (Krishna and Perry 1998).

criticizded



The difference between QF and VCG is that with QF, voters simply choose some amount to contribute, whereas with VCG, voters report their *utility function* to the mechanism, and then the mechanisms tells them how much they have to pay.




### The Biggest Problem with Quadratic Funding


The biggest problem with quadratic funding is that it assume that people can figure out how much to pay in order to maxize




 *specific contribution amount*, and with VCG, voters




They are also *individually rational*, meaning people will voluntarily participate.


This was quite a surprise to me. 



Both also assume that there is no *collusion* or side payments -- people make a decision about how much to contribute without coordinating with other people or paying each other to make contributions on their behalf. 






-----


U = 2000 sqrt(x) its worth paying 1,000,000
  b^2/4 = 4000000/4 = 1000000


U = bsqrt(F)
b/2sqrt(F) = 1/m
b2m2/4 = F

m = 1 if b = 2, F=1
m = 100 and b=2, F = 10,000
m = 1000 and b=2, F = 1,000,000









they would all benefit. But if only 100 people contributed 1000, 

if enough people contributed to fund some public good, everyone would benefit. TODO: 





But nobody contributes, because individually they would be better of if everybody else contributed.



### The Fixed Budget Allocation Problem



### charitible vs. utilitarian contributions


Participatory budgeting is when a group of people decide how to spend money from some common 

- charitible vs. utilitarian contributions
- complete information
- fixed budgets and Gibbard's theorem
- strategyproofness
- collusion



---

# Efficient Quadratic Funding with Incomplete Information using Virtual Agents

## Abstract

We propose a modification to the quadratic funding mechanism that is efficient under incomplete information. Participants report their utility functions to the mechanism, which calculates the efficient equilibrium using an iterative best-response algorithm. The modified mechanism is stable Bayes-Nash incentive compatible while preserving the theoretical properties of the original mechanism.

## Introduction

Complete information is unrealistic in public goods provisioning. Utility functions are rarely common knowledge, and even if they were. [so and so] showed that quadratic funding loses is efficient under incomplete information only under "knife-edge" conditions, and that inefficiency increases with population size.

Even with complete information, efficiency would require each participant to calculate the equilibrium, which can only be done by modeling the utility functions of all other contributors, and then executing some algorithm to find the equilibrium. Few individuals have the tools and sophistication to do this calculation.

Our proposal is to build this calculation into the mechanism itself. Voters communicate their utility functions to the mechanism, either using symbolic expressions or choosing from a parameterized class of utility functions, and the mechanism calculates the equilibrium and charges each user the corresponding amount. Because the resulting equilibrium is the same that would be obtained in the original QF mechanism under complete information.

## Mechanism

TO ADD: instead of expecting voters to know the utility functions of every member of society, we only expect them to know their own. And instead of expecting them to calculate the equilibrium, the mechanism calculates it for them.

Consider a variant of quadratic funding structured as an extensive form game. Contributors submit *provisional contributions*, which they can adjust at any time. The current funding amount is visible to all participants.

Each update represents a *conditional commitment*: a contribution that holds (with funds held in escrow) provided the total funding amount does not deviate beyond a predefined threshold.

## Convergence of Best-Response Dynamics

The quadratic funding formula guarantees a unique equilibrium, with concavity ensuring that iterative best-response dynamics lead to stable convergence. Since the best-response function is continuous and monotonic, adjustments stabilize at equilibrium. By the Banach fixed-point theorem, the contraction mapping framework ensures convergence to a unique fixed point. Imposing a maximum contribution constraint alters the feasible strategy set but does not disrupt convergence.

## Timing and Virtual Agents

A challenge is *timing*. While convergence occurs within finite rounds, delayed responses may extend the process indefinitely. A time limit risks locking in suboptimal contributions.

The use of virtual agents solves this problem. Participants report their *utility functions* and maximum contributions to their agent, and the agents use iterative best-response dynamics to discover the equilibrium.&#x20;

## Incentives

Individuals cannot gain from misreporting their utility to their virtual agents. The "inner game" played by the virtual agents has a subgame perfect Nash equilibrium. Agents should be programmed to seek an equilibrium: misreporting by agents only leads to to a suboptimal allocation, and misreporting by contributors only leads to misreporting by agents.

This holds regardless of beliefs about others’ utilities, ensuring robustness under minimal assumptions. The mechanism is therefore *robust*, *belief-free*, and *prior-free*.

## Practicality

Mchanisms requiring utility specification (e.g., VCG mechanisms) have been criticized for demanding excessive information from users. However, the nonlinear component of most reasonable quasilinear utility functions can be approximated by a small set of parameterized families (e.g., isoelastic), requiring only one or two parameters. Providing this information is less demanding for participants than estimating the utility functions of all other participants and then computing the equilibrium.

## Conclusion

By leveraging virtual agents, quadratic funding can efficiently allocate resources even under incomplete information. The modified mechanism preserves individual incentive compatibility but retains vulnerabilities, such as susceptibility to collusion.



----


# Quadratic Funding with Incomplete Information using Virtual Agents

We propose a modification to the quadratic funding mechanism that is efficient under incomplete information. The mechanism uses *virtual agents* to quickly discover the efficient equilibrium. The modified mechanism is stable Bayes-Nash incentive compatible while otherwise retaining the same theoretical properties as the original mechanism.

---

Complete information is unrealistic in many public goods provisioning settings. The utility functions of each member of society will never realistically be common knowledge, and even if they were, each participant would still need to use this information to compute the equilibrium -- which is not trivial.

However the welfare-maximizing equilibrium can still be *discovered* by participants even if they don't have complete information *a priori*. 

Consider a variant of quadratic funding played as an extensive form game, where contributors enter *provisional contributions* that they can change at any time. The current provisional funding amount is computed and visible to all contributors.

Each time the user changes their provisional contribution, they make a *conditional commitment* to contributing the stated amount given the total funding amount (including their own contribution) does not change by more than some threshhold amount. 

The QF funding formula guarantees a unique equilibrium, and players will converge on an equilibrium if they all consistently best-respond to changes by other players [todo: why? concavity!]. 

One potential problem with this approach is *time*. Although convergence within a any tolerance will happen within a finite number of rounds, if individuals don't respond promptly the process can continue for an indefinite amount of time. A time limit on may catch some users with provisional contributions that are far from optimal.

But the use of virtual agents can solve the timing problem. Individuals report their *utility functions* to the agent, along with their maximum contribution, and the agent immediately an automatically updates their contribution in response to changes by other contributors. If all contributors use a virtual agent, then the result is calculated instantly. [TODO: does the max contribution break our unique equilibrium assumption?]

Mechanisms that require users to specify their utility function (e.g. VCG mechanisms) have been criticized in the past as requiring individuals to supply too much information. But most plausible quasilinear utility functions will have a non-linear component that can be approximated by one of a handful of families of utility functions (e.g. isoelastic), which can be fully specified with only 2 parameters. Eliciting a function family and a couple of parameters surely is a lesser burden for participants than estimating the utility functions of all other users and using this to compute the equilibrium (presumably, using the same iterative best-response algorithm that the mechanism would perform on their behalf).

Individuals cannot benefit from unilaterally misreporting their utility functions. The subgame played by the virtual agents has a subgame perfect Nash equilibrium. Agents cannot profit (on behalf of contributors) from misreporting (todo: proof), and contributors can therefore not profit from manipulating their own agents. This is the case regardless of what individuals believe about the utilities of other agents (TODO: robust/belief-free/prior-free).

TODO: Summary










 can be described by a couple of parameters. For example, all scale-invariant preferences take the form:

  u(x_j) = ∑_j c_j * x_j^β

Scale-invariance means...which is a reasonable assumption for the utility voters enjoy from public goods projects


 and translation-invariant preferences

, also called *isolastic* preferences, take the same form.



The belief-free approach, spurred by Bergemann and Morris (2005, 2009a,b), has been especially influential. In essence, it requires mechanisms to ‘perform well’, regarldess of the agents’ beliefs about each other. 









We propose that any practical implementation of quadratic funding requires an equilibrium-discovery process.

The original Quadratic Funding mechanism assumes perfect information. This is probably not 



realistic in any plausible setting. The utility function of every member of society is not common knowledge, and if it was, each participant would still need to use this information to compute the equilibrium -- which is not trivial.

Many natural phenomenon can be explained as naturally-occurring equilibria; but these equilibria arise as a result of ongoing natural processes of *tatonnement* -- incremental changes in behavior made by individuals in response to the current aggregate behavior of everyone else. They rarely take the form of strategic-form games where everyone performs some complex calculation and agrees on a single rational outcome. 

Although equilibrium solution concepts help us understand where people end up, they don't necessarily tell us they get there. When designing a mechanism, we must both ensure a desirable equilibrium and provide a way for participants to find it.


A Quadratic Funding implementation can implement a equilibrium-discovery process as a kind of extensive form game, where contributors enter *provisional contributions* that they can change at any time. The current provisional funding amount as a function of all provisional contributions is computed and visible to all contributors.

Each time the user changes their provisional contribution, they make a *conditional commitment* to contributing that amount given the funding amount (including their own contribution) does not change by more than some threshhold value. 

Players will converge on an equilibrium if they all consistently best-respond to changes by other players. Experiments will show that convergence within a reasonable tolerance will generally happen within (small number) of rounds.

However the biggest problem with this approach is *time*. The process cannot continue forever, but if there is a time limit, individuals can manipulate the result by saving their contributions to the last minute.








A conditional contribution by user i is a tuple of a contribution amount and funding amount, and represents a commitment by user i to contribute the that amount given the total funding (including voter i's contribution) equals the stated amount. The process ends when there is a funding amount to which all individuals have conditionally committed.


A Quadratic Funding implementation can implement a equilibrium-discovery process as a kind of extensive form game, where contributors enter *conditional contributions* and can change them at any time. A conditional contribution by user i is a tuple of a contribution amount and funding amount, and represents a commitment by user i to contribute the that amount given the total funding (including voter i's contribution) equals the stated amount. The process ends when there is a funding amount to which all individuals have conditionally committed.



In situations of incomplete information, finding this equilibrium could take a long time. 



 where each individual enters a *conditional* contribution, which they commit to funding *given the current funding amount*.







However, QF can be modified to work just as well without complete information given an equilibrium-discovery process.

The QF mechanism has a Nash equilibrium that is also welfare-maximizing. But contributors need to discover this equilibrium is in order to know how much to contribute. Under the complete information assumption, contributors know the utility function of every single individual in society, and use this to compute this equilibrium. 



Complete information allows rational participants to calculate this equilibrium.



: it assumes each individual knows the utility function of every single individual in society. Further, it assumes they have the ability to compute the equilibrum

We propose a modification to the mechanism for settings of incomplete information.



This is unrealistic in almost any setting. A practical implementation of quadratic funding




# Quadratic Funding Probably Isn't Working

## ...and can't without an equilibrium discovery process

Quadratic Funding in an innovative approach to public goods funding that has generated a certain amount of excitement, especially in the world of cryptocurrency and DAOs, where it is used for example in the [Gitcoin Grants program].

The method has an elegant theoretical basis including a proof of optimality: under certain assumptions, the amount of funding allocated to a project is the amount that theoretically maximizes utility.

Unfortunately, these assumptions probably do not hold in any setting where the mechanism has been used to date. Of course, we don't need things to be perfect to be useful. But if the assumptions are too unrealistic, we need to improve the mechanism or implement in a way that the assumptions are approximated.

Perhaps the most obvious problem with quadratic funding is its susceptibility to collusion. The proof of optimality assumes people don't collude, but the mechanism creates a strong incentive for collusion. This problem and potential solutions have been extensively discussed elsewhere.

But I think there is an even more fundamental problem: that participants have no way to find the **equilibrium**.

The basic idea of quadratic funding is that individuals contribute to a public goods project, and then their contributions are matched from some fund (e.g. the government), using the quadratic funding formula. Each individual knows this formula, and thus knows how much to contribute to maximize their own individual utility. And the formula is cleverly designed such that, when every individual is maximizing their utility, overall utility for all individuals is also maximized.

But the problem is, the amount that an individual must contribute to maximize their utility **depends on what other individuals are contributing**. 

Now the formula is cleverly is designed such that **there exists an equilibrium**, where each individual is contributing an amount that maximizes their utility **given what everyone else is contributing**. That's great. But how do individuals find this equilibrium?

For an individual to know how much to contribute, they have to know how much other individuals are contributing. For example, it may be that a public park improvement project has $250,000 worth of funding, based on my neighbors' proposed contributions plus matching. And I know that if I contribute another $100, it will be matched at $10,000, bringing total funding to $260,100. This is worth it for me, because that extra $10,100 will be used to buy trees and plants that I think will create $100 worth of enjoyment to me individually.

However, let's say the total funding reaches $1,000,000. Well, at that point I think it's excessive. I think any extra money spent would be wasted, so I withdraw my contribution.

But this affects the calculation for *all other users*. Assuming they were all contributed an amount that maximized their individual utility *given what I was contributing*, now that I am contributing less, every one of the other individuals will benefit from contributing slightly more. Theoretically, they will eventually find a new equilibrium that partially (but not fully) making up for the loss of my contribution.

But how exactly does this equilibrium-finding process take place?

One rather optimistic possibility is everybody just *knows* what the equilibrium is from the start. We might assume individuals have [complete information] -- that is, everybody knows everybody else's complete utility functions for the project. Voters can thus deduce what the equilibrium will be by simulating this equilibrium-finding process in their own minds. 

This assumption is not realistic. In projects such as Gitcoin grants, individuals may have little idea who else might be contributing, let alone what those people's utility functions may be, or what their budget constraints are, etc.

# Virtual Negotiation

But suppose individuals can approximate their own utility function. What if we design a new mechanism in which individuals report their *utility function* to the mechanism, and the mechanisms performs a virtual negotation process to discover the equilibrium? Individuals commit to contributing whatever amount is negotiated, within some limit.

As long as user's utility functions confirm to certain standard assumptions, then the mechanism will find a unique equilibrium.

One nice thing about this mechanism is that it is strong-Nash incentive compatible. What  



question: can voters profit from holding back



mechanisms to fund open source projects
  https://www.matthewthom.as/blog/fund-open-source/
  good critique. Shows that actual mechanims with matching budget contraints are completely different mechanisms


https://globalprioritiesinstitute.org/luis-m-v-freitas-wildredo-l-maldonado-quadratic-funding-with-incomplete-information/



 have to increase their contributions


Of course, 



However, if then a bunch of otehr 



( (sqrt(100)*50) + 10 )^2 
- ( (sqrt(100)*50))^2 



A^2 + 2*AB + B^2 - A^2 = 2AB + B^2 = 2*10*50*10 + 100 = 10000 + 100


260100

The formula is designed such that there are *increasing, but diminishing returns* to individual contributions. The more I contribute to a project, the more it is funded, but the matching amount decreases with every dollar I contribute. So for everyone, there is some point where contributing another dollar result in less than a dollar of utility. They stop contributing at that point.


Suppose, for example, that a project to put up Christmas decorations on Main Street has $100,000 in funding (including matching). If I contribute 

50 people have contributed 100, that's $5000, but $250,000 of funding 





But how much will individuals contribute? 

Well, we assume that each individual experiences diminishing returns for all projects. Residents may each experience a certain amount of 


Quadratic funding rests on the idea that there is 

The idea behind quadratic funding is that individuals contribute to




## The Idea, Brief, and without Math

I think I need to review some key idea behind the mechanism here. We don't need to be mathematically precise to understand these.

First, the idea is that we have one or more projects that people can contribute to voluntarily. Then there is a matching fund, perhaps provided by the government. The more you contribute to a project, the more is matched. But the matching is not 1:1: the match amount is determined by the quadratic funding formula.

The theory is that we can predict how much a rational, utilitarian individual will contribute, based on some assumptions about their utility function -- the dollar-value of utility they receive from a project as a function 



For example, a project to decorate main street at Christmas. A rational, utilitarian individuals will contribute to the project in proportion to the dollar-valued utility they receive.



individuals will contribute to projects in proportion to its utility for them.



The theoretical elegance of the mechanism 





However


A lot has been written about how realistic these assumptions may or not be

But I will argue that the assumptions probably deviate too much from reality



But we can only hope realize some of the theoretical benefits in settings where the assumptions are at least approximated

. But despite a great deal of optimism I think that has not been the case in any application of 



My intent is not to enumerate or elucidate these, but to focus on just one problem that I see as a deal-breaker unless it is solved: the question of *how people find the equilibrium*.



However, there are however a lot of problems criticisms of the approach. Rather than discuss these criticisms, I will just focus on one major assumption, and why this assumption **probably** doesn't hold in any setting where quadratic funding has been used. And finally, a proposal for how the assumption *could* be made to hold.

