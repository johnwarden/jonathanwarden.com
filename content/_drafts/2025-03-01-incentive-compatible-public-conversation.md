# An Incentive-Compatible Protocol for Public Conversation

I am working on applying mechanism design to social media algorithms to incentivize good-faith conversation. I am getting close to a complete mechanism. Here is a summary of my thinking so far.

Good-faith conversation implies not only expressing honest opinions, but also updating opinions honestly when presented with new information and arguments.

So the goal is a mechanism with an equilibrium at which participants express honest opinions *given the arguments that have been presented to them*. 

But this is an ambitious goal. Actually proving incentive compatibility is difficult in general. And an online conversation setting presents unique challenges. What is the reward currency? How can it incentivize honestly about purely subjective opinions? How can it defend against Sybil attacks and coordinated manipulation? 

First, the payouts of the mechanism take the form of *attention*, because ultimately this is the only thing that motivates participants in social platforms. It may seem like there are many other possible motives, such as promoting a product, politician, or ideology, or simply making money. But you can only obtain these things through social platforms when people actually pay attention to what you have to say. Online discourse is an attention game. And so mechanism design may let us design a game with more desirable properties.

Now, how do you incentive honest expression of subjective *opinion*? There are some great mechanisms for *information elicitation without verification*. These mechanisms have a Bayes-Nash equilibrium where participants honestly report private information, such as personal opinions, as long as they expect others to do the same. 

The payout of such a mechanism can be used to calculate reputation scores, which determines how much influence each participant has on the collective attention of the group.

But online conversation is more than isolated expressions of opinion. It involves arguments, where people share ideas and information meant to change each others' opinions. So the mechanism should also incentivize good faith argument and *good faith response to argument*.

Finally, online platforms are vulnerable to deliberate manipulation. To keep things simple in this paper, we'll assume a closed community (e.g. invite only, reputation-gated) so that Sybil attacks are not possible. However, we won't ignore the problem of collusion or coordination, since experience with social platforms has shown that brigading, voting rings, and other forms of manipulation absolutely *will* happen in large online communities if people can benefit from it.

### Which Kind of Incentive Compatibility?

Clearly, such a mechanism cannot be dominant-strategy incentive compatible, unless we have a way of actually reading people's minds. But Bayes-Nash incentive compatibility is not sufficient if we need to worry about collusion. So what's left? Below we outline a mechanism where the minimum coalition-size that can profit from collusion grows must be [greater than X% of the size of the community....]







# Incentivizing Reasonableness

Suppose that participants can, in addition to authoring comments, vote to express support or opposition to other people's comments. Further, they can *change* their votes and, most importantly, the author of a comment can *retract* their comment if they no longer support it.

Now all these actions: authoring, voting, and changing votes, are all just *expressions of opinion*. And the same mechanism can incentivize honesty in all these scenarios. Which means we can design a mechanism such that, when somebody makes an argument that *honestly changes your mind*, the rational response is to *honestly update your opinion!*




## The Argumentation Protocol

I imagine a mechanism that looks a whole lot like a typical social media discussion, with comment and hierarchical replies, plus upvotes and downvotes. 

If at least one person downvotes a comment, there is disagreement, and the argument begins! First, the system identifies a minority and majority position (e.g. if 20% of voters downvoted the comment, the downvoters are the minority). The system then identifies the *top response from those in the minority*.

Finally, the system sends notifications to participants in the majority that looks something like this:

  Subject: So and So Responded to a Comment you Upvoted

    so and so | 3 hours ago 
    I disagree because yada yada

  After considering this comment, do you want to withdraw your upvote?

    [Yes] [No]

Your response is another expression of opinion: either a reaffirmation, or a retraction. But the reaffirmation/retraction is a *new, independent expression of* **conditional opinion**. There is a separate and independent payout for the conditional opinion. This is one of the most subtle but important aspects of the mechanism. 

Consider an example: a post claiming that politician X made outrageous statement Y. The post goes viral because it reinforces some people's negative opinions about X. But it has been taken out of context.

Initially, it gets a lot of upvotes. A certain percentage of people honestly believe that it is true. The payout function rewards people for honestly reporting their **initial opinion**, because those votes help predict the initial opinions of others.

But then somebody replies "here's what he actually said, and a link to the video where he said it". It then asks people whether they agree that X made statement Y *after considering that new information*. 

Some people will honestly change their minds, some won't. Either way, the payout function will reward people for voting honestly about their *conditional opinion*, because those votes will help predict the *conditional opinions* of others.

--- the mechanism is self-moderatoing






If you answer "No", it then asks?
  
  Why?

  [That's Not True / I Disagree ] [ I Agree, but I'm Not Convinced ]

And finally if you answer "I Agree, but I'm Not Convinced", it prompts

  - "If you agreed, would you be convinced"

  [Yes] [No]



If there is any disagreement, then those who disagree

## Incentivizing Reasonable Conversation
Symbll


----

- First you have just plain *upvotes*...
  - Weighted by reputation
  - Thomson sampling
  - Shadow Scoring
    - If you have no reputation, we still calculate predictive power
- Then you have *downvotes with reasons*.
  - weighted by reputation. So a single high-reputation person has "moderator-like" influence
  - reason could be link to existing "fact-check"
- Then you have top reason for downvote
  - Reason with most upvotes among downvoters
- Then you notify upvoters of reason
  - And calculate proability that they would till upvote



  Okay analysis here. Sybil resistance and collusion. 

  Upvoters Collude
    Causes more attention on post with note
    If you upvote post that is classified as misleading (or misinformed), should hurt reputation
      if you simply reaffirm vote, we don't know if you think factcheck is true
      if you have to answer "note is false / disagree " "Agree but..."
        then if you say not is false/disagree, reputation is on the line for that...


  Downvoters Collude




---




### Academic Paper Approach


# A Mechanism for Incentive-Compatible Public Conversation

We propose applying mechanism design to social networks to encourage **good-faith conversation**. 

Good-faith conversation implies expressing honest opinions, as well as updating opinions honestly when presented with new information and arguments.

We propose a conversation mechanism with a [what kind of] equilibrium at which participants express honest opinions *given the arguments that have been presented to them*.

Such a mechanism could directly address the spread of misinformation in social networks, and help to online conversation more informed, reasonable, and intelligent.

## Intro

Various mechanisms have been designed for *elicitation of information without verification*, including peer prediction, the Bayesian truth serum, the peer truth serum, and variants of these mechanisms. These mechanisms induce an equilibrium where participants honestly report private information, such as personal opinions, as long as they expect others to do the same. These mechanisms are possible because even personal opinions are not random -- opinions are statistically correlated. This means an honestly-expressed opinions of one person is *information* that can help predict the honestly-expressed opinion of others. This provides the basis for scoring formulas where participants maximize their expected rewards by reporting honestly, given others are doing so.

But applying these to the setting of online conversation is challenging. First, participants may have exogenous incentives: they may be trying to promote a product, an ideology, or their own reputation. Second, participants in online conversations may self-select and collude. The phenomenon of *brigading* is well-known in online forums: a small motivated group can take over a conversation, drowning dissenting opinion.

The first problem has a surprisingly simple and elegant solution. Most exogenous incentives can be internalized, so to speak, by using **attention** as the payout currency. The rewards of participation in social media are almost all mediated by attention; it is impossible to promote an idea, a product, or oneself, unless people are paying attention to you. Further, it is possible to *quantify* and measure attention; certain online behaviors such as clicks or likes are directly proportional to the amount of attention something receives. So platforms can throttle the rate that attention is allocated to any piece of content by ranking and filtering. And this attention has a market value, so it is even possible to approximate the dollar-value of the payouts of an attention-allocation mechanism.

TODO: reputation

The second problem is solved with a feedback loop, where the more attention a post receives, the more reputation is lost when participants behave dishonestly, such that manipulation reduces their long-term attention payout.

## Measuring Attention and Upvote Rate

Stuff from: https://github.com/social-protocols/quality-news#readme

## Setup

- A set of "statements"
- A set of participants
- Each participant has a binary opinion on the statement 
- An App
  - Routes attention to content 
    - Rate of attention is proportional to upvote rate
  - Measures how much attention has been routed
  - Collects "upvotes"

## Estimated Upvote Rate

  - Gamma Distribution

## Scoring Formula

  initialRate = 1.0   log= 0
  entryRate = 2    log = 1
  postEntryRate = 2.1 log = 1.07 
  finalRate = 4 log = 2

  total information value created is: totalInfoValue = log(finalRate) - log(initialRate) = log(finalRate/initialRate)

  information value created by me is: myInfoValue = log(postEntryRate) - log(entryRate) = log(postEntryRate / entryRate)

  percent created by me is: myInfoValue / totalInfoValue = log(postEntryRate / initialRate) / log(finalRate/entryRate) * upvotes

  








  




  a    b  c        d

  c-b / d-b



buy at .80
sell at .90
profit

  log(initialRate)


  
  ( finalRate - initialRate )  

















--






The problem of self-selection has solutions, though these are messier. In online discussion contexts, participants don't just write posts and comments, they also *vote*. This voting data can result in multiple opinion data points for each participant, which can be the basis for clustering or latent factor models that can be used for statistical corrections when the self-selected set of participants is not representative of the larger population. Twitter/X's Community Notes is one example of a platform that has successfully used such an approach. 

Attention-allocation mechanism also have feedback loops that frustrate manipulation, if a small group *brigades* to temporarily give an item more attention, the additional attention will result in more votes from people not part of the brigade. If a latent-factor or cluster model has identified the "brigade", then the 












 a phenomenon reflected in online advertising pricing structures.

TODO: stronger link between



Observe that whatever ulterior motives (exogenous incentives), 


attention / legitimacy


prior with AI





requires addressing a few major issues: anonymity and reputation, ulterior motives, and self selection bias.







This correlation can be used to design formulas that reward users for information that predicts

, being information, can reduce entropy


  anonymitiy
  self-selection
  attention







Specifically, we propose a mechanism where express


Online conversations takes place on platforms that create incentives for bad-faith conversation. 

Online conversation platforms are a mechanism


We propose a mechanism that aligns

