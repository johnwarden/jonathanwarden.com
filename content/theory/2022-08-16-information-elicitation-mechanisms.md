---
layout: single
title:  "Information Elicitation Mechanisms: Paying for Truth"
slug: information-elicitation-mechanisms
date:  2021-09-14 00:00:00 +0200
toc: true
toc_sticky: true
tags: ['Game Theory']
canonical_url: https://jonathanwarden.com/information-elicitation-mechanisms
summary: This essay explains how you can pay people to tell the truth, even if you can't verify their answers. 

---

## Introduction

<!--
[TODO: link to studies with emperical verification of these methods]
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0177385
-->

This essay explains how you can pay people to tell the truth, even if you can't verify their answers. 

The key is to create the right incentives. This can be done by making payments using a formula that takes into account how people's answers correlate with those of their peers. For this reason these techniques are often referred to as [**Peer Prediction**](https://presnick.people.si.umich.edu/papers/elicit/FinalPrePub.pdf) mechanisms. More generally they are known as mechanisms for **Information Elicitation without Verification (IEWV)**. 

A summary from a few years ago of the state-of-the art in IEWV can be found in the short textbook by [Faltings and Radanovic](https://www.amazon.com/Game-Theory-Data-Science-Intelligence/dp/1627057293). A real-world validation of these results in an a large-scale crowdsourcing project is described by [Frank et al.](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0177385)

In this post I will provide a non-mathematical explanation of how these techniques work. This post assumes familiarity with the concept of a [Coordination Game](https://en.wikipedia.org/wiki/Coordination_game) from game theory, which is introduced in the [previous post](/truthtelling-games) in this series.

## The Basic Idea

Suppose we are randomly drawing from an urn containing a mix of red and blue marbles. With each marble we draw, we get a better idea of the ratio of red to blue: **each draw provides information** that tends to improve our estimate of the true ratio. This is a key concept in statistics, enshrined in the [law of large numbers](https://www.britannica.com/science/law-of-large-numbers).

Now suppose that instead, a group of students each draws one marble, looks at it, and puts it back, without showing it to anyone else. Each student then reports the color they drew. We can use the reports of the students to estimate the true ratio. The problem is, now we don't know if they are telling the truth.

## Paying for Information

But we can pay them for their reports in a way that incentivizes truthtelling. Since accurate information tends to improve our estimate of the true ratio, we simply pay students based on how well their report improves our estimate. The closer the new estimate is to the true ratio, the higher the payment. Inaccurate reports have no information value: the result of a coin flip can't possibly improve our estimate of the content of the urn. So students will generally receive a higher payment if they report the truth.

But how do we know the true ratio to compare to? We only have the students' reports. We have a chicken-and-egg situation.

## Peer Prediction

The [Peer Prediction](https://presnick.people.si.umich.edu/papers/elicit/FinalPrePub.pdf) mechanism solves this problem by calculating each student's payment based on how well our estimate, after taking into consideration the student's report, predicts the report of a randomly selected peer. The payment formula must be a [proper scoring rule](https://www.lesswrong.com/tag/scoring-rule): a way of scoring a probability estimate based on how likely the actual outcome was estimated to be. The closer our estimate is to the actual probability that a random peer reports red or blue, the higher the expected score in the long run. So as long as their peer is telling the truth, students maximize the expected value of their payment by also telling the truth.

<!--
[TODO: aside with log example]
-->

## Coordinating Honesty

But why do they expect the other students to tell the truth? This is a coordination game, just like in a blockchain consensus protocol. As described in [truthtelling games](/truthtelling-games), everybody wins if everybody tells the truth, but to successfully coordinate on the truthtelling strategy, everybody needs to believe that everyone else is going to tell the truth.

The Peer Prediction game actually has other equilibria. For example, there is an equilibrium on the strategy of always reporting blue, or on always reporting the opposite of what you observed, etc. If the group coordinates in advance on the always-blue strategy, then everyone will expect everyone else to pick blue, and so that's what they will do.

But establishing an expectation of truthtelling is not hard: it can be as simple as having one participant that is known to be scrupulously honest. 


## Your Subjective Opinion is Information

This method gets students to reveal **private information**: something they know that nobody else does, and that there is no way of verifying. But this information is not completely arbitrary: it is random, but because students draw marbles from the same urn, each marble is a sample from the **same underlying probability distribution**. The more we sample from the group, the more information we gain about the underlying distribution.

This means any private information can theoretically be elicited using this method, because a person's private beliefs and preferences are also just a sample from a group, and the laws of nature dictate that anything we measure about a group will follow some probability distribution. If we ask people "do you like anchovies?", there is some true ratio of students that actually like anchovies, just like there is a true ratio of red to blue marbles. As a member of the human race every fact about us correlates with facts about other people, and therefore has value as information that can help predict facts about others.

## The Law of Attention

Unfortunately, all this is based on an assumption that can rarely be made in reality: that the students are sufficiently motivated by the payments, and don't have ulterior motives for lying. But of course if we are asking for potentially embarrassing or compromising information, or if people have an agenda, a small payment can be a relatively insignificant motivation. 

However in [the Law of Attention](/the-law-of-attention) I argue that in online communities, **people will always behave as if they are motivated primarily by attention**, because those that don't will stop participating in the community.

So if the payout is **attention**, it is possible to create social platforms where people only upvote or share content that they honestly endorse.

This may not seem like a big deal. Don't people already like and share things they honestly endorse? Definitely not. First, without the protection of anonymity, people are known to [express socially "correct" opinions even if they disagree](https://en.wikipedia.org/wiki/Preference_falsification#:~:text=Preference%20falsification%20is%20the%20act,preference%20is%20more%20acceptable%20socially.). And anonymity makes it worse: users will often promote a political or commercial agenda instead of their true beliefs.

Honesty about subjective opinions requires the right incentives. Among online users in a social network these incentives can only exist in a a game-theoretic equilibrium, where users maximize attention by being honest.


## Common Priors

But there is another catch. The peer prediction method rests on an assumption of [**common priors**](https://economics.mit.edu/files/17484). This assumption effectively means that people **don't know how other people are going to answer** -- or at least they don't have any knowledge that the person running the game doesn't have.

Unfortunately, if people do know how their peer is likely to answer, then this is also information that can be used to improve the estimate of the true ratio. If their own opinion differs from the opinion they expect their peers are likely to have, then knowledge of popular opinion may be a better prediction of the peer's opinion than their own. In these cases, student's that know that their opinion is unpopular are better off lying.

However, knowledge of popular opinion has no information value if the person running the game (we call this person the "center") has the same knowledge, and has already incorporated it into their estimate. So for peer prediction to work, we have to assume that the students and the center have the same information, or **common priors**. Given these assumptions, the only way for a student to improve their expected score is to provide information the center doesn't already have: the student's own opinion.

Unfortunately, the assumption of common priors severely limits the applicability of Peer Prediction....

## Bayesian Truth Serum

But this problem is solved by a variation of the Peer Prediction method called the [Bayesian Truth Serum](https://economics.mit.edu/files/1966). This method asks participants one additional question: what is your prediction of the percentage of people that will agree with you? People's payouts are then calculated partially on the accuracy of their prediction. So as long as they expect other participants to answer honestly, they are better off making an honest prediction. 

But the genius of the mechanism is this: their payouts are also partly based on a formula designed such that, if other participants are making honest predictions, they are better off answering honestly! 

In other words, there is an equilibrium where everyone answers honestly because everyone predicts honestly, and everyone predicts honestly because everyone answers honestly.

## Peer Truth Serum and Other Methods

But asking students for this estimate complicates things. One of our goals is to induce truthtelling in a social network. In social networks, asking people for their predictions about how many other people will also share or upvote some post would require awkward UI changes, and people may not engage with them.


Fortunately, variations of these methods have since been developed that don't assume common priors, and don't require asking for predictions.

With the [Peer Truth Serum](https://ojs.aaai.org/index.php/HCOMP/article/view/13145/12993) mechanism, the center publishes their current best estimate of the probability that the next participant will answer "yes" to some question, and does a Bayesian belief update of this estimate each time it receives a report. The scoring mechanism is designed such that, if the participant believes the center's estimate is too low, they maximize their expected payout by answering "yes", and otherwise they maximize by answering "no". So users don't answer based on whet they personally honestly believe, but rather on what they think other people believe. The paper proves that this mechanism is *asymptotically truthful*: It will converge converge on the true distribution.

Another mechanism that doesn't require common priors is the multi-task peer-prediction method proposed by [Dasgupta and Gosh](http://www.arpitaghosh.com/papers/elicit_arxiv.pdf) asks participants for opinions on **multiple** items, and then the system in a sense uses this information in place of common priors. Obtaining opinions on multiple items is not a problem in social networks if, for example, people upvote or downvote many posts. This mechanism has the additional benefit of creating incentives for participants to put effort in solving the the task.

I would also propose using LLMs to provide estimates. Since the "beliefs" of an LLM are in essence the average beliefs of a large sample of human beings as expressed in an large archive of human language, these predictions may be as good or better than the predictions of most participants -- especially if the LLM can be fine-tuned on the beliefs of the particular group. Of course this calls into question the need for asking people for their opinions at all. But I would expect LLMs to do a good job of predicting what people **believe other people to believe**, and these beliefs [may differ from what people actually believe](https://en.wikipedia.org/wiki/Pluralistic_ignorance).


### Other Issues: Self Selection and Brigading

In social networks such as Reddit, *brigading* is when a group of users coordinates to submit a large number of votes on some piece of content that would receive relatively few votes otherwise. If the subgroup all vote honestly, they will still expect positive rewards.

The problem here is *self-selection*. In social networks, users self select: usually decide what to vote and not vote on.

## Conclusion

We think these mechanisms have tremendous unrealized potential: in crowdsourced fact checking systems and blockchain governance protocols, and as key part of social media algorithms that [give truth the advantage](/give-truth-the-advantage).


