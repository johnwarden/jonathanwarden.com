---
title: "The Decision Engine and Prediction Markets"
slug: "the-decision-engine-and-prediction-markets"
image: assets/images/Pres12_WTA.png
alias: http://jonathanwarden.com/2013/08/16/the-decision-engine-and-prediction-markets/
date: "2013-08-16T21:41:00-07:00"
draft: true
---

Today I have been thinking about Prediction Markets, and thought I would share some thoughts on how the Decision Engine could be designed to behave very much like a Prediction Market.

If you aren't familiar with prediction markets, check out this explanation from <a href="https://medium.com/@argongroup/decentralized-prediction-markets-explained-d9f0425d331c">Argon Group</a>.
<h2>Verifiable Events</h2>
Prediction markets rely on events that will at some point in the future be objectively and unambiguously verifiable (e.g. who won a game, who was elected president). The Decision Engine, on the other hand, is designed for making predictions or decisions on questions that may be matters of opinion or judgement, with no external way of judging correctness of that decision. So how could the Decision Engine resemble a prediction market?
<h2>The Consensus Index as Market Price</h2>
The answer is the <strong>consensus index</strong>, which is a measure of the consensus on any question. As a discussion proceeds and supporting- and counter-arguments are introduced, discussed, validated or discarded, people's opinions will change. But (and this is a key aspect of the Decision Engine's design) it will be calculated not as the percentage of participants that agree/disagree, but the probability that a participant will agree/disagree after reading all the arguments.

The Consensus index will tend to change over time, especially as people introduce convincing arguments. It is this difference between the initial, pre-argument consensus index and the final consensus index (determined by some stopping condition such as time or stability) that makes for the possibility of an interesting prediction market.

Each question could have an underlying virtual security that can be redeemed at a price equal to, say, $1 times the consensus index at some stopping point (e.g. after a certain amount of time, or a certain level of stability). If you believe that by introducing a convincing argument, you can cause the consensus index to fall (and remember, the consensus index is calculated based on the percentage of people who agree/disagree <em>after reading yor argumets</em>), then you have an opportunity to earn points or (virtual) cash by shorting that security!
<h2>Example: Common Myths</h2>
Common myths make intersting cases for the decision engine, because they should result in a consensus index that changes dramatically after somebody has introduced an argument debunking the myth (which I think should be as easy as linking to Urban Legend or Snopes or Wikipedia).

For example, suppose this question and answer has been posted to the decision engine:

<strong>Question</strong>: who was the first European to arrive in America?

<strong>Top Answer</strong>: Christopher Columbus (<strong>Initial Consensus Index</strong>: 60%)

This is a fantastic opportunity!  You know it's a common myth that Columbus was the first European to discover America (taught in school's even). All you need to do is short this security, post an argument linking to any authoritative source on Christopher Columbus, and you can bet that the Consensus Index will fall and you'll make some money!
<h2>Moving the Market with Arguments</h2>
Not every discussion will be as simple and clear cut as a historical fact. However, even for more complex arguments, facts will matter, as the Decision Engine will ask people for their <em>reasons</em> for supporting a particular positions, and reasons supporting these reasons, and at some point each argument will be supported by a base set of assumptinos that may be more clear-cut. If some position on a complex topic is supported by a <em>reason</em> that is de-bunked, or on a reason that is supported by another reason that is debunked, the engine will adjust the consensus index accordingly. In this way, it may sometimes be possible for one person to topple a whole argument structure that was supported on one weak but commonly accepted assumption, causing the consensus index of a big argument to swing wildly, and make a fortune.