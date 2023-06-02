---
title: "Example Decision Engine Process Walkthrough"
slug: "example-decision-engine-process-walkthrough"
image: assets/images/globe-and-wires.png
date: "2013-06-21T16:29:00-05:00"
aliases:
- /2013/06/21/example-decision-engine-process-walkthrough/
---

In my last post, I introduced the general idea behind the decision engine. This post walks you through an example of what the decision engine might look like from a user's point of view, and hopefully give you an idea of how it could result in more intelligent group conversations.

You see a screen that says:
<div style="font-family: Courier">
<p style="padding-left: 30px;">Consider the following statement:</p>
<p style="padding-left: 30px;"><em>"Romney would make a better President than Obama"</em></p>
<p style="padding-left: 30px;">Do you:</p>
<p style="padding-left: 30px;"><strong><span style="color: #993300;">Agree</span> | <span style="color: #449977;">Disagree</span></strong></p>

</div>
Suppose you think Obama is the better candidate, so you click "Agree"
<div style="font-family: Courier">
<p style="padding-left: 30px;">Thanks for your opinion. You earned an additional 10 points.</p>
<p style="padding-left: 30px;">Now, earn additional points by responding to one of the arguments below, or posting your own.</p>

</div>
<h2>Reasons</h2>
Wow, it's easy to earn points in this game. Reading on, I see this:
<div style="font-family: Courier">
<p style="padding-left: 30px;">Which of the following are valid reason&nbsp;to DISAGREE:</p>

<ul style="padding-left: 30px">
 	<li><strong>Romney is a Republican</strong></li>
 	<li><strong>Mormons are Polygamists</strong></li>
</ul>
</div>
Interesting. I know that mainstream Mormons have not practiced Polygamy since the 19th century. No, let's say my reasons for preferring Obama are purely partisan. So I select "He's a Republican", but not "Mormons are Polygamists".
<div style="font-family: Courier">
<p style="padding-left: 30px;"><span style="color: #993300;">Congratulations</span>! 80% of people chose "Romney is a Republican" as a valid reason to disagree. You earned 10 more points.</p>
<p style="padding-left: 30px;"><span style="color: #449977;">Sorry</span>! 68% of people chose "Mormons are Polygamists" as a valid reason to disagree.</p>

</div>
<h2>Convincing People</h2>
Hmm, I am glad there are people who agree with me, but disappointed in their reasons. But wait, the argument is not over.
<div style="font-family: Courier">
<p style="padding-left: 30px;"><strong><span style="color: #0000ff;">Challenge Opportunity</span></strong></p>
<p style="padding-left: 30px;">You now have the opportunity to challenge the majority and earn 100 points. Submit your argument against the claim "Mormons are Polygamists", and you will gain points for every person that is convinced by your argument.</p>

</div>
So I write up my argument:
<div style="font-family: Courier">
<p style="padding-left: 30px;"><strong>SUMMARY</strong>: That's a Myth</p>
<p style="padding-left: 30px;">Okay looks folks, the Mormon Church did away with Polygamy in 1890. And although there are some offshoots of the mainstream Mormon church that still practice it, it's pretty rare. You can find it all in this article.</p>
<p style="padding-left: 30px;">http://en.wikipedia.org/wiki/Mormonism_and_polygamy</p>

</div>
I click submit, and then go on with my day. &nbsp;The next day, I receive an email with the following email:
<div style="font-family: Courier">
<p style="padding-left: 30px;"><strong>SUBJECT</strong>: Congratulations! You have convinced the majority. You earn 100 points.</p>
<p style="padding-left: 30px;">Only 45% of people accept the argument "<em>Mormon's are Polygamists</em>", after reading your argument "<em>It's a Myth</em>".</p>

</div>
Wow! I didn't expect that. I changed the percentage of people that were convinced by the silly "Mormon's are Polygamists" argument from 68% to 45%?
<h2>Will People be Rational?</h2>
How did that happen?

Well, first of all, a lot of people simply don't know a lot about Mormons. That's good for me, because ignorance can be cured with information. For some, just reading my comment or reading the Wikipedia article was an eye opener, and was enough to convince them.

Second, the system was asking <em>new</em> participants on the discussion what they thought <em>after</em>&nbsp;reading your comment. These new participants are less likely to be entrenched in a position, and will not take a position until having read your argument. The fact is, a group of people just asked to answer based on what they know will typically answer differently than a group of people that are first asked to reads relevant information.

Third, participants will be forced to consider whether <em>other</em> people will find your counter-argument convincing, before taking a position. This helps replace a knee-jerk reaction with a thought process such as "will I get shot down if I take this position? Is that statement really true? Is that article reliable? Will there be a strong counter-argument?"

Finally, participants have the opportunity to gain lots of points if they correctly guess whether the argument and counter-argument will be accepted or rejected. If they think that the "Mormon's are Polygamists" argument is ultimately weak, even if it is an argument supporting their position against Romney, they will not only be encouraged to reject the argument but even to pile on their own counter-arguments. If they want to convince people not to support Romney, they will find a stronger argument and support that.

So I hope this example gives you an idea of how the right conversation structure and scoring mechanism can encourage people to act rationally and produce more meaningful, intelligent conversations. I won't know if this particular process really works until testing with real prototype, but these are the lines along which I'm thinking.