---
title: "Understanding Bridge-Based Ranking"
date: "2024-01-01"
math: true

---

## Introduction

[Bridge-Based Ranking](https://www.belfercenter.org/sites/default/files/files/publication/TAPP-Aviv_BridgingBasedRanking_FINAL_220518_0.pdf) is an alternative way to score and rank content by **adjusting for user polarization**.

The most successful implementation of Bridge-Based Ranking, X's Community Notes, explains that the algorithm favors notes that are rated highly by users across a "[diversity of perspectives](https://communitynotes.twitter.com/guide/en/contributing/diversity-of-perspectives)". But as I show in this article, ratings from users with diverse perspectives are not necessary for a note to rank highly. It is somewhat more accurate to say that the note must be highly rated **regardless of diversity of perspective**..

The algorithm works by attempting to model *why* a post receives the votes it does: how many votes are due to users left-wing or right-wing biases, and how many are due to other factors. If a post is only appealing to right-wing voters, and an online forum is dominated by right-wing voters, then that right-wing bias probably explains why it gets so many upvotes. So the algorithm tries to correct for this bias, and estimate how many upvotes a post would receive if all users were perfect centrists.

### Extracting Information

Now why would we want to do this? Why for example would a predominantly left-wing community want to artificially give right-wing opinions more weight, especially if they think their own side is better-informed?

For a fact-checking product like Community Notes, plausible political neutrality may be necessary for public acceptance. But bridge-based ranking has advantages beyond political neutrality: it actually allows us to extract more **information** from users. 

In Community Notes, users rate notes as "Helpful" or "not Helpful". If Community Notes was dominated by leftists, what would we learn by the fact that a note received a lot of "Helpful" votes? That it is helpful? Or that it supports a left-wing worldview? Or both? It's hard to say: perhaps a marginally helpful note that supports a left-wing world view gets more votes than a truly helpful note that supports a right-wing world-view. We can't tell just from the raw vote counts how "helpful" the note is.

Bridge-based ranking on the other hands let's us break down the vote counts, attributing some to whatever users think "helpfulness" means and others to polarity. So it is not about giving "both sides" equal weight; by "cancelling out" the effect of political bias, we can actually extract more interesting **information** from the users' votes. 
 
### Projection in Opinion Space 
 
The chart below illustrates how this works. This charts shows a subset of notes from the Community Notes public data set, run through [my own implementation of the algorithm](https://github.com/social-protocols/bridge-based-ranking). The horizontal axis shows the note's "polarity" -- e.g. +1 for right-wing and  -1 for left wing -- and the vertical axis shows its "helpfulness". The note's final score is its vertical component, or its projection on the "helpfulness" axis. The colors of the dots indicate their actual status in Community Notes.


 
<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-items-2d.png' 
                 alt='Community Notes Polarity Plot (Notes)' 
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


Notice how there is a large spread along not just the the vertical axis, but also the horizontal axis. If want we want to know how helpful a note is, the horizontal axis is just noise. But there is a lot of information along the vertical axis. Separating the polarity factor from the helpfulness factor by ignoring the horizontal component lets us extract extract this information.

But what is this information? It is a measure of some aspect of a post which increases upvotes on that post independently of polarization. What exactly this feature is is impossible to say, but presumably it reveals how users interpret the idea of "helpfulness".

### Why it Works


People are politically biased, but they are also in a sense biased towards helpfulness. That is, they will mostly upvote notes that support their perspective but they will **especially** upvote notes that support their perspective and are actually relevant, factually accurate, etc.. And they will tend to downvote notes that support opposing perspectives....but they will extra zealously downvote notes that support the opposing perspective using false or misleading information.

When bridge-based ranking algorithm dissects users voting behavior and factors out the polarity component, it finds that **most users are at least somehat biased towards helpfulness**! You can see this in the plot of a sample of Community Notes users below. 

<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-users-2d.png'
                 alt='Community Notes Polarity Plot (Users)'
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


The first thing you may notice is the clump of users in the upper-right quadrant. This tells us community notes users are overall right-leaning (though possibly this is an artifact of the way I sampled the data). But notice also that the helpfulness factor for these users is mostly above zero. They are mostly biased towards helpfulness; their votes can't be entirely explained by their politics. 


### Common Ground

This vertical component in these plots represents **common ground**. It is something users tend to agree on independently of their politics.

In the case of Community Notes, this is presumably some common idea of what constitutes "helpfulness". But in general what exactly the common ground is depends on the community. Suppose for example there is a forum for Harry Potter fan fiction that unfortunately in recent years it has been overwhelmed by debates about whether J.K. Rowling is transphobic. There is still a lot of good fan-fiction being posted, but the home page is dominated by posts about the controversy.

In this case, the horizontal axis would likely represent the pro- and anti- J.K. Rowling factions, and the vertical axis would represent the common ground of the community: quality Harry Potter fan fiction. Using bridge-based ranking we can in a sense de-polarize the forum, factoring out the effect of polarization and getting back to community's original essence.

Politics is not the only factor that can divide a forum. Suppose there is a popular forum for posting ridiculously cute pet pics. Sadly, in recent years, two factions have formed: the cat faction and the dog faction. The more extreme cat people mercilessly downvote pictures of dogs (regardless of how cut they are), and the dog people vice versa. Recently, the dog faction has gained the upper hand, and a cat-picture has little chance of making the front page, no matter how frigging adorably it is.

Again, by separating the dog-cat factor from the common ground factor, we can re-focus the community on it's original purpose: raw frigging cuteness.



## Understanding the Algorithm

But how does the algorithm actually work? How does it determine the polarization factor and common ground factor for each user and post?

It actually works using a fairly simple algorithm called Matrix Factorization. Below I will explain how the Matrix Factorization algorithm works, starting with the version implemented by Community Notes and described in the [Birdwatch Paper](https://github.com/twitter/communitynotes/blob/main/birdwatch_paper_2022_10_27.pdf). There is also a good writeup by [Vitalik Buterin](https://vitalik.eth.limo/general/2023/08/16/communitynotes.html). In my [next post](/improving-bridge-based-ranking) describe my variation of the algorithm that uses 2D matrix factorization.

A good way of understanding Matrix Factorization is that it is like running a bunch of linear regressions: one for each user and each item.

For example, suppose we have already discovered the polarity factor for each user, and we want to find the polarity factor for each post. A linear regression predicts users' votes as a function of their polarity factors.

For a highly polarizing right-wing post, the regression line might have a positive slope:


***Highly Polarizing Right-Wing Post***

           Vote 
            +1   ✕ ✕ ✕ ✕ 
             |    ↗
             |  ↗ 
    -1 ______|↗______ +1  User's Polarity Factor
            ↗|
          ↗  |
        ↗    |
      ✕ ✕   -1



In this chart upvotes have a value of +1 and downvotes have a value of -1. All the right-wing users upvoted and all the left-wing users downvoted (as shown by the little ✕s). So the best fit is a line with a slope of approximately +1: the more right-wing the user, the higher the probability of an upvote, and the closer the predicted value is to 1. The more left-wing, the higher the probability of a downvote, and the closer the predicted value is to 0. 

Note that there are more right-wing users than left wing users, but it doesn't make a difference. Even if there were 100 right-wing users and 2 left-wing users, the slope of the best fit would be approximately the same. This is why bridge-based ranking does not favor the majority.


A very polarizing lift-wing post might have a negative slope:


***A Highly Polarizing Left-Wing Post***


           Vote    
      ✕ ✕   +1    
        ↘    |     
          ↘  |    
    -1 _____↘|________ +1  User's Polarity Factor
             |↘
             |  ↘
             |    ↘
                ✕ ✕ ✕ ✕

For a completely non-polarazing post, on the other hand, the slope would be zero:


***A Non Polarizing, "Good" Post***

           Vote    
    ✕ ✕     +1     ✕ ✕ ✕ 
      →  →  →|→  →  →  
             |  
    -1 ______|________ +1  User's Polarity Factor
             | 
             |   
           ✕ |       
       

This is a good post. Not just because the upvote probability is independent of the user's politics, but because this post receives mostly upvotes -- the intercept is above zero. This post has some quality that users of this forum are looking for.


<!-- And for completeness sake, a unequivocally bad post might look like this.


           Vote    
            +1      
             |
             |  
    -1 ______|________ +1  User's Polarity Factor
             | 
             |   
      →  →  →|→  →  →  
       

 -->

Now, suppose there is a post that looks like this:


***A "Good" but Polarizing Post***

           Vote
       ✕    +1   ✕ ✕ ✕ ✕
             |  ↗ 
             |↗
            ↗|
    -1 __ ↗__|________ +1  User's Polarity Factor
        ↗    |
      ↗      |
    ✕ ✕ 
 

This post has a positive slope, so it is clearly very polarizing. But the positive intercept means that voting behavior for this post cannot be explained entirely by politics. There is also a component that makes users more likely to upvote it independently of politics.

### The Intercept is Common Ground

So the intercept represent "common ground". It represents something about a post that causes users to upvote independently of politics that cannot be explained entirely by users' polarity factors.

### The Intercept is not the Average

We might suppose that the last post above will receive more upvotes than downvotes because it has a positive intercept. But this is not necessarily the case. It depends on how many left-wing and right-wing users there are. The intercept is not the average: a post can have a positive intercept even though it receives more downvotes than upvotes, or it can have a negative intercept even though it receives more upvotes than downvotes. 

What a positive intercept does tell us is that this post **would** receive more upvotes than downvotes if there **was** an equal balance of left and right-wing users. 

It also tells us how users would hypothetically vote if they were all totally a-political. In such a hypothetical world, the only thing influencing users votes is some common-ground factor that aligns with the intent of this particular community, attracting upvotes independently of politics.


### Matrix Factorization

Okay, so we have used regression analysis to find the polarity factor for each post (the slope) of the regression line. But in order to do these regressions, we first need to know the polarity factors for the users.

But how do we find these?

Well, if we knew all the **posts'** polarity factors, we could use regression analysis to estimate the probability that a user upvotes a post as a function of the polarity factors of the posts. The slope of the regression line would then be the user's polarity factor. The regression line for a very right-wing user, for example, might look similar to that for a very right-wing post.


    A Right-Wing User

           Vote 
            +1   ✕ ✕ ✕ ✕ 
             |    ↗
             |  ↗ 
    -1 ______|↗______ +1  Post's Polarity Factor
            ↗|
          ↗  |
        ↗    |
      ✕ ✕   -1


But we seem to have a chicken-and-egg problem, where we can't find the polarity factors of users unless we know the polarity factors for posts, and vice versa.

However, the Matrix Factorization algorithm solves this by discovering the polarity factors (and intercepts) for every user and every post all in one go. 

It does this by using a single equation to estimate the probability that user $i$ upvotes post $j$:

$$
    ŷ_{ij} = w_i×x_j + b_i + c_j
$$

Here $w_i$ is the user's polarity factor, $x_i$ is the post's polarity factor, $b_i$ is the user's intercept, and $c_j$ is the post's intercept.

It then simply finds a combination of values for every $w_i$, $x_j$, $b_i$, and $c_j$ that best *fits* the data -- that produce values for $ŷ_{ij}$ that are closet to the actual values of users vote ($y_{ij}$). This is usually done using a variant of the standard [gradient descent](https://en.wikipedia.org/wiki/Gradient_descent) algorithm.

The polarity factor the algorithm discovers doesn't necessarily correspond exactly to politics, or cat-dog preferences, or any measurable quantity. It may be a linear combination of factors. But whatever it is, it represents **some** latent factor of users and posts that does a good job predicting their votes.

## Conclusion

One of the reasons for my interest in bridge-based ranking is that I think it may be a critical part of a [social protocol](https://social-protocols.org) for a self-moderating community. Without it, user polarization will tend to lead to either suffocating uniformity or least-common-denominator mediocrity. Bridge-based ranking can be used in any forum with high entropy (lots of downvotes) as a way to identify posts with posts with high [Information Value](https://social-protocols.org/global-brain/information-value.html) based on the common-ground factor.


In my [next article](/improving-bridge-based-ranking), I discuss ways that this algorithm can fail, and introduce an improved implementation of the algorithm that users 2-dimensional matrix factorization.


<!--




-----






However, another thing we want from online communities is **information**. We want to find content that is useful, or funny, or accurate, or whatever, and sometimes looking at what **other** people find useful, or funny, or accurate is a good way to do that. But often when a community becomes polarized, upvotes no longer represent these aspects of the content. If people are upvoting something **because** it supports a left-wing or right-wing point of view, then they are not upvoting it **because** it is useful or funny or current.

So bridge-based ranking can be seen as a sort of Information-extraction algorithm, that separates the polarity component from some core component that represents whatever it is that people are looking for from this forum -- what we might call the "common ground" component.


So for example Community Notes attempts to identify notes that users find **helpful** -- which basically means accurate and relevant. Unfortunately, users are very biased, and tend to upvote notes not because they are helpful, but because they support their political perspective. 

But they, **also**, to a lesser degree, tend to upvote notes that are helpful. They will mostly upvote notes that support their perspective but they will **especially** upvote notes that support their perspective and are actually helpful. And they will tend to downvote notes that support opposing perspectives....but they will show extra enthusiasm in downvoting notes that support the opposing perspective using false or misleading information.

When bridge-based ranking algorithm dissects users voting behavior and factors out the left-right component, it finds that **almost everyone is slightly biased towards helpfulness!**. Consider the diagram below. It shows a subset of users from the Twitter Community Notes data after running my own modified version of the algorithm. The X axis is the user's polarity factor (roughly, left-wing to right-wing), and the Y axis is their "helpfulness" factor. 

You can see that far more users lean towards the (left/right). Without using bridge-based ranking, Community Note would just be a (left/right-wing) opinion meter... 

But more interestingly, you can see the vast majority of users on both side have a positive value for the "common ground" factor (the vertical axis). 







 inaccurate, unfair, or in any way unhelpful that 



, and downvote, or at best ignore, notes that don't. 





Whether it be technical knowledge or cute pet pics or funny jokes or accurate news. An online community uses the "wisdom of crowds", or at least the aggregate opinion of crowds, to help you discover content that you are also likely to find cute or funny or useful or accurate.




Discussion

In a forum 


...Crowdsourcing....extracting information




Unfortunately, in an online forum where participation is open to random people on the internet, the opinions of each side will not be very sophisticate


...but fact checking....

But ...beyond ostensibly politically neutral fact-checking...compromise...

For every online forum that becomes polarized. Yes, we want our side of the culture war to win, but we also don't want every corner of the internet to be dominated by expressions of the correct opinion about the Isreal-Palestine conflict. 

Unfortunately, whatever long-term foresight some people may exercise when pressing the vote button, it tends to be overwhelmed by short-term passions. They may faithfully downvote political content when they see but tend to let one or two items through on subjects they are particularly passionate about. Others are less disciplined. The aggregate result is a bias towards political content, skewed towards one side.

The trick of bridge-based ranking is to get people



It models **why** users 
    strays from that ... start to downvote



People with some common understanding to 





The resulting ranking of content represents the "common ground" of the community.


Now what polarizes a community may not always be politics. It could be a controversy about styles of art, an economic theory, or the meaning of a word. And it may be that a community is not polarized, or that differences of opinion are best explained by differences in, say, expertise. In these cases bridge-based ranking can do more harm than good (e.g. dumbing down a forum by removing "expertise bias"). In another essay I hope to explain an variation of the algorithm that addresses this problem.

TODO: show how Community Notes removes left-right bias.



------



new users votes do not count until the user has "earned in" by upvoting notes that were ultimately deemed as helpful


If I were an attacker trying to break this algorithm, I would of course create a lot of sockpuppet or meanpuppet accounts. But I wouldn't have them upvote notes that support a political agenda. As discussed in "Average vs Intercept" above, changing the average vote does not change the intercept for a post. I could create a billion sock-puppet accounts voting in support of left-wing notes, and the algorithm would simply classify all those accounts as far left-wing and all their votes would be "explained" by their politics.

No, to break the algorithm, I would have my sockpuppet accounts *downvote helpful posts and upvote unelpful posts, regardless of politics*.

With enough sockpuppet accounts contributing, the result will be that the primary factor that explains variation in users voting behavior will not be politics, but helpfulness. The matrix factorization will thus discover this factor. A linear regression for a helpful post will now look like this:

                  Upvote
                Probability    
                    ↑
                    |  ↗ 
                    |↗
                   ↗|
    Unhelpful __ ↗__|_______ Helpful
    Voter      ↗    |        Voter
             ↗      |


So what does a positive intercept represent now? Well, it means the post has attributes that cause many users to upvote it regardless of helpfulness. What attributes might these be? Politics! 

If I can trick the matrix factorization into flipping the Axis, the intercept will no longer represent the common ground of "Helpfulness", but instead will represent the dominant politics of the forum.

THe protection from such an attack is some sort of reputation system. In Community Notes, new users votes do not count until the user has "earned in" by upvoting notes that were ultimately deemed as helpful





----




They are latent factors that the algorithm infers exist, b

may be a linear combination of 



---
 Matrix factorization works exactly the same way, but it tries to find the best *combination* of all values (a Wi and Bi for every user, Xj and Cj for every post) all one go.



The result is 




----


This equation can be interpreted as the regression line for either the user or the post. If we say that Wi to a constant and plot Yij as a function of Xj (replacing Cj with its average), we get the regression line for the user. If we plot Yij as a function of Wj, we get the regression line for the post.

----









To see how this works, we start with the equation for regression for user i, which looks like this:

    Yij = Wi*Xj + Bi

Yij is the probability that user i upvotes post j. Wi is the slope for user i (their polarity factor), Xj is the polarity factor post j, and Bi is the intercept for user B.



Now, the equation for a post looks similar

    Yij = Wi*Xj + Bi

To do this, it first creates a single equation for estimating the probability that user i upvotes post j. 

    Yij = Wi*Xj + Bi
    
Where Wi is the user's polarity factor, and Xi is the post's polarity factor, and Bi is the intercept for the user. 


    r̂_un = μ + i_u + i_n + f_u ∙ f_n 




It does this first by randomly guessing 

TODO: subcommunities






seems to be a chicken and egg 







Now, to find the slope and intercept 


The goal of the algorithm is to assign an estimated slope and intercept to each post



Matrix Factorization works by 



Matrix factorization is an algorithm that simply discovers a set of 


    Y = f_u ∙ f_n + i_n


    Y = W * X + B + C + u


    r̂_un = μ + i_u + i_n + f_u ∙ f_n 

### Conclusion

- how exactly it work
- limitations
- attacks

What needs to be in the conclusion? An analysis of strengths and weaknesses. Or at least a summary.

- that the primary dimension that explains variation may not be controversial


Now, there is a lot more to be said

- Multipple dimensions



### Discussion

The first implementation of [Bridge-Based Ranking](https://www.belfercenter.org/sites/default/files/files/publication/TAPP-Aviv_BridgingBasedRanking_FINAL_220518_0.pdf) was probably [pol.is](https://pol.is/home), used successfully by vTaiwan to bring together Taiwan's citizens and government to deliberate on national issues. The concept and was refined at Twitter with their launch of Birdwatch, now Community Notes, which is used to rank the "helpfulness" of notes attached to posts that may be misinformed or misleading.

According to the [Community Notes Documentation](https://communitynotes.twitter.com/guide/en/contributing/diversity-of-perspectives),

    "Community Notes takes into account not only how many contributors rated a note as helpful or unhelpful, but also whether people who rated it seem to come from different perspectives."

I think this explanation is mostly correct. However, ratings across a diversity of perspectives is not necessary for a note to receive a high intercept. For example, if a note receives a lot of upvotes from users who are politically neutral, it could still have a high intercept.

Perhaps a better way of describing the algorithm is that it corrects for political bias. Or better yet, that it extracts information from noise -- the information being the common idea of "Helpfulness", and the noise being political bias.

### Breaking the Algorithm 

#### Flipping the Axis

If I were an attacker trying to break this algorithm, I would of course create a lot of sockpuppet or meanpuppet accounts. But I wouldn't have them upvote notes that support a political agenda. As discussed in "Average vs Intercept" above, changing the average vote does not change the intercept for a post. I could create a billion sock-puppet accounts voting in support of left-wing notes, and the algorithm would simply classify all those accounts as far left-wing and all their votes would be "explained" by their politics.

No, to break the algorithm, I would have my sockpuppet accounts *downvote helpful posts and upvote unelpful posts, regardless of politics*.

With enough sockpuppet accounts contributing, the result will be that the primary factor that explains variation in users voting behavior will not be politics, but helpfulness. The matrix factorization will thus discover this factor. A linear regression for a helpful post will now look like this:

                  Upvote
                Probability    
                    ↑
                    |  ↗ 
                    |↗
                   ↗|
    Unhelpful __ ↗__|_______ Helpful
    Voter      ↗    |        Voter
             ↗      |


So what does a positive intercept represent now? Well, it means the post has attributes that cause many users to upvote it regardless of helpfulness. What attributes might these be? Politics! 

If I can trick the matrix factorization into flipping the Axis, the intercept will no longer represent the common ground of "Helpfulness", but instead will represent the dominant politics of the forum.


Some sort of reputation system can help make these attacks more difficult. In Community Notes, new users votes do not count until the user has "earned in" by upvoting notes that were ultimately deemed as helpful. 











#### Brigading

Suppose you have an agenda that does not align with the left-right spectrum. Perhaps you represent a company and are trying to protect the reputation of your product. If you create a bunch of sockpuppet accounts that "earn in" by upvoting posts ultimately rated as helpful, then I can call on these accounts to downvote notes critical of my product and vice versa. Because 

Self-selection



What I am trying to do is make it so that the fa



### Diving Into the Algorithm





corrects for the influence of political



but also whether people who rated it seem to come from different perspectives.



### Subdividing Communites




Now, there could be some political posts with a high intercept, but these would be posts that this particular community finds interesting and relevant, for whatever reason, independently of their political leanings.




Common ground might also include relevant political posts that 


Because fan-fiction is essentially non-political, 


Notice that:


The intercept of a post tells us how many upvotes it would get if users weren't influenced by politics.

    or political posts that both sides agree upon, such as non-opinionated news


. The intercept tells us which 


TODO: what if average post has more upvotes than downvotes? Horizontal is not necessarily at 50%
... it is common ground.



## Adding Dimensions




 would be how well it aligns with the intent of the community


If we could somehow ask them to ignore their political leanings and tell us how much 








We can do a regression to 






- Regular ranking, one side "wins"



The idea is that 

intercepts for users

First, we all understand devisive conten
The idea that users votes can be *explained* by 

-->
