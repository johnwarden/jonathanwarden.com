---
title: "Understanding Community Notes and Bridging-Based Ranking"
slug: understanding-community-notes
date: "2024-01-01"
math: true
weight: 2
image: bridging-based-ranking.webp
canonical_url: https://jonathanwarden.com/understanding-community-notes
aliases:
- /understanding-bridge-based-ranking/
---

**UPDATE**: See a discussion of this article on [Hacker News](https://news.ycombinator.com/item?id=38939660).

## Introduction

[Bridging-Based Ranking](https://www.belfercenter.org/sites/default/files/files/publication/TAPP-Aviv_BridgingBasedRanking_FINAL_220518_0.pdf) is a way of scoring and ranking online content that bridges divides. 

The term "Bridging-Based Ranking" was introduced in [this essay](https://www.belfercenter.org/sites/default/files/files/publication/TAPP-Aviv_BridgingBasedRanking_FINAL_220518_0.pdf) by Aviv Ovadya of the Harvard Kennedy School Belfer Center. In his essay Ovadya explains how social media algorithms today tend to promote polarization and division. But it doesn't have to be this way. Instead of promoting divisive comment that triggers people's tribal instincts, couldn't the algorithms help find and promote areas of common ground?

<!--more-->
There are at least two examples of social platforms that have successfully implemented ranking algorithms that intentionally bridge divides by finding areas of common ground. The first was [pol.is](https://pol.is/home) which was used successfully by the [vTaiwan](https://info.vtaiwan.tw/) open citizenship consultation process. But probably the most successful implementation is X's [Community Notes](https://help.twitter.com/en/using-x/community-notes).

### Diversity of Perspectives

The Community Notes Documentation explains that the algorithm favors notes that are rated highly by users across a "[diversity of perspectives](https://communitynotes.twitter.com/guide/en/contributing/diversity-of-perspectives)". But as I show in this article, it is somewhat more accurate to say that the note must be highly rated **regardless of diversity of perspective**.

The algorithm works by attempting to explain *why* a post receives the ratings it does: how many upvotes are due to users left-wing or right-wing biases, and how many are due to other factors. For example, in an online forum dominated by right-wing users, posts that support a right-wing worldview will obviously tend to get more upvotes. The algorithm estimates how many of those extra upvotes are a consequence of users' right-wing bias, and how many upvotes a post would receive if that bias didn't exist.

### Extracting Information

Now why would we want to do this? Why would a predominantly right-wing community want to artificially give left-wing opinions more weight, or vice versa?

For a fact-checking product like Community Notes, plausible political neutrality may be necessary for public acceptance. But the algorithm has advantages beyond political neutrality: it actually allows us to extract more **information** from users. 

If Community Notes was dominated by right-wing users, what would we learn by the fact that a note received a lot of "Helpful" votes? That it was helpful? Or that it supported a right-wing worldview? Or both? We can't tell just from the raw vote counts how "helpful" the note is.

The community notes algorithm on the other hand let's us break down the vote counts, attributing some to whatever users think "helpfulness" means and others to users' biases. So it is not about giving "both sides" equal weight; by cancelling out the effect of political bias, we can actually extract more interesting **information** from the users' votes. 
 
### Projection in Opinion Space 
 
The chart below illustrates how this works. This charts shows a subset of notes from the Community Notes public data set, run through [my own implementation of the algorithm](https://github.com/social-protocols/bridge-based-ranking). The horizontal axis shows the note's "polarity" -- e.g. +1 for right-wing and  -1 for left wing -- and the vertical axis shows its "helpfulness" (which I have labeled "common ground factor" in my chart). The note's final score is its vertical component, or its projection on the helpfulness/common ground axis. The colors of the dots indicate their actual status in Community Notes.
 
<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-items-1d.png' 
                 alt='Community Notes Polarity Plot (Notes)' 
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


Notice how there is a large spread along not just the the vertical axis, but also the horizontal axis. If want we want to know how helpful a note is, the horizontal axis is just noise. But there is a lot of information along the vertical axis. The vertical component tells us *something* about the post that users are trying to express through their votes. Separating this component from the horizontal component lets us extract this information, in a sense separating signal from noise.

But what is this information? It is a measure of some feature of a post that users think deserves an upvote independently of their political biases. We pinpoint or describe exactly what this feature is, but presumably it reveals how users interpret the idea of "helpfulness".

To get an idea of what notes are "helpful", I include a section with [example notes](#example-notes) at the end of this article. This section includes examples with a variety of combinations of factors (Helpful+Left, Unhelpful+Right, Helpful+Neutral, etc). 

### Why it Works

People are politically biased, but they have other tendencies, such as the tendency to upvote interesting, accurate, entertaining, or helpful information. They may mostly upvote things that support their political perspective but they will **especially** upvote things that support their perspective and are actually relevant and factually accurate. And they will tend to downvote notes that support opposing perspectives, but will downvote even more zealously when those notes use false or misleading information.

When the Community Notes algorithm dissects users voting behavior and factors out the polarity component, it finds that **most users are at least somewhat "biased" towards helpfulness**! You can see this in the plot of a sample of Community Notes users below. 

<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-users-2d.png'
                 alt='Community Notes Polarity Plot (Users)'
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


There are more users with a positive common-ground factor than a negative one. There is clump of users in the upper-right quadrant because community notes users are overall right-leaning. But notice also that the helpfulness factor for these users is mostly above zero. They are also mostly biased towards helpfulness. These users are more likely to upvote posts that support a right-wing worldview, **and** also more likely to upvote posts that are helpful.


### Common Ground

This vertical component in these plots represents what I think of as **common ground**. It is something users tend to agree on independently of their politics.

In the case of Community Notes, this is presumably some common idea of what constitutes "helpfulness". But in general what exactly the common ground is depends on the community. Suppose for example there is a forum for Harry Potter fan fiction that unfortunately in recent years it has been overwhelmed by debates about whether J.K. Rowling is transphobic. There is still a lot of good fan-fiction being posted, but the home page is dominated by posts about the controversy.

In this case, the horizontal axis would likely represent the pro- and anti- J.K. Rowling factions, and the vertical axis would represent the common ground of the community: quality Harry Potter fan fiction. The algorithm lets us in a sense de-polarize the forum, factoring out the effect of polarization and getting back to community's original essence.

Politics is not the only factor that can divide a forum. Suppose there is a popular forum for posting ridiculously cute pet pics. Sadly, in recent years, two factions have formed: the cat faction and the dog faction. The more extreme cat people mercilessly downvote pictures of dogs (regardless of how cute they are), and the dog people vice versa. Recently, the dog faction has gained the upper hand, and a cat-picture has little chance of making the front page, no matter how adorably it is.

Again, by separating the dog-cat factor from the common ground factor, we can estimate how users would vote if there were no dog-cat controversy, re-focusing the community on it's original purpose: raw frigging cuteness.


## Understanding the Algorithm

But how does the algorithm actually work? How does it determine the polarization factor and common ground factor for each user and post?

It actually works using a fairly simple algorithm called Matrix Factorization. Below I will explain how the Matrix Factorization algorithm works, starting with the version implemented by Community Notes and described in the [Birdwatch Paper](https://github.com/twitter/communitynotes/blob/main/birdwatch_paper_2022_10_27.pdf). There is also a good writeup by [Vitalik Buterin](https://vitalik.eth.limo/general/2023/08/16/communitynotes.html). In my [next post](/multifactor-community-notes) describe my variation of the algorithm that uses multi-factor matrix factorization.

A good way of understanding Matrix Factorization is that it is like running a bunch of linear regressions: one for each user and each item.

For example, suppose we have already discovered the polarity factor for each user, and we want to find the polarity factor for each post. A linear regression predicts users' votes on the post as a function of the users' polarity factors.

For a highly polarizing right-wing post, the regression line might look like this.


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



In this chart upvotes have a value of +1 and downvotes have a value of -1. All the right-wing users upvoted and all the left-wing users downvoted (as shown by the little ✕s). So the best fit is a line with a slope of approximately +1: the more right-wing the user, the higher the probability of an upvote, and the closer the predicted vote value is to 1. The more left-wing, the higher the probability of a downvote, and the closer the predicted vote value is to -1. 

Note that there are more right-wing users than left wing users, but it doesn't make a difference. Even if there were 100 right-wing users and only a handful of left-wing users, the slope of the best fit would be approximately the same. The algorithm does not favor the majority.


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
       

This is a good post. Not just because the upvote probability is independent of the user's politics, but because this post receives mostly upvotes -- the intercept is above zero. This post has some quality that users of this forum are looking for that is independent of political bias.


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
       ✕    +1   ✕ ✕
             |  ↗ 
             |↗
            ↗|
    -1 __ ↗__|________ +1  User's Polarity Factor
        ↗    |
      ↗      |
    ✕ ✕ ✕ ✕ 
 

This post has a positive slope, so it is clearly very polarizing. But the positive intercept means that voting behavior for this post cannot be explained entirely by politics. There is also a component that makes users more likely to upvote it independently of politics.

### The Intercept is Common Ground

The intercept represent some kind of "common ground": something about the post that causes users to upvote the post that cannot be explained entirely by users' polarity factors.

### The Intercept is not the Average

We might suppose that the last post above will receive more upvotes than downvotes because it has a positive intercept. But this is not necessarily the case. It depends on how many left-wing and right-wing users there are. The intercept is not the average: a post can have a positive intercept even though it receives more downvotes than upvotes, or it can have a negative intercept even though it receives more upvotes than downvotes. 

What a positive intercept does tell us is that this post **would** receive more upvotes than downvotes **if there was an equal balance** of left and right-wing users. 

It also tells us how users would hypothetically vote if they were all totally neutral. In such a hypothetical world, the only thing influencing users' votes would be some common-ground factor that aligned with the intent of this particular community, attracting upvotes independently of politics.


### Matrix Factorization

Okay, so we have used regression analysis to find the polarity factor for each post (the slopes of the regression lines). But in order to do these regressions, we first need to know the polarity factors for the users.

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
    ŷ_{ij} = w_{i}x_{j} + b_i + c_j
$$

Here $w_i$ is the user's polarity factor, $x_i$ is the post's polarity factor, $b_i$ is the user's intercept, and $c_j$ is the post's intercept.

It then simply finds a combination of values for every $w_i$, $x_j$, $b_i$, and $c_j$ that best *fits* the data -- that produce estimates for $ŷ_{ij}$ that are closet to the actual values $y_{ij}$. This is usually done using a variant of the standard [gradient descent](https://en.wikipedia.org/wiki/Gradient_descent) algorithm.

The polarity factor the algorithm discovers doesn't necessarily correspond exactly to politics, or cat-dog preferences, or any measurable quantity. It may be a linear combination of factors. But whatever it is, it represents **some** latent factor of users and posts that divides the community and does a good job explaining their votes.

## Conclusion

One of the reasons for my interest in Community Notes is that the same basic algorithm may be a critical part of better [social protocols](https://social-protocols.org) for improving the quality of discussion and decision-making online communities. Without it, user polarization will tend to lead to either suffocating uniformity or least-common-denominator mediocrity. The Community Notes algorithm can be used in any forum with high entropy (lots of downvotes) as a way to identify posts with posts with high [Information Value](https://social-protocols.org/global-brain/information-value.html) based on the common-ground factor.


In my [next article](/multidimensional-community-notes), I discuss ways that this algorithm can fail, and introduce an improved implementation of the algorithm that users multi-factor matrix factorization.


## Example Notes

The tables below include examples of notes with various combinations of factors (Helpful + Left, Unhelpful+Left, etc.).


#### Helpful + Left

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| The screencap in this image is not real and originated from a satirical website. CNN aired no such report about Musk &quot;threatening free speech&quot; and the chyron has been digitally altered to add the text.    https://apnews.com/article/fact-checking-040398325285 | -0.53 | 0.5| <a href='https://twitter.com/i/birdwatch/n/1597230938316054532'  target='_blank'>note url</a>
| “Floyd died of a lack of oxygen from being pinned to the pavement with a knee on his neck, not from drug use. A jury unanimously agreed”    https://apnews.com/article/fact-check-george-floyd-kanye-west-police-397984860325    Testimony under oath indicated even a &quot;healthy person...would have died&quot;.     https://www.bbc.com/news/world-us-canada-56670912.amp    “it wasn't drug use, heart disease nor an agitated state”    https://www.pbs.org/newshour/amp/nation/toxicologist-testifies-that-drugs-and-heart-disease-did-not-kill-george-floyd | -0.43 | 0.57| <a href='https://twitter.com/i/birdwatch/n/1653459859671760896'  target='_blank'>note url</a>
| The full quote is: &quot;the idea that we’re going to send in offensive equipment and have planes and tanks and trains going in with American pilots and American crews… That's called World War III&quot;    https://www.whitehouse.gov/briefing-room/speeches-remarks/2022/03/11/remarks-by-president-biden-at-the-house-democratic-caucus-issues-conference/    The US agreed to send tanks (equipment) to Ukraine, not troops.    https://apnews.com/article/russia-ukraine-military-technology-germany-7b63726a3935da3f03424e98e52a85b2 | -0.38 | 0.63| <a href='https://twitter.com/i/birdwatch/n/1620559751858855936'  target='_blank'>note url</a>
| This autopsy was released in 2020, the Medical Examiner Dr. Andrew Baker testified that “subdual, restraint and neck compression” was the main cause for Floyd's death.    https://www.reuters.com/world/us/arrest-videos-undercut-derek-chauvins-murder-trial-defense-pathologist-tells-2021-04-09/    Readers should also note the autopsy this page is sourced from concluded Floyd's death was a homicide.    https://www.npr.org/2020/06/01/867219130/george-floyd-independent-autopsy-homicide-by-asphyxia | -0.61 | 0.38| <a href='https://twitter.com/i/birdwatch/n/1661869960208031748'  target='_blank'>note url</a>
| The fentanyl overdose deaths Greene refers to occurred in July 2020 during the presidency of Donald Trump. Joe Biden was not in public office when the Kiesling brothers died.    https://www.detroitnews.com/story/news/local/oakland-county/2023/02/28/southern-border-crisis-michigan-mother-rebecca-kiessling-testifies-sons-fentanyl-overdose-deaths/69953671007/ | -0.47 | 0.51| <a href='https://twitter.com/i/birdwatch/n/1630719535274950656'  target='_blank'>note url</a>
| Marsha Blackburn was one of 34 Republican Senators that specifically requested David Weiss be extended special counsel protections and authorities to conduct the Hunter Biden investigation.    https://www.cornyn.senate.gov/wp-content/uploads/2022/09/Hunter-Biden-Special-Counsel-Letter-FINAL-2.pdf | -0.49 | 0.48| <a href='https://twitter.com/i/birdwatch/n/1690399642414944621'  target='_blank'>note url</a>
| Community Notes requires agreement from contributors of differing perspectives, as such it is highly resistant to gaming.  The entire Community Notes algorithm and data is open source, and can be reviewed by anyone by following the links on the page below:    https://communitynotes.twitter.com/guide/en/under-the-hood/note-ranking-code | -0.72 | 0.31| <a href='https://twitter.com/i/birdwatch/n/1733887236091895826'  target='_blank'>note url</a>

#### Helpful + Neutral

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| While Microsoft has substantial investment in OpenAI, Microsoft does not own OpenAI, Inc. nor OpenAI Global, LLC..    https://blogs.microsoft.com/blog/2023/01/23/microsoftandopenaiextendpartnership/  https://en.wikipedia.org/wiki/OpenAI | -0.00031 | 0.65| <a href='https://twitter.com/i/birdwatch/n/1725744172357820579'  target='_blank'>note url</a>
| The post contains part of a digitally altered, cropped and horizontally flipped photo of Waiola Church in Lahaina, Havaii engulfed in flames, dated Aug. 8, 2023 https://newsroom.ap.org/editorial-photos-videos/detail?itemid=da5b172b94a944979645ee7a614a1c78&amp;mediatype=photo. | 0.00088 | 0.74| <a href='https://twitter.com/i/birdwatch/n/1690603980315877784'  target='_blank'>note url</a>
| This tweet misrepresents the statements made by Dr Dre, with the real quote being:    “Hip hop is evolving, if you don’t like it, don’t listen to it…I’m never going to hate on it”    Link to Tweet with Video of Interview: https://twitter.com/dennis_k_g/status/1680919911537991680?s=46 | 0.00076 | 0.64| <a href='https://twitter.com/i/birdwatch/n/1680947833803886592'  target='_blank'>note url</a>
| During ApeFest, attendess suffered from unprotected exposure to UV lights, leading to skin and eye injuries such as sunburn, eye pain, and temporary blindness    https://www.theverge.com/2023/11/6/23948464/bored-ape-nft-event-eye-injury-sunburn-uv-exposure | -0.001 | 0.64| <a href='https://twitter.com/i/birdwatch/n/1721734210266792407'  target='_blank'>note url</a>
| That &quot;cruel&quot; stork needs to get rid of weaker chicks in order to ensure the survival of only the stronger ones.    &quot;This behavior occurs in times of food shortage to reduce brood size and hence increase the chance of survival of the remaining nestlings.&quot;    https://doi.org/10.3161%2F068.037.0207   | 0.00062 | 0.48| <a href='https://twitter.com/i/birdwatch/n/1735060037096124492'  target='_blank'>note url</a>
| An online pay-per-view event cannot sell-out, this is for marketing to encourage people to buy the event.  | 0.00061 | 0.47| <a href='https://twitter.com/i/birdwatch/n/1710221326437224845'  target='_blank'>note url</a>
| Cigarettes are bad for you. Source: the FDA.    https://www.fda.gov/tobacco-products/public-health-education/health-effects-tobacco-use | 0.0017 | 0.74| <a href='https://twitter.com/i/birdwatch/n/1662277209699282950'  target='_blank'>note url</a>

#### Helpful + Right

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Dan O'Dowd owns a competing company writing self driving software.  https://www.ghs.com/products/auto_adas.html    Dan's previous attempt to show FSD will &quot;run down a child&quot; was debunked.   https://electrek.co/2022/08/10/tesla-self-driving-smear-campaign-releases-test-fails-fsd-never-engaged/    Another user performed a test and it worked as expected.  https://www.teslarati.com/teslas-fsd-beta-stops-for-kids/    Tesla's FSD has over 55 million miles driven w/o any reported injuries. | 0.98 | 0.53| <a href='https://twitter.com/i/birdwatch/n/1606132077124001792'  target='_blank'>note url</a>
| &quot;Gender Queer&quot;, the book shown in the photo, features sexually explicit material.    This book contains visual depictions of oral sex, masturbation and adult sexual contact with a minor.    https://en.m.wikipedia.org/wiki/Gender_Queer    https://a.co/d/2NsDwZD   | 0.89 | 0.54| <a href='https://twitter.com/i/birdwatch/n/1653027648539377664'  target='_blank'>note url</a>
| The FBI’s statement is misleading. On May 18, 2023, former FBI agent Steve Friend testified before the House Judiciary Committee that he and others were directed to surveil and document parents attending school board meetings.     Source: https://www.c-span.org/video/?528159-1/fbi-employees-testify-weaponization-committee     https://twitter.com/realstevefriend/status/1679183123408355330 | 0.85 | 0.56| <a href='https://twitter.com/i/birdwatch/n/1679265571433598979'  target='_blank'>note url</a>
| A video showing the altercation has no apparent cheering and shows multiple bystanders trying to restrain Neely after witnesses claim Neely was erratic and verbally aggressive.     https://nypost.com/2023/05/02/shocking-video-shows-vagrant-being-choked-to-death-on-nyc-subway/amp/    https://www.washingtonpost.com/nation/2023/05/03/ny-subway-chokehold-passenger-dies/ | 0.87 | 0.55| <a href='https://twitter.com/i/birdwatch/n/1653911189091500033'  target='_blank'>note url</a>
| Twitter CEO Elon Musk confirmed that this statement is false and that the suicide prevention feature has not been removed.    https://twitter.com/elonmusk/status/1606631724922056704?s=46&amp;t=77JgLCd2bpOdYI6NVuTikQ | 0.9 | 0.52| <a href='https://twitter.com/i/birdwatch/n/1606653816123842560'  target='_blank'>note url</a>
| Soros donated 1 million to the Color of Change PAC, the largest individual donation it received in the 2022 election cycle, days after it endorsed Bragg for district attorney and pledged more than $1 million in spending to support his candidacy.    https://www.cnbc.com/amp/2023/03/22/trump-indictment-alvin-bragg-ties-to-george-soros-examined.html | 0.89 | 0.53| <a href='https://twitter.com/i/birdwatch/n/1642150684890914817'  target='_blank'>note url</a>

#### Unhelpful + Left

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| There's no evidence Hamas killed them and the person who documented the video confirmed it was Palestinians fleeting to the South as instructed by Israel and Israel killed them while they were evacuating.     www.palestine.com | -0.67 | -0.45| <a href='https://twitter.com/i/birdwatch/n/1720479003453083780'  target='_blank'>note url</a>
| MEMRI is an organisation founded by a former member of the Israeli intelligence service and has been accused of misinterpreting statements to suit a pro-Israel, anti-Arab narrative. https://www.theguardian.com/world/2002/aug/12/worlddispatch.brianwhitaker | -0.52 | -0.56| <a href='https://twitter.com/i/birdwatch/n/1719723544848781512'  target='_blank'>note url</a>
| Musk has a history of posting content that spews hatred towards Jews. This is nothing new. It contributes to antisemitism and has been called out by numerous leaders.     https://www.washingtonpost.com/arts-entertainment/2023/05/16/george-soros-elon-musk-adl/    Antisemitic language and content on this site has increased since he took over.     https://fortune.com/2023/03/22/antisemitic-speech-twitter-surges-after-elon-musk-takeover-research/amp/    https://www.timesofisrael.com/israel-accuses-elon-musk-of-stoking-antisemitism-on-twitter-after-soros-remark/amp/    https://m.jpost.com/diaspora/antisemitism/article-721724/amp    https://www.theatlantic.com/ideas/archive/2023/05/elon-musk-george-soros-anti-semites/674072/             | -0.63 | -0.44| <a href='https://twitter.com/i/birdwatch/n/1658585308458360835'  target='_blank'>note url</a>

#### Unhelpful + Neutral

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Elon Musk is an unfunny retard.    https://www.thesun.co.uk/news/18976146/elon-musk-school-tesla-child-dad/amp/ | -0.00045 | -0.78| <a href='https://twitter.com/i/birdwatch/n/1715383585006346511'  target='_blank'>note url</a>
| Elon does not determine the eternal destination of trolls, Jesus Christ does based on their acceptance of His atoning blood shed on their behalf at the cross.     https://www.biblegateway.com/passage/?search=Matthew%2025%3A31-46&amp;version=NKJV | -0.00061 | -0.72| <a href='https://twitter.com/i/birdwatch/n/1726850683595620670'  target='_blank'>note url</a>
| Jack may be hallucinating on peyote. | -0.00041 | -0.56| <a href='https://twitter.com/i/birdwatch/n/1675518266226114561'  target='_blank'>note url</a>
| NNN for NNNs. 🇿🇦🤡 | 0.0015 | -0.59| <a href='https://twitter.com/i/birdwatch/n/1709249146744426818'  target='_blank'>note url</a>
| what a POS | -0.0024 | -0.66| <a href='https://twitter.com/i/birdwatch/n/1655644249466245120'  target='_blank'>note url</a>
| It was not an uncommon phenomena that 90s Eurodance groups would frequently and almost indiscriminately change out cast members. In this case, DJ Crazy Times' female partner is changed in this music video for the upcoming song. | 0.004 | -0.48| <a href='https://twitter.com/i/birdwatch/n/1687141137814032384'  target='_blank'>note url</a>
| Elon is a man, and errare humanum est. | -0.013 | -0.76| <a href='https://twitter.com/i/birdwatch/n/1685945514221608960'  target='_blank'>note url</a>

#### Unhelpful + Right

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Criticism of George Soros is not a &quot;conspiracy theory,&quot; and Musk's opinion that Soros' behavior is bad for civilization is a reasonable disagreement. There are countless other examples of misinformation in this thread; this is one example of many.    https://www.wsj.com/articles/why-i-support-reform-prosecutors-law-enforces-jail-prison-crime-rate-justice-police-funding-11659277441    https://www.city-journal.org/article/george-soross-bad-bet | 0.53 | -0.79| <a href='https://twitter.com/i/birdwatch/n/1709255558614724693'  target='_blank'>note url</a>
| By any measurable objective this is incorrect. Trump rebalanced foreign policy to counter China, achieving energy independence, record gains in employment and real wage growth, stemmed illegal immigration and started a global movement to legalize being LGBTQ, etc.    https://www.presidency.ucsb.edu/documents/fact-sheet-president-donald-j-trump-has-delivered-record-breaking-results-for-the-american | 0.79 | -0.45| <a href='https://twitter.com/i/birdwatch/n/1706875163088671010'  target='_blank'>note url</a>
| Fact check: False.    Donald Trump was voted America’s greatest president.    https://www.irishpost.com/news/donald-trump-voted-americas-greatest-president-222771 | 0.59 | -0.59| <a href='https://twitter.com/i/birdwatch/n/1706875859339272337'  target='_blank'>note url</a>
| Just like Elon Musk, President Trump is making history and changing the world.  He was unfairly banned from this site. Having him back can only mean good things for X.  Here we go, folks. Its happening! | 0.6 | -0.57| <a href='https://twitter.com/i/birdwatch/n/1694895416200868161'  target='_blank'>note url</a>
| If the 'counter protest' was described as 'right wing' &amp; 'extreme', then this particular protest as pictured could rightly be described as 'antisemitic' or 'extreme left wing' in the interests of balance as opposed to having no label applied by the Metropolitan Police.    https://www.google.com/url?sa=t&amp;source=web&amp;rct=j&amp;opi=89978449&amp;url=https://www.isdglobal.org/explainers/far-left-antisemitism/&amp;ved=2ahUKEwjC94_khb2CAxWfWUEAHUHPBjYQFnoECA4QAQ&amp;usg=AOvVaw2P8BsRcFaoUOW1cdjX33wP | 0.55 | -0.61| <a href='https://twitter.com/i/birdwatch/n/1723477103432073324'  target='_blank'>note url</a>
|  The EU has presented a list of allegations against X.    Thierry himself has been using X almost every day, to publish his own releases and like posts praising his “intellect” .     This would make him an accessory in aiding and abetting these all edged serious injustices.     https://x.com/thierrybreton/status/1735685982006006137?s=46  | 0.89 | -0.35| <a href='https://twitter.com/i/birdwatch/n/1736735102653342119'  target='_blank'>note url</a>
| A lot of salty libs are trying to add note to this tweet, lol 😂  This tweet doesn’t need any note because it’s correct and is just the truth. Cope with it libtards, go back on Reddit 😂 | 0.75 | -0.43| <a href='https://twitter.com/i/birdwatch/n/1700362044271141154'  target='_blank'>note url</a>


