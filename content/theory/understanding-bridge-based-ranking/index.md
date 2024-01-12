---
title: "Understanding Bridge-Based Ranking"
date: "2024-01-01"
math: true
image: https://cdn.discordapp.com/attachments/1097595489282703571/1194709292775911589/alphaomega1551_Two_groups_of_people_on_opposite_sides_of_the_wa_1c90a2c7-2f57-467a-910b-be12aa5d5f87.png?ex=65b156bc&is=659ee1bc&hm=c91b61ec3c3fcf7de856b308313b0ebab93df4782a170ae108878fe1dfedfe3e&

---

## Introduction

[Bridge-Based Ranking](https://www.belfercenter.org/sites/default/files/files/publication/TAPP-Aviv_BridgingBasedRanking_FINAL_220518_0.pdf) is an alternative way to score and rank content by **adjusting for user polarization**.

The most successful implementation of Bridge-Based Ranking, X's Community Notes, explains that the algorithm favors notes that are rated highly by users across a "[diversity of perspectives](https://communitynotes.twitter.com/guide/en/contributing/diversity-of-perspectives)". But as I show in this article, ratings from users with diverse perspectives are not necessary for a note to rank highly. It is somewhat more accurate to say that the note must be highly rated **regardless of diversity of perspective**.

The algorithm works by attempting to model *why* a post receives the ratings it does: how many upvotes are due to users left-wing or right-wing biases, and how many are due to other factors. If a post is only appealing to right-wing voters, and an online forum is dominated by right-wing voters, then that right-wing bias probably explains why it gets so many upvotes. So the algorithm tries to correct for this bias, and estimate how many upvotes a post would receive if there was an equal balance of left-wing and right-wing voters.

### Extracting Information

Now why would we want to do this? Why for example would a predominantly left-wing community want to artificially give right-wing opinions more weight, especially if they think their own side is better-informed?

For a fact-checking product like Community Notes, plausible political neutrality may be necessary for public acceptance. But bridge-based ranking has advantages beyond political neutrality: it actually allows us to extract more **information** from users. 

In Community Notes, users rate notes as "Helpful" or "Not Helpful". If Community Notes was dominated by leftists, what would we learn by the fact that a note received a lot of "Helpful" votes? That it is helpful? Or that it supports a left-wing worldview? Or both? It's hard to say: perhaps a marginally helpful note that supports a left-wing world view gets more votes than a truly helpful note that supports a right-wing world-view. We can't tell just from the raw vote counts how "helpful" the note is.

Bridge-based ranking on the other hands let's us break down the vote counts, attributing some to whatever users think "helpfulness" means and others to polarity. So it is not about giving "both sides" equal weight; by cancelling out the effect of political bias, we can actually extract more interesting **information** from the users' votes. 
 
### Projection in Opinion Space 
 
The chart below illustrates how this works. This charts shows a subset of notes from the Community Notes public data set, run through [my own implementation of the algorithm](https://github.com/social-protocols/bridge-based-ranking). The horizontal axis shows the note's "polarity" -- e.g. +1 for right-wing and  -1 for left wing -- and the vertical axis shows its "helpfulness". The note's final score is its vertical component, or its projection on the "helpfulness" axis. The colors of the dots indicate their actual status in Community Notes.


 
<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-items-1d.png' 
                 alt='Community Notes Polarity Plot (Notes)' 
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


Notice how there is a large spread along not just the the vertical axis, but also the horizontal axis. If want we want to know how helpful a note is, the horizontal axis is just noise. But there is a lot of information along the vertical axis. The vertical component tells us *something* about the post that users are trying to express through their votes. Separating this component from the horizontal component lets us extract this information, in a sense separating signal from noise.

But what is this information? It is a measure of some feature of a post that users think deserves an upvote independently of their political biases. What exactly this feature is is impossible to say, but presumably it reveals how users interpret the idea of "helpfulness".


At the end of this article, I include a section with [example notes](#example-notes) with a variety of factors (Helpful+Left, Unhelpful+Right, Helpful+Neutral, etc). 

### Why it Works

People are politically biased, but they have other biases, such as the bias towards interesting, accurate, entertaining, or helpful information. They may mostly upvote things that support their political perspective but they will **especially** upvote things that support their perspective and are actually relevant and factually accurate. And they will tend to downvote notes that support opposing perspectives, but will downvote even more zealously when those notes use false or misleading information.

When bridge-based ranking algorithm dissects users voting behavior and factors out the polarity component, it finds that **most users are at least somewhat biased towards helpfulness**! You can see this in the plot of a sample of Community Notes users below. 

<img src='https://raw.githubusercontent.com/social-protocols/bridge-based-ranking/main/plots/community-notes-large-users-2d.png'
                 alt='Community Notes Polarity Plot (Users)'
                 style='display: block; margin-left: auto; margin-right: auto; max-height: 500px' />


There is clump of users in the upper-right quadrant because community notes users are overall right-leaning. But notice also that the helpfulness factor for these users is mostly above zero. They are also mostly biased towards helpfulness. These users are more likely to upvote posts that support a right-wing worldview, **and** also more likely to upvote posts that are helpful.


### Common Ground

This vertical component in these plots represents **common ground**. It is something users tend to agree on independently of their politics.

In the case of Community Notes, this is presumably some common idea of what constitutes "helpfulness". But in general what exactly the common ground is depends on the community. Suppose for example there is a forum for Harry Potter fan fiction that unfortunately in recent years it has been overwhelmed by debates about whether J.K. Rowling is transphobic. There is still a lot of good fan-fiction being posted, but the home page is dominated by posts about the controversy.

In this case, the horizontal axis would likely represent the pro- and anti- J.K. Rowling factions, and the vertical axis would represent the common ground of the community: quality Harry Potter fan fiction. Using bridge-based ranking we can in a sense de-polarize the forum, factoring out the effect of polarization and getting back to community's original essence.

Politics is not the only factor that can divide a forum. Suppose there is a popular forum for posting ridiculously cute pet pics. Sadly, in recent years, two factions have formed: the cat faction and the dog faction. The more extreme cat people mercilessly downvote pictures of dogs (regardless of how cut they are), and the dog people vice versa. Recently, the dog faction has gained the upper hand, and a cat-picture has little chance of making the front page, no matter how frigging adorably it is.

Again, by separating the dog-cat factor from the common ground factor, we can re-focus the community on it's original purpose: raw frigging cuteness.


## Understanding the Algorithm

But how does the algorithm actually work? How does it determine the polarization factor and common ground factor for each user and post?

It actually works using a fairly simple algorithm called Matrix Factorization. Below I will explain how the Matrix Factorization algorithm works, starting with the version implemented by Community Notes and described in the [Birdwatch Paper](https://github.com/twitter/communitynotes/blob/main/birdwatch_paper_2022_10_27.pdf). There is also a good writeup by [Vitalik Buterin](https://vitalik.eth.limo/general/2023/08/16/communitynotes.html). In my [next post](/improving-bridge-based-ranking) describe my variation of the algorithm that uses multi-dimensional matrix factorization.

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



In this chart upvotes have a value of +1 and downvotes have a value of -1. All the right-wing users upvoted and all the left-wing users downvoted (as shown by the little ✕s). So the best fit is a line with a slope of approximately +1: the more right-wing the user, the higher the probability of an upvote, and the closer the predicted value is to 1. The more left-wing, the higher the probability of a downvote, and the closer the predicted value is to -1. 

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



## Example Notes

The tables below include examples of notes with various combinations of factors (Helpful + Left, Unhelpful+Left, etc.).


#### Helpful + Left

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Lira has violated Article 436-2 of Ukraine's Criminal Code.    He publicly denied the Bucha massacre and other RU attacks against civilians, blaming Ukraine, exposed the location of Western journalists and Ukrainian soldiers, including their faces, among other crimes.  https://www.lexology.com/library/detail.aspx?g=83d10eb2-cede-4417-9a3f-9a535949585f  https://www.thedailybeast.com/gonzalo-lira-red-pill-dating-coach-who-is-accused-of-shilling-for-putin-is-arrested-in-ukraine  https://news.yahoo.com/pro-russian-blogger-gonzalo-lira-172900103.html  https://archive.ph/t5sOW  https://www.bbc.com/news/60981238 | -0.73 | 0.68| <a href='https://twitter.com/i/birdwatch/n/1733797244619427891'  target='_blank'>note url</a>
| Kyiv is about 500km/310mi from the nearest active front line - far from being &quot;war torn&quot; - though there are nightly air raid alerts due to attacks  https://deepstatemap.live/en#6/49.246/34.893  https://liveuamap.com/  https://alerts.in.ua/en    Footage of people trying to live a normal life is frequently used by pro-Russian publishers to discourage western aid  https://observers.france24.com/en/europe/20230512-parties-in-kyiv-nightclubs-in-the-midst-of-war-watch-out-for-these-misleading-images  https://usatoday.com/story/news/factcheck/2022/09/21/fact-check-ukraine-war-real-despite-video-dancing-kyiv-bar/8004366001/ | -0.53 | 0.8| <a href='https://twitter.com/i/birdwatch/n/1688330206438678905'  target='_blank'>note url</a>
| The Secret Service does not provide protection to non-incumbent Presidential candidates until 120 days before the general election.    The next election is Nov. 5, 2024.  July 8, 2024 is 120 days prior.    https://www.secretservice.gov/about/faq/general | -0.57 | 0.73| <a href='https://twitter.com/i/birdwatch/n/1684955453963321344'  target='_blank'>note url</a>
| Fox network lawyers explain that the Tucker Carlson show is not factual: https://law.justia.com/cases/federal/district-courts/new-york/nysdce/1:2019cv11161/527808/39/     Mistakes during the audit were found.   https://sos.ga.gov/page/2020-general-election-risk-limiting-audit https://www.ajc.com/politics/georgia-investigation-finds-errors-in-fulton-audit-of-2020-election/BZ7D5JXOMRBPZIU4PNVYIHQZR4/    Georgia received several recounts &amp; audits requested by president Trump, all showing Biden won. https://apnews.com/article/election-2020-joe-biden-donald-trump-georgia-elections-4eeea3b24f10de886bcdeab6c26b680a https://www.cnn.com/2020/12/07/politics/georgia-recount-recertification-biden/index.html https://sos.ga.gov/news/historic-first-statewide-audit-paper-ballots-upholds-result-presidential-race    Trump's cabinet declared that the election was fair https://apnews.com/article/barr-no-widespread-election-fraud-b1f1488796c9a98c4b1a9061a6c7f49d   | -0.9 | 0.44| <a href='https://twitter.com/i/birdwatch/n/1723283633421586614'  target='_blank'>note url</a>
| Obama’s order 13603 does not involve the possibility to suspend the US Constitution. The order by Obama was a minor update of a similar order by President Clinton. The Defense Production Act has been in effect since the Truman Administration &amp; amended several times since  https://www.snopes.com/fact-check/national-defense-resources-preparedness/    https://www.presidency.ucsb.edu/documents/executive-order-13603-national-defense-resources-preparedness | -0.67 | 0.6| <a href='https://twitter.com/i/birdwatch/n/1672599133339672577'  target='_blank'>note url</a>
| Community Notes requires agreement from contributors of differing perspectives, as such it is highly resistant to gaming.  The entire Community Notes algorithm and data is open source, and can be reviewed by anyone by following the links on the page below:    https://communitynotes.twitter.com/guide/en/under-the-hood/note-ranking-code | -0.69 | 0.57| <a href='https://twitter.com/i/birdwatch/n/1733887236091895826'  target='_blank'>note url</a>
| People of all sexes and genders have breast tissue, and can get breast cancer.    https://www.cdc.gov/cancer/breast/men/index.htm    https://www.cancer.org/cancer/breast-cancer-in-men/about/what-is-breast-cancer-in-men.html     | -0.57 | 0.7| <a href='https://twitter.com/i/birdwatch/n/1642424873623646216'  target='_blank'>note url</a>

#### Helpful + Neutral

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| This video is not from Greece, but from a fire in Spain on July 2022. They are firefighters from the Zulú-2 brigade in Zamora and they speak in Spanish.    The video was viral back then and can be retrieved from several reliable media, like Levante-EMV,  20 minutos or RAC1:  https://www.levante-emv.com/buzzeando/2022/07/22/tiernas-imagenes-bomberos-dando-beber-corzo-68965340.amp.html    https://www.20minutos.es/noticia/5033552/0/un-grupo-de-bomberos-forestales-presta-auxilio-a-un-pequeno-corzo-deshidratado/    https://www.rac1.cat/societat/20220723/98793/video-bombers-ajuden-cabirol-cervol-sobreviure-aigua-incendi-zamora-losacio.amp.html | 0.0001 | 0.68| <a href='https://twitter.com/i/birdwatch/n/1684227349380825092'  target='_blank'>note url</a>
| While the post might be satire. The man in the pictures is not actually Riff Raff, but Nina Agdal's ex-boyfriend Reid Heidenry    https://www.tmz.com/2015/11/01/heidi-klum-halloween-party-jlo/ | -0.0001 | 0.6| <a href='https://twitter.com/i/birdwatch/n/1692432070834811360'  target='_blank'>note url</a>
| The foreign minister of Germany did not and has no constitutional power to “declare war.” See Arts 65a and 115 of the Basic Law. https://www.gesetze-im-internet.de/englisch_gg/englisch_gg.html#p0305    If a foreign minster’s use of the phrase “at war” was a declaration of war, Russia would have declared war on “the West” a long time ago. https://www.newsweek.com/russia-already-war-us-collective-west-lavrov-1770102?amp=1 | -0.00052 | 0.84| <a href='https://twitter.com/i/birdwatch/n/1618271345120346119'  target='_blank'>note url</a>

#### Helpful + Right

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Emily wasn’t lost. She was abducted by terrorists from Hamas.     https://edition.cnn.com/videos/world/2023/11/25/israel-thomas-hand-daughter-released-ward-nr-vpx.cnn   | 0.65 | 0.75| <a href='https://twitter.com/i/birdwatch/n/1728553688354918804'  target='_blank'>note url</a>
| Emily Hand was kidnapped by Hamas Terrorists on October the 7th. The use of the term lost is inappropriate and fails to highlight that she was released as part of a hostage deal.     https://www.bbc.co.uk/news/world-europe-67533506.amp | 0.65 | 0.74| <a href='https://twitter.com/i/birdwatch/n/1728554736557711815'  target='_blank'>note url</a>
| Claims against UNRWA have been documented for a long time.    The headmaster of an UNRWA school  was a terrorist.  https://www.reuters.com/article/middleeastCrisis/idUSL05686115/    Film by David Bedein in Jenin, UNRWA policies and practices    https://vimeo.com/856467890    UNRWA teachers celebrated Oct 7 massacre  https://unwatch.org/report-u-n-teachers-celebrated-hamas-massacre/    UNRWA teacher holds hostage in attic  https://www.washingtonexaminer.com/policy/defense-national-security/united-nations-gaza-teacher-israeli-hostage-attic | 0.58 | 0.8| <a href='https://twitter.com/i/birdwatch/n/1730887409146175557'  target='_blank'>note url</a>
| &quot;Has died&quot; is misleading and falsely gives the appearance the death was one of natural causes. Vivian Silver was in fact murdered on Oct. 7 by palestinian terrorists, as also confirmed by her son Yonatan Zeigen.      https://www.ctvnews.ca/canada/canadian-peace-activist-vivian-silver-was-murdered-by-hamas-son-says-1.6643670 | 0.64 | 0.73| <a href='https://twitter.com/i/birdwatch/n/1724249231005712599'  target='_blank'>note url</a>
| Harvard President Claudine Gay appeared in a Congressional hearing yesterday.      While she expressed her personal opinion against these calls, she repeatedly refused to say that calling for genocide of Jews is against Harvard's Code of Conduct:    Sources:   https://www.washingtonpost.com/education/2023/12/06/3-elite-college-presidents-answered-questions-antisemitism/    Video Recording: https://www.youtube.com/watch?v=6Bn95MFQNPY | 0.64 | 0.71| <a href='https://twitter.com/i/birdwatch/n/1732476720798658662'  target='_blank'>note url</a>
| Ashdod, Beersheba, Hebron and Jerusalem mentioned in the Bible as part of Israel before the Quran was written.  According to the Supreme Muslim Council's guide for tourists, Temple Mount is linked to Solomon’s Temple (Solomon was a Jewish king).  https://en.wikipedia.org/wiki/Jerusalem  https://en.wikipedia.org/wiki/Solomon  https://en.wikipedia.org/wiki/Temple_Mount#:~:text=Muslim%20interpretations%20of%20the%20Quran,Islam%2C%20that%20was%20later%20destroyed.  https://x.com/visegrad24/status/1730269555115565483?s=20  https://en.wikipedia.org/wiki/Hebron  https://en.wikipedia.org/wiki/Beersheba  https://en.wikipedia.org/wiki/Ashkelon  https://en.wikipedia.org/wiki/Quran | 0.56 | 0.8| <a href='https://twitter.com/i/birdwatch/n/1730687338127188240'  target='_blank'>note url</a>
| The photo was taken in 1933, not 1944 as claimed.    The photo shows the shores of Tel Aviv, a city built by Zionist Jews in 1909 under Ottoman rule, later under the British Mandate, and later under the State of Israel    Sources:  https://www.ariehsharon.org/TelAviv/Introduction/i-MJLJZFH/A  https://www.polin.pl/en/news/2020/01/09/gdynia-tel-aviv-exhibition-architecture-and-politics  https://en.wikipedia.org/wiki/Tel_Aviv#History | 0.53 | 0.82| <a href='https://twitter.com/i/birdwatch/n/1736144012447273315'  target='_blank'>note url</a>

#### Unhelpful + Left

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| There is fix for stupid. | -0.59 | -0.86| <a href='https://twitter.com/i/birdwatch/n/1694894980794372107'  target='_blank'>note url</a>
| The protest was not due to the restaurant being Jewish-owned, it was related to the owner, Michael Solomonov, using the restaurant to fundraise for an organization that provides aid to the Israeli military    https://www.cbsnews.com/philadelphia/news/philadelphia-based-israeli-chef-michael-solomonov-zahav-israel-war/ | -0.62 | -0.76| <a href='https://twitter.com/i/birdwatch/n/1731572646721040430'  target='_blank'>note url</a>
| Elon musk makes this statement, but has been censoring Twitter user @SaeedDicaprio for making tweets in support of Palestine.    https://x.com/saeeddicaprio/status/1737533757052072070?s=46&amp;t=gq1UkkPF91hMLPZfJ2wNbw | -0.68 | -0.67| <a href='https://twitter.com/i/birdwatch/n/1737556625768980979'  target='_blank'>note url</a>
| I like announcements! But it requires context or a thread.  | -0.61 | -0.74| <a href='https://twitter.com/i/birdwatch/n/1679170741177135104'  target='_blank'>note url</a>
| MEMRI is an organisation founded by a former member of the Israeli intelligence service and has been accused of misinterpreting statements to suit a pro-Israel, anti-Arab narrative. https://www.theguardian.com/world/2002/aug/12/worlddispatch.brianwhitaker | -0.57 | -0.77| <a href='https://twitter.com/i/birdwatch/n/1719723544848781512'  target='_blank'>note url</a>

#### Unhelpful + Neutral

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| Linda Yaccarino, is an executive at NBC Universal and holds a position at the World Economic Forum.  Last month, during an interview with Elon, Yaccarino encouraged him the reinstatement of an &quot;influence council&quot; for advertisers to regularly interact with Twitter's leadership.    https://twitter.com/marionawfal/status/1657012965336285184?s=46  | 0.0046 | -0.79| <a href='https://twitter.com/i/birdwatch/n/1657055953345212417'  target='_blank'>note url</a>
| Community Notes agrees with you Elon.  | 0.0056 | -0.7| <a href='https://twitter.com/i/birdwatch/n/1649884571779956736'  target='_blank'>note url</a>
| Then lock her up      YouTube.com | 0.0062 | -0.49| <a href='https://twitter.com/i/birdwatch/n/1715086495059415522'  target='_blank'>note url</a>

#### Unhelpful + Right

| note | polarity | helpfulness | link
| ---- | ---- | ---- | ---- 
| NNN. It is not incorrect to refer to an unaffiliated person as a journalist.  | 0.7 | -0.52| <a href='https://twitter.com/i/birdwatch/n/1733778660245876859'  target='_blank'>note url</a>
| It always has, and it always will be. Even without any humans left on earth there would still be a climate change.  | 0.63 | -0.58| <a href='https://twitter.com/i/birdwatch/n/1694556665071149142'  target='_blank'>note url</a>
| By any measurable objective this is incorrect. Trump rebalanced foreign policy to counter China, achieving energy independence, record gains in employment and real wage growth, stemmed illegal immigration and started a global movement to legalize being LGBTQ, etc.    https://www.presidency.ucsb.edu/documents/fact-sheet-president-donald-j-trump-has-delivered-record-breaking-results-for-the-american | 0.62 | -0.59| <a href='https://twitter.com/i/birdwatch/n/1706875163088671010'  target='_blank'>note url</a>
| Criticism of George Soros is not a &quot;conspiracy theory,&quot; and Musk's opinion that Soros' behavior is bad for civilization is a reasonable disagreement. There are countless other examples of misinformation in this thread; this is one example of many.    https://www.wsj.com/articles/why-i-support-reform-prosecutors-law-enforces-jail-prison-crime-rate-justice-police-funding-11659277441    https://www.city-journal.org/article/george-soross-bad-bet | 0.54 | -0.64| <a href='https://twitter.com/i/birdwatch/n/1709255558614724693'  target='_blank'>note url</a>
| Trump never conspired to steal an election, challenge it yes, but THAT IS Constitutional!  | 0.58 | -0.59| <a href='https://twitter.com/i/birdwatch/n/1712930576578101281'  target='_blank'>note url</a>

