---
layout: single
title:  "Moderation as Consensus"
date:   2020-11-08 13:40:43 -0700
toc: true
image: moderation-as-consensus.png
canonical_url: https://jonathanwarden.com/moderation-as-consensus

# sidebar:
#   - title: "<h1>Definitions</h1>"
#     text: "
#         <h5>Tragedy of the Commons</h5>
        
#         From [Wikipedia](https://en.wikipedia.org/wiki/Tragedy_of_the_commons): *A situation in a shared-resource system where individual users, acting independently according to their own self-interest, behave contrary to the common good of all users by depleting or spoiling the shared resource through their collective action.*
 
#         <h5>Eternal September</h5>

#          From [Wikipedia](https://en.wikipedia.org/wiki/Eternal_September): *Usenet slang for a period beginning in September 1993, the month that Internet service provider America Online (AOL) began offering Usenet access to its many users, overwhelming the existing culture for online forums.*
   

#         <h5>Keynsian Beauty Contest</h5>

#         From [Wikipedia](https://en.wikipedia.org/wiki/Keynesian_beauty_contest): *A beauty contest where judges are rewarded for selecting the most popular faces among all judges, rather than those they may personally find the most attractive.*

#         <h5>Faustian Bargain</h5>

#         From [dictionary.com](https://www.dictionary.com/browse/faustian-bargain): *To “strike a Faustian bargain” is to be willing to sacrifice anything to satisfy a limitless desire for knowledge or power.*

 
#         "

---

In this article I argue that a decentralized community moderation system can be seen as is a kind of **consensus protocol**, similar to those used to secure blockchains; and that such a protocol can be designed to produce a Nash equilibrium where users reliably enforce a commonly-understood set of community standards of relevance and civility.

## The Fundamental Moderation Problem

Most casual users of social media have no idea of the magnitude of the moderation problem. We take for granted that our Twitter and Facebook feeds are free from videos of live-streamed suicides, violent executions, and child abuse. We don't see the small army of tens of thousands of moderators (35,000 at Facebook [as of Oct 2020](https://www.rev.com/blog/transcripts/tech-ceos-senate-testimony-transcript-october-28)) at work behind our feeds, spending all day viewing the most horrific content in the world so that we don't have to.

Without moderation, online spaces naturally amplify the worst of humanity. It only takes a small number of malicious individuals to overwhelm an online community. They can create and re-create fake accounts, and upload and re-upload the same graphic content countless times.

In a 2019 exposé in *the Verge*, an ex-Facebook moderator (in violation of his NDA) expresses his anguish and anger caused by just one video of an animal being tortured: 

> "'They kept reposting it again and again and again,'" he said, pounding the table as he spoke. 'It made me so angry. I had to listen to its screams all day.'"
>
> [Bodies in Seats](https://www.theverge.com/2019/6/19/18681845/facebook-moderator-interviews-video-trauma-ptsd-cognizant-tampa), The Verge, 2019

This moderator was one of many who were diagnosed with PTSD and shared in a [$52 million settlement from Facebook](https://www.theverge.com/2020/5/12/21255870/facebook-content-moderator-settlement-scola-ptsd-mental-health) to compensate them for mental health issues developed on the job. Victims of the constant, raging, silent war against the trolls.


## The Tragedy of the Conversational Commons

> "The best lack all conviction, while the worst<br/>
>  Are full of passionate intensity."
> 
> -- William Butler Yeats, [*The Second Coming*](https://www.poetryfoundation.org/poems/43290/the-second-coming)


Unmoderated online public spaces suffer from the classic [**tragedy of the commons**](https://en.wikipedia.org/wiki/Tragedy_of_the_commons#:~:text=The%20tragedy%20of%20the%20commons,resource%20through%20their%20collective%20action.). As explained by Clay Shirkey.


> Individual users...have an incentive to maximize expression of their point of view, as well as maximizing the amount of communal attention they receive.
>  
> Clay Shirky, [Group as User](http://art.yale.edu/file_columns/0000/2307/shirky.pdf), 2003


In online communities, conflict and shocking content are often the easiest way to attract attention. But it only takes a small amount of this content to pollute a community so that it is no longer fun and interesting for everyone. Eventually the community is abandoned to the trolls.

This pattern has repeated itself in countless online communities, starting with Usenet during the legendary [Eternal September of 1993](https://en.wikipedia.org/wiki/Eternal_September). Well before the age of widespread social media, [Clay Shirkey](http://art.yale.edu/file_columns/0000/2307/shirky.pdf) and [Eliezer Yudkowsky](https://www.lesswrong.com/posts/tscc3e5eujrsEeFN4/well-kept-gardens-die-by-pacifism) were teaching us why an unmoderated online community simply cannot survive.


There is a lot of discussion today about the toxic environment of social media, and many people see this as negative reflection on contemporary society. But the exaggerated horror of a completely unmoderated online forum makes it clear that this toxic environment is a reflection not of the average member of that community, but of the worst. The problem is a product not of human nature, but of the nature of large online group communication.

Social media is so toxic today not because people are toxic, but because the social networks' gargantuan moderation efforts have been barely enough to keep these communities alive, let alone healthy.

## How Real-World Communities Moderate Themselves

Human society has naturally evolved mechanisms for moderating social behavior. Our instincts for reputation and approval are part of what has made it possible for human beings to actually cooperate and form peaceful and prosperous communities. 


Very few people would pull out their smartphone in a crowded café and show pornographic photos of children, or videos of animals being tortured. In fact in offline public spaces people tend to be on their best behavior, generally even being *polite*. We are strongly influenced by the prospect of even non-verbal expressions of disapproval: a cringe, a scowl, or even an awkward silence. We don't need to pay behavior police. The community moderates itself.

## Community Moderation in Online Spaces

Moderation by paid employees does not fix the underlying causes of the sickness that infects communities when they enter cyberspace: fake and anonymous profiles, the lack of non-verbal social feedback, the difficulty of sustaining back-and-forth conversation.

Community moderation re-introduces these mechanisms. The effect of approval and reputation can be introduced by either requiring verified real-world identifies, or using digital reputation systems such as the "Karma" of Stack Overflow and Hacker News. The powerful non-verbal feedback of scowls and cringes can be approximated using downvotes and flagging.

Slashdot developed the basic approach back in the aughts, as explained by Clay Shirky.

> Rating, karma, meta-moderation -- each of these systems is relatively simple in technological terms. The effect of the whole, though, has been to allow Slashdot to support an enormous user base, while rewarding posters who produce broadly valuable material and quarantining offensive or off-topic posts.
> 
> Clay Shirky, [Group as User](http://art.yale.edu/file_columns/0000/2307/shirky.pdf), 2003



## Filter Bubbles and Echo Chambers

Re-introducing community moderation mechanisms creates new challenges for online communities. In the real world, the dynamics of reputation and approval can often result in cultures of denunciation and oppression, where everyone lives in fear of saying the wrong thing -- about God, or the Emperor, or the Party. Second order dynamics can emerge, where simply failing to scowl or cringe at the right things is dangerous, where we fear being [the first to stop clapping for Stalin](https://www.disappearingman.com/communism/men-wouldnt-stop-clapping/), where we inform and denounce others to show our own righteous zeal.

Likewise, online communities with moderation systems often develop very constrained, idiosyncratic, and sometimes toxic cultures. There are forums where respectful and intelligent conversation is the norm, but others where only the most crude an juvenile posts are successful. There are forums that tolerate only left-wing views or only right-wing views. Many of these forums turn into echo chambers that breed misinformation and extremism.

## Engineering a Culture

But one thing that online spaces have over offline spaces is that they the norms and culture of a community can, in a sense, be engineered. 

Slashdot-like moderation system work similarly to the consensus protocols that power blockchains such as Bitcoin: they work on the principle of the [**Keynsian beauty contest**](https://en.wikipedia.org/wiki/Keynesian_beauty_contest), the name given by economists to situations where everybody does what they expect everyone else to do, because the way to "win" the contest (gaining reputation, crytocurrency, etc) is simply to vote with the majority. 

In situations where the contest is repeated over and over, once the participants develop a common understanding of what it takes to win -- that is, once everybody knows what everybody expects everyone to do -- an equilibrium is established that can be very stable even when there is tremendous motivation to cheat. This is why participants in blockchains can use these protocols to agree on the balance of cryptocurrency wallets, even though they generally don't know or trust each other. As blockchain visionary Vitalik Buterin put it:

> Everyone wants to provide the correct answer because everyone expects that everyone else will provide the correct answer and the protocol encourages everyone to provide what everyone else provides.
> 
> -- Vitalik Buterin, [SchellingCoin](https://blog.ethereum.org/2014/03/28/schellingcoin-a-minimal-trust-universal-data-feed/), 2014

Online forums such as Reddit and Hacker News use digital reputation scores instead of cryptocurrency, but the result is an equilibrium where everyone is trying to submit and upvote the kind of comments that everyone expects everyone else to submit and upvote. In fact a Keynsian beauty contest can be seen almost literally played out in some image boards, where all the photos that are posted conform to that forum's particular standards of "beauty". 

The founders of a new online forum have complete control over what content is  allowed, and thus tremendous power in establishing the culture and norms that develop early on. If the founders later relinquish their censorship powers, the norms and culture that have been established tend to be tend to be maintained.

## Moderating General Social Networks

What would happen if consensus protocol was used to moderate a general-interest social network such as Facebook or Twitter? This seems almost like a contradiction in terms: consensus protocols create equilibria at specific behaviors and norms, which would result in a social network that is no longer general-interest.

But I think it is possible to establish a set of minimal standards that are sufficiently universal: "don't call names", "no videos of actual violence to humans or animals", "no nudity", an so on. Any content that does not violate one of the written rules would be acceptable.

Initially, the rules could be enforced by a core group of trusted moderators. But other users would gain reputation by making moderation decisions consistent with the trusted core. Over time a common-knowledge understanding of how the trusted moderators interpret the rules would emerge and be imitated by other users. At this point, the initial core group would not need to continue making moderation decisions: an equilibrium will have been established among a larger group of trusted users. In cases where there is a clear common understand among these users of what the "correct" answer is, the consensus algorithm will be as stable as a blockchain consensus algorithm: everyone in the trusted group will want to provide the correct answer because they expect everyone else to do the same. A simple transparent process such as a [**Deliberative Poll**](/the-deliberative-poll) can be used to resolve some cases where there is disagreement about the correct decision. A trusted core can act as a kind of supreme court in cases where this process does not produce a unanimous result.

The idea is that such a system avoid the problems of echo-chambers. Users don't just down-vote content they don't like, they flag content that violates specific rules, and their downvotes only have weight if they have a reputation for interpreting the rules consistently with other trusted users.


## Moderating Social Networks at Scale

All online communities require moderation to avoid being overrun by toxic content. Moderation can be either centralized or decentralized. 

The social networks that have used centralized moderation to reach very large scale have made the Faustian bargain, achieving success with a product they [know to be bad for the world](https://www.nytimes.com/2020/11/24/technology/facebook-election-misinformation.html).

Centralized moderation not only fails to deal with any but the most toxic content, it is inevitably biased and open to abuse. It places the power of censorship into the hands of armies of young people with little training. They make mistakes. They have their biases. There is no transparency: they ultimately answer only to their employer.

A healthy online community must place moderation responsibility in the hands of the community, using a consensus protocol engineered to produce reliable judgments on whether or not a post violates community standards.

