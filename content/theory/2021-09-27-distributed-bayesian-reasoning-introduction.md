---
layout: single
title:  "Distributed Bayesian Reasoning Introduction"
date:   2021-09-08 00:00:00 +0200
weight: 50
series: ['Distributed Bayesian Reasoning']
# tags: distributed-bayesian-reasoning
toc: true
toc_sticky: true
math: true
summary: "**Distributed Bayesian Reasoning** is a kind of **hypothetical opinion poll**. It tells us not what people actually believe, but what they **would believe** if they knew more."

sidebar:
  - nav: "distributed-bayesian-reasoning"
    title: "In This Series"
  - nav: "distributed-bayesian-reasoning-related"
    title: "Related Articles"
# image: assets/images/distributed-bayesian-reasoning/distributed-brain-without-caption.svg
# header:
#     teaser: /assets/images/distributed-bayesian-reasoning/distributed-brain-without-caption.svg

---


## Brief Introduction

**Distributed Bayesian Reasoning** is a kind of **hypothetical opinion poll**. It tells us not what people actually believe, but what they **would believe** if they knew more.

The purpose is to produce **more intelligent [group judgments](https://personal.lse.ac.uk/list/PDF-files/ReviewPaper.pdf)** that better represent a group's collective knowledge.

The method starts by surveying the **informed opinion** of a group of people who have participated in a discussion about the topic of the poll, and are aware of all the information that was shared during the discussion (see the [Deliberative Poll](/the-deliberative-poll)). But in very large groups, there may be many different discussions and branches of discussion that are impossible for one person to completely follow. This means critical information and arguments shared by some group members may not reach all members of the group. So instead of simply summarizing the informed opinion of the average group member, Distributed Bayesian Reasoning estimates a **hypothetical informed opinion** that group members would have **if they all participated in the full discussion**. 



It works using **[Bayesian inference](https://wiki.santafe.edu/images/2/2e/Bayesian-Reasoning-for-Intelligent-People-DeDeo.pdf)**: a set of mathematically optimal rules that an ideal rational being would theoretically use to revise their beliefs based on new information. These rules are used to estimate the opinion of a hypothetical group-member that 1) **starts with the opinion of the average**, 2) **acquires the knowledge of all**, and 3) is a **perfect Bayesian reasoner**.

How exactly this is works is described in several articles in [this series](#next-in-this-series). In the following introduction, we provide a high-level, non-technical illustration of the basic idea using the sample of a simple jury trial, and briefly discuss the potential of the method. 

## Jury Trial Example

<!--
<span style="image-credit">
Image by <a href="https://pixabay.com/users/artsybeekids-392631/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=5665992">Venita Oberholster</a> from <a href="https://pixabay.com/?utm_source=link-attribution&amp;utm_medium=referral&amp;utm_campaign=image&amp;utm_content=5665992">Pixabay</a>
</span>
-->


<img src="/assets/images/distributed-bayesian-reasoning/jury-trial-cartoon.jpg"
     alt="A Jury Trial"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 300px" />

Suppose a group of 12 jurors is deliberating the verdict in a murder trial, and the verdict is to be determined by a simple majority.

8 of the jurors believe that there was valid DNA evidence, and these 8 vote guilty. The other 4 think the DNA evidence was **not** valid and vote innocent. And let's suppose initially that the trial focused entirely on the DNA evidence: no other arguments supporting guilt or innocence were made during the trial.




Now suppose that a separate sub-jury of 12 different jurors is convened to decide whether or not the DNA evidence is actually valid. This jury is shown evidence that the DNA lab tech was drunk, and as a result votes unanimously that the DNA evidence is *not* valid. 

<img src="/assets/images/distributed-bayesian-reasoning/sample-jury-trial-counts.svg"
     alt="Sample Jury Trial Data"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 300px" />

Should the defendant go to jail? 

No, the defendant should go free. The average juror, had they participated on both trials, *would* have found the DNA evidence invalid, and that juror *should* find the defendant innocent. 



## The Justified Verdict

Now, it could be that some members of the first jury would have voted guilty even if they thought the DNA evidence was invalid. Perhaps the DNA evidence was not the actual **reason** for their vote. Maybe they just found the defendant unsympathetic.

Yet the defendant should be acquitted regardless of how they would have voted. Because if the trial has been fair, then both sides have had their chance to make their case, provide reasons for their beliefs, and subject the beliefs of the other side to critical scrutiny. We can therefore treat the reasons that jurors provide for their decisions as complete: any additional reasons jurors are still withholding after deliberation should not influence the verdict.

So a juror who finds the DNA evidence valid should convict, and a juror who finds the DNA evidence invalid should acquit, **for there is no justification for any other opinion**. 

And since the second trial revealed that a randomly-selected juror *would* find the DNA evidence invalid if they had participated in both trials, **the hypothetical juror who participated in both trials *should* find the defendant innocent**.


## The Bayesian Juror

The reasoning of the hypothetical juror can be seen as a trivial use of **Bayesian inference** to **revise beliefs based on new information**. For a very brief review of the basic concept of Bayesian inference, sufficient to understand the section below, I suggest [A Bayesian Inference Primer](/bayesian-inference-primer).

Imagine a hypothetical juror that is a perfect Bayesian reasoner and that has **prior beliefs that are equal to the average beliefs of the jurors in the first jury**. 

1. a 8/12 probability that the DNA evidence is **valid** AND the defendant is **guilty**
2. a 4/12 probability that the DNA evidence is **invalid** AND the defendant is **innocent**

Now suppose the hypothetical juror also participates in the second jury trial, and concludes that there is a 100% chance that the DNA evidence is **invalid**. 

As a Bayesian reasoner they can no longer rationally can believe that the defendant is guilty. This is because they believed, *a priori*, that there was zero probability of a scenario where both the DNA evidence is **invalid** AND the defendant is **guilty**. Thus if the evidence is invalid, there is zero probability that the defendant is guilty.

<img src="/assets/images/distributed-bayesian-reasoning/bayesian-juror-posterior.svg"
     alt="A Bayesian Juror's Posterior Beliefs"
     style="display: block; margin-left: auto; margin-right: auto; height: 450" /> 

More generally we can use the rules of Bayesian inference to calculate how a hypothetical Bayesian juror would update their belief in any proposition after changing their belief in any other proposition. 

Even if the verdict of the second jury was not unanimous, the rules of Bayesian inference tell us exactly what the hypothetical juror must believe after participating in both juries. For example, if the hypothetical juror concluded, after the second trial, that there was a 50% chance that the DNA evidence was invalid, they must then conclude that there was a 54.2% that the defendant was guilty. The exact math to apply the rules of Bayesian inference in this case is developed out in [the Basic Math](/distributed-bayesian-reasoning-math).

So by converting the average beliefs of the jurors in both juries into the beliefs of a hypothetical perfect Bayesian reasoner, we can simulate what the average juror **would** believe if they were a perfect Bayesian reasoner and participated in both juries -- even if no single juror actually participated in both juries!

## Distributed Reasoning

This means we can take very complex questions, where no individual can come close to being able to grasp all the complex issues involved, and essentially **simulate** a fully-informed individual by breaking the problem down into parts. We can hold a separate trial of ideas for each bit of evidence and each bit of evidence supporting this evidence, and so on for arbitrarily large argument graphs. Verdicts of sub-sub-juries deep in the argument graph are used to revise the verdict of higher-level juries, and the results propagate through the argument graph to determine the main verdict.

<img src="/assets/images/distributed-bayesian-reasoning/distributed-brain.svg"
     alt="A Distributed Brain"
     style="display: block; margin-left: auto; margin-right: auto; width: 400px; max-height: 375px; margin-top: 0px; margin-bottom: 0px" />

This results in a kind of distributed brain, where the approximate reasoning of the average person is applied across the entire argument graph.

This simulated brain would of course would be no more *intelligent* than the average person -- just more informed. It would not be capable of making inferences that the average person couldn't and wouldn't make from the same premises. It would simply conclude what the average person would conclude, based on the probability that they would accept the premises, and the probability that they would accept the conclusion given the premises. 


## Jury Selection

The actual level of intelligence of the distributed brain depends on the people involved. Just as a fair trial requires a reasonable jury and competent lawyers, a fair trial of ideas requires reasonable, informed participants.

Yet different groups of reasonable and informed people will produce very different opinions. The result of this process should be thought of more like the result of a sophisticated opinion poll: it still depends on the group of people polled.

So Distributed Bayesian Reasoning is **not** a method for determining what is **true**, or what people **should believe**, or even what educated or informed people **would believe**. It is a method of estimating what a **specific group of people should believe**, after a thorough and fair deliberation, if they were all informed of all the arguments that were shared.

## Potential

The value of this method lies, first, in the possibility of **better group judgments**. It can be used in decision-support systems to help an organization harness the collective intelligence of its members. It can be used by journalists for collaborative fact-checking, or by blockchains for intelligent governance. And it can be used by social networks to de-amplify potentially viral information that individuals impulsively share but would probably not share if they knew more. 

<img src="/assets/images/distributed-bayesian-reasoning/viral-meme.svg"
     alt="Viral Meme and Critical Comment"
     style="display: block; margin-left: auto; margin-right: auto; max-height: 450px" />


Second, this method provides a mathematical basis for **analyzing disagreement**. It can identify the top reasons people give for their beliefs, and the counter-arguments that are the most convincing. It can identify if there is a "crux" of an argument somewhere in the argument graph that is the ultimate source of disagreement. And it can help people examine their own belief and discover unjustified assumptions and inconsistent reasoning.


<img src="/assets/images/distributed-bayesian-reasoning/crux-of-argument.svg"
     alt="Crux of the Argument"
     style="display: block; margin-left: auto; margin-right: auto; max-height:  550px" />

Finally, the method can be used to **improve the quality of online conversation**. It can be used to create healthier feedback loops that reward and amplify comments that can stand up to the scrutiny of the group, instead of just comments that generate engagement. And it can help identify areas to focus discussion that are most likely to result in consensus.


In future articles we discuss these and other potential benefits of this method. We also discuss various challenges that we foresee in implementing this method in an online system, as well as some assumptions we have glossed over in this introduction.


## Superhuman Collective Intelligence

We have all heard about the "wisdom of crowds" and their famous ability to accurately estimate the weight of an ox or the number of Jellybeans in a jar. If people just as likely to over-estimate as to under-estimate, then even if most people's estimates are way off, the errors can cancel out. 

This type of crowd wisdom evokes the famous [Condorcet Jury Theorem](https://en.wikipedia.org/wiki/Condorcet%27s_jury_theorem), which says that a large enough jury is guaranteed to produce the correct verdict under majority voting, as long as the jurors are independent, and the average juror is **competent**: more than 50% likely to choose the correct verdict.

Unfortunately, in many scenarios, the average person may not be competent, and so the average answer will be totally wrong.

The real potential of collective intelligence is discovering not the opinion of the average person, nor even the opinion of the most competent people, nor that of the most competent people in a group after deliberation has made them more competent...but an opinion that is beyond the abilities of any one person: the opinion that the most competent person in the group would form if they could surpass the limits of time and memory and comprehend all the relevant knowledge shared by all the other members of the group.

This super-human collective intelligence can only be achieved through a process of distributed reasoning.

## This Series

- In [The Meta-Reasoner](/the-meta-reasoner), we precisely define the assumptions that go into our model of the hypothetical fully-informed group-member whose beliefs represent the combined knowledge and intelligence of the group.
- We then derive the [Basic Math](/distributed-bayesian-reasoning-math) for estimating the post-deliberation beliefs of the meta-reasoner for a single argument thread.
- In [Bayesian Averaging](/distributed-bayesian-reasoning-bayesian-averaging), we refine the technique, using Bayesian Inference properly in the case of small samples.
 to deal with the problem of small samples
- The final piece of the puzzle, which is still a work in progress, is dealing with argument graphs involving multiple argument threads.

