---
layout: single
title: "Developing the Seven Stems Method for Spanish Verb Conjugation"
slug: developing-the-seven-stems-method
date: 2026-07-21 00:00:00 -0600
toc: true
weight: 1
tags: ['Spanish', 'Software Design']
summary: "How searching for a compact representation of Spanish irregular verbs led me to seven reusable stem types, a conjugation engine, a book, and the Seven Stems website."
---

<style>
.seven-stems-formula {
  display: flex;
  flex-wrap: wrap;
  align-items: baseline;
  gap: 0.42em;
  margin: 1.25em 0;
  line-height: 1.4;
}

.seven-stems-formula--lead {
  justify-content: center;
  padding: 0.8em 0.7em;
  font-size: 1.1em;
}

.seven-stems-formula-term {
  display: inline-flex;
  align-items: baseline;
  gap: 0.42em;
  white-space: nowrap;
}

.seven-stems-spill {
  display: inline-flex;
  align-items: baseline;
  overflow: hidden;
  white-space: nowrap;
}

.seven-stems-formula-stem {
  font-family: Arial, Helvetica, sans-serif;
  font-style: normal;
  font-weight: 700;
}

.seven-stems-formula-ending {
  display: inline-block;
  padding: 0.01em 0.18em 0.05em;
  border-radius: 0.18em;
  color: #222;
  font-family: ui-monospace, "SFMono-Regular", Menlo, monospace;
  font-size: 1em;
  font-weight: 400;
  line-height: 1.18;
}

.seven-stems-formula-plus {
  color: var(--card-text-color-secondary);
  font-weight: 700;
}

.seven-stems-ending-ar { background: #f1ecf5; }
.seven-stems-ending-er { background: #f3eee6; }
.seven-stems-ending-ir { background: #e6f1f2; }
.seven-stems-ending-boot { background: #98d9f3; }
.seven-stems-ending-l { background: #f8bacc; }
.seven-stems-ending-gerund { background: #9fdec2; }
.seven-stems-ending-preterite-i { background: #fabea9; }
.seven-stems-ending-preterite-j { background: #e4beed; }
.seven-stems-ending-future { background: #baccff; }
.seven-stems-ending-imperfect { background: #eec698; }
</style>

<div class="seven-stems-formula seven-stems-formula--lead" role="img" aria-label="Stem formula for caber: cab-er, quep-a, cup-iera, cabr-ía">
  <span class="seven-stems-formula-term"><span class="seven-stems-spill"><span class="seven-stems-formula-stem">cab·</span><span class="seven-stems-formula-ending seven-stems-ending-er">er</span></span></span>
  <span class="seven-stems-formula-term"><span class="seven-stems-formula-plus" aria-hidden="true">+</span><span class="seven-stems-spill"><span class="seven-stems-formula-stem">quep·</span><span class="seven-stems-formula-ending seven-stems-ending-l">a</span></span></span>
  <span class="seven-stems-formula-term"><span class="seven-stems-formula-plus" aria-hidden="true">+</span><span class="seven-stems-spill"><span class="seven-stems-formula-stem">cup·</span><span class="seven-stems-formula-ending seven-stems-ending-preterite-i">iera</span></span></span>
  <span class="seven-stems-formula-term"><span class="seven-stems-formula-plus" aria-hidden="true">+</span><span class="seven-stems-spill"><span class="seven-stems-formula-stem">cabr·</span><span class="seven-stems-formula-ending seven-stems-ending-future">ía</span></span></span>
</div>

I have been living part-time in Madrid for the past three years, and my Spanish is still not as fluent as I want it to be. The thing that trips me up most is irregular verb conjugation.

So I decided to start studying deliberately. My goal was to conjugate every irregular verb confidently in every tense I was likely to use.

At first, the task felt daunting. A Spanish verb table has six pronouns across seven simple-tense columns, which gives you 42 verb forms before you get to imperatives, compound tenses, and less common forms I sometimes hear older people use, like *hubiese*. Most of those forms are regular. Many irregular forms follow common patterns; others follow rarer ones. A few seem like true one-off exceptions.

I wanted to understand these patterns well enough to find "patterns to the patterns" and minimize what I had to memorize. Put another way, I wanted to *compress* each irregular conjugation table into the smallest representation I could.

This is an appealing problem for a software engineer. Much of software design is compression: finding a smaller, cleaner representation of apparent complexity. When it works, you not only save space; you understand the problem better.

I sat down with a notebook, a pencil, and the conjugation tables for a few common verbs that kept tripping me up. Then I marked the irregular stems and spelling changes and began categorizing the patterns.

I started with *regar*. I remembered my stepmother saying “riego las plantas” (“I’m watering the plants”) and wondered why she said *r**ie**go* rather than “*r**e**go*.” Its conjugation table revealed a simple pattern: the stem ***reg-*** changes to ***rieg-*** in present and present-subjunctive forms except *nosotros* and *vosotros*:

![The Seven Stems conjugation table for regar, with Boot-stem forms highlighted in blue](regar-conjugation.svg)


This pattern is extremely common in Spanish. These positions are called the “boot” positions because, when marked on a classroom conjugation table, they form a rough boot shape:

![A classroom present-tense table for regar with the four boot positions outlined in the shape of a boot](boot-stem-pattern.png)

Many other verbs change *e* to *ie* in the boot positions: p**e**nsar → p**ie**nsa, ent**e**nder → ent**ie**nde, and so on. This and a handful of other [**Boot** stem vowel change patterns](https://7stems.net/learn/model-verbs/#boot-stem-vowel-change-models) account for a large portion of the irregularity in Spanish verbs.

## Patterns to the Patterns

Other irregular verbs showed the same basic structure: their stems changed only in specific tenses. For example, *conocer* uses the stem ***conozc-*** for present indicative *yo* and throughout the present subjunctive:

![The Seven Stems conjugation table for conocer, showing the Subjunctive stem conozc-](conocer-conjugation.svg)

This **Subjunctive stem** pattern is extremely common. It appears in forms such as *venga*, *tenga*, *quepa*, and *construya*.

## Key Insight: The Stem Type Determines the Ending

More significantly, **Subjunctive stems** always take the same endings: *-o* for present *yo* and endings beginning with *-a-* in the present subjunctive, regardless of the verb’s regular type.

This means you don't need to conjugate the verb regularly and then swap in the subjunctive stem. Instead, you can **conjugate the stem**: start with *veng-*, *dig-*, or another subjunctive stem and add the appropriate ending. Some research suggests that fluent speakers do exactly this.

The same held for every irregular stem pattern I found: an irregular stem’s conjugation is independent of the regular stem type. In the forms where it applies, each stem behaves like a verb of its own type and is conjugated the same way: **by adding an ending to the stem**.

In this model, what makes a Spanish verb irregular is that **it has more than one stem**. To conjugate it, you need to know:

1. which stem applies to the tense you want to use
2. the stem's type
3. which ending that stem type takes for the desired person and tense

With relatively few exceptions, almost all irregularity in Spanish verb conjugation can be explained by **seven irregular stem types**. Each stem type has a table showing where it applies and which endings it takes. Because each table covers only a subset of forms, I call it a **conjugation card**. Here they are.

![The seven irregular conjugation cards arranged as a compact reference](stem-cards-reference.svg)



<!-- 
## Stem Formulas

To remember a verb’s irregular stems, **you just need to remember one conjugated form for each stem type**. If you remember *venga* for the present subjunctive of *venir*, then it is easy to remember *vengo* in the present indicative, *tú vengas* in the present subjunctive, and so on.

This means we can break down an irregular verb’s conjugation table into a **stem formula** with one form for each stem type, including the regular stem. For example, *valer* has three stems. Its stem formula is:

<div class="seven-stems-formula" role="img" aria-label="Stem formula for valer: val-er, valg-a, valdr-ía">
  <span class="seven-stems-formula-term"><span class="seven-stems-spill"><span class="seven-stems-formula-stem">val·</span><span class="seven-stems-formula-ending seven-stems-ending-er">er</span></span></span>
  <span class="seven-stems-formula-term"><span class="seven-stems-formula-plus" aria-hidden="true">+</span><span class="seven-stems-spill"><span class="seven-stems-formula-stem">valg·</span><span class="seven-stems-formula-ending seven-stems-ending-l">a</span></span></span>
  <span class="seven-stems-formula-term"><span class="seven-stems-formula-plus" aria-hidden="true">+</span><span class="seven-stems-spill"><span class="seven-stems-formula-stem">valdr·</span><span class="seven-stems-formula-ending seven-stems-ending-future">ía</span></span></span>
</div>
 -->
<!-- 
## Spelling Rules Are Regular


Some apparent irregularities are really regular **spelling changes** that apply consistently in certain situations. For example, a silent *u* is added after the *g* in *llegar* whenever the ending begins with *e*. Otherwise, the *g* would be pronounced with a different sound.

But this does not make *llegar* irregular. If the added *u* were an irregularity, you would need to remember which verbs it applied to. Instead, you just need to remember the situation in which the spelling rule applies.

This spelling rule does not change the pronunciation. It prevents the pronunciation from changing, ensuring that the *g* in the stem sounds the same with every ending.

There are also spelling rules that create small pronunciation changes. For example, *leer* has forms such as *leyó*. Spanish spelling does not allow an unstressed *i* to sit between two vowels in that position, so the vowel *i* becomes the consonant *y*: *le**i**ó* becomes *le**y**ó*. The result has a slightly different pronunciation, especially in regions where consonant *y* has a strongly fricated sound. Because of this, the Real Academia Española considers *leer* irregular—but in my opinion, it is a rather regular kind of irregular.
 -->


## Visualizing the Conjugation Tables

Once I understood these patterns, I found myself lying in bed one day—really—imagining conjugation tables as overlapping cards. I pictured each stem type as a different-colored piece of laminated cardstock bearing a partial conjugation table. Beneath them sat the full table for the regular stem type.

I fired up my favorite AI coding agent and built them. They looked great! Here are some examples:

**decir**

![The Seven Stems conjugation table for decir, showing its Boot, Gerund, Subjunctive, Preterite-J, and Future stems and its participle and imperative exceptions](decir-conjugation.svg)

**pedir**

![The Seven Stems conjugation table for pedir, showing its Boot and Gerund vowel changes](pedir-conjugation.svg)

**tener**

![The Seven Stems conjugation table for tener, showing its Boot, Subjunctive, Preterite, and Future stems and its imperative exception](tener-conjugation.svg)

You can find a table for virtually any Spanish verb at [7stems.net](https://7stems.net/).

## The Algorithm

Together, these seven irregular stem types and a handful of spelling rules gave me an elegant model of most Spanish verb irregularities. In software terms, it builds a conjugation table from layered matrices: start with the regular stem card, layer the irregular cards on top in precedence order, apply spelling rules, and finish with any true exceptions.

I implemented the model as [seven-stems-conjugator](https://github.com/johnwarden/seven-stems-conjugator), a Python library that builds a complete conjugation table for any verb.

In keeping with my goal of **compression**, each verb requires only a small data structure containing its irregular stems and exceptions. Here are a few examples:

***Sample Verb Conjugation Data***

```python
'caber': {
    'stems': {
        'Subjunctive': 'quep',
        'PreteriteI': 'cup',
        'Future': 'cabr',
    },
},
'saber': {
    'stems': {
        'Subjunctive': 'sep',
        'PreteriteI': 'sup',
        'Future': 'sabr',
    },
    'exceptions': {(0, PRESENT): 'sé'},
},
'tener': {
    'stems': {
        'BootE': 'tien',
        'Subjunctive': 'teng',
        'PreteriteI': 'tuv',
        'Future': 'tendr',
    },
    'exceptions': {(1, IMPERATIVE): 'ten'},
},
```

## Validating the System

Before publishing the system, I needed to know it was correct. So I wrote an automated test that compares my library’s output against 462,348 forms from an independent conjugation library. When discrepancies revealed numerous errors in that library, I used the Real Academia Española as my source of truth.

## The Book and Website

Eventually, I decided to self-publish the method and the model-verb conjugation tables as a book. [*Seven Stems Spanish Verb Conjugation*](https://7stems.net/book/) is now available in paperback from Amazon and several online retailers. I've also made the book's core content and searchable conjugation tables available for free on the [Seven Stems website](https://7stems.net/).

I never expected to sell a huge number of copies, but it would be gratifying to get the method into the hands of people who would use it. After trying the method, an honest Amazon review would help other learners decide whether it is right for them. I also welcome detailed feedback.
