---
title: "Immutable Unicode Identifiers"
slug: "immutable-unicode-identifiers"
image: assets/images/immutability_ancient_aliens.jpg
alias: http://jonathanwarden.com/2016/08/18/immutable-unicode-identifiers/
date: "2016-08-18T12:05:00-05:00"
series: ["Unicode Identifiers"]
aliases:
- /2016/08/18/immutable-unicode-identifiers/
draft: true
---

In a previous post I introduced the Unicode Consortium's Identifier Spec (<a href="http://unicode.org/reports/tr31">TR 31</a>) and Security Spec (<a href="http://unicode.org/reports/tr39">TR 39</a>), and explain my own recommendations for implementing <strong>Unicode Identifiers</strong> in conformance with the Unicode recommendations, which is to use a fixed whitelist of allowed identifier characters.
<h2>The Problem</h2>
As new versions of Unicode are released, what qualifies as an identifier could change.

For this reason, TR31 includes recommendations for <a href="http://unicode.org/reports/tr31/#Immutable_Identifier_Syntax">immutable identifiers</a>.  The idea behind these is both backwards and <strong>forwards compatibility</strong> (or <strong>upward compatibility</strong>): make it so parsers/specs written against one version of Unicode recognize the same identifiers as parsers/specs written against future versions of Unicode. This approach is used in the XML 1.1 Recommendation.
<h2>The Unicode Consortium's Recommendation</h2>
The Unicode recommended approach (UAX31-R2) allows all characters in identifiers (whether or not they are assigned in any specific version of Unicode), except known punctuation/control/whitespace/etc.  Because this approach is so inclusive of potentially inappropriate characters (emojis, ideograms, etc), it suggests that alternatively you can start with valid identifier characters (from XID_START and XID_CONTINUE) from a specific version of Unicode, and then allow any character that aren't assigned in that version.

This approach essentially decouples your spec/parser from the Unicode Character Database. But the result is that you lack information about the properties of parsed identifiers, such as what script they belong to, and their NFC/NFKC/case-folded forms.

This makes it impossible to implement the recommended restrictions from the Security Spec (restricted scripts, non-NFKC, etc.). And most importantly, it makes it impossible to compare identifiers for equality, which requires NFC normalization at the very least. Indeed UAX31-R2 ends with the recommendation that identifiers not use any unassigned characters.

I emailed Mark Davis of the Unicode Consortium with an earlier version of this post, and he explained:
<blockquote>
  The immutable identifiers are designed for those cases (like XML) that can't update across versions of Unicode. They are not particularly recommended. Any system that *can* update across versions of Unicode (as your proposal requires) are instead recommended to use the default identifiers.</blockquote>
So in other words, decoupling your identifier spec from the UCD like this makes sense for XML because it doesn't concern itself with what you do with the tags/attributes in the parsed document, it just needs to decide "identifier/not-identifier". But for a programming language, you can and probably should disallow unassigned characters so you can use the UCD to understand the properties of identifiers, most importantly so you can NFC/NFKC normalize identifiers and compare them for equality.
<h2>My Recommendation: Fixed Whitelist of Allowed Characters</h2>
Now, immutability is still a neat idea for a programming language. As a language designer you will be swimming in complexity and change: it would be nice if there was one aspect that was set in stone once and for all. The definition of an identifier could be one of those things.

Another way to achieve immutability is to simply use a fixed white list of allowed Unicode characters.  I recommend using the latest list of allowed characters from the Security Spec as your <a href="http://www.unicode.org/Public/security/9.0.0/IdentifierStatus.txt">identifier character whitelist</a>, and then <em>never upgrading this list</em>.

Your gut reaction to forever pegging your language spec to a specific version of Unicode is probably "<em>no</em>"! Software developers are used to upgrading to the latest versions of everything.

But Unicode is a standard, not code, and as of Unicode 9.0, the Unicode character set is <em>incredibly</em> complete.  We've gone beyond encoding all the world's currently used languages to encoding obscure academic scripts and emoji.  If there's a potentially legal identifier character in some language that somebody might actually program in, it's almost certainly already in Unicode.

The suggested <a href="http://www.unicode.org/Public/security/9.0.0/IdentifierStatus.txt">identifier character whitelist</a> data file  contains the version of Unicode in which each character was added.  A quick search shows that there are only 11 characters characters added to Unicode between 8.0 and 9.0 that made it onto the whitelist, and another 9 previously existing characters were added to the whitelist (from the tamil script).  In other words, evolution of this list has ground to a near halt.
<h2>Summary</h2>
You will still probably want to use the <a href="http://unicode.org/reports/tr31/#R1">default syntax for identifiers</a> to make sure your identifier doesn't, say, start with a number, and to allow optional medial punctuation (e.g. hyphens). This syntax relies on the XID_START and XID_CONTINUE character properties from the Unicode Character Database (plus any optional characters you choose to add).  You are probably using the version of the Unicode Character Database that is bundled with whatever language you are using to build your parser, which is probably less than the latest version (9.0 at the time of this writing).  This means that the set of allowed identifier characters will be a subset of the characters in the identifier character whitelist, which is based on Unicode 9.  So, as your UCD is upgraded, you will "grow in" to the full list of allowed characters in the whitelist.

If you follow this recommendation, you still have some choices to make about optional medial characters, restricting mixed-script identifiers, and NFC/NFKC normalization.  I have defined an identifier profile called <a href="http://jonathanwarden.com/2016/07/24/unicode-identifiers-in-your-language/#IPIC9_An_Immutable_Profile_for_Identifiers_in_Code">IPIC9</a> (Immutable Profile for Identifiers in Code), which uses the recommendation above and also specifies exactly which optional characters to allow, with specific rules for normalizing and restricting identifiers.

Using this spec will give you an extremely exhaustive, very clean set of legal identifier characters that will grow to a certain point and then never change.