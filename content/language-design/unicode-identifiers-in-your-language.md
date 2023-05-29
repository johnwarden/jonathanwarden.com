---
title: "Unicode Identifiers in Your Language"
slug: unicode-identifiers-in-your-language
image: assets/images/andysinger_scrabbletiles-672x372.png
alias: http://jonathanwarden.com/2016/07/24/unicode-identifiers-in-your-language/
date: "2016-07-24T22:25:00-05:00"
series: ["Unicode Identifiers"]
aliases:
- /2016/07/24/unicode-identifiers-in-your-language/
dratf: true
---

<strong>Abstract</strong>: In this post I summarize the Unicode Consortium's Recommendations for Identifier Syntax and Security (<a href="http://www.unicode.org/reports/tr31">TR31</a> and <a href="http://www.unicode.org/reports/tr39">TR39</a>), and introduce some of the decisions that a language designer needs to make when implementing these.

I also recommend an exact syntax that conforms to the Unicode consortium recommendations for <strong>unicode identifiers</strong> while balancing issues of inclusiveness, confusability, and backwards and forwards compatibility.  In a series of followup posts, I plan to discuss the reasons for these recommendations in detail.
<h2>The Decision</h2>
Many languages are allowing Unicode characters in identifiers:

**Julia**
{{< highlight julia "linenos=false" >}}
∑(x) = sum(x)
{{< / highlight >}}

**Haskell**
{{< highlight haskell "linenos=false" >}}
type ℚ = Ratio ℤ
{{< / highlight >}}

**Scala**
{{< highlight scala "linenos=false" >}}
for (n ← 1 to 10) ...
{{< / highlight >}}


Although these languages all have different specs for what characters can be allowed in identifiers.

There is <a href="[http://programmers.stackexchange.com/questions/16010/is-it-bad-to-use-unicode-characters-in-variable-names]">some debate</a> on whether this is a good idea at all, if only because some coders will have trouble typing non-ASCII characters.

But there is also the issue of confusability, or <em>homograms</em> -- different identifiers that look similar or identical -- which can cause frustration and bugs at best, security concerns at worst.  For example, micro sign `µ` and greek mu `μ`, `scope` in Latin and `ѕсоре` in Cyrillic, and worst of all, invisible control characters.

There is also the question of treatment of superscripts and subscripts, comparison, and backwards compatibility and immutability (so the set of valid identifiers doesn't change as Unicode evolves).

On the other hand, not only is `xᵦ²` just pretty neat, it can be more readable than `x[beta]^2` for some applications.

But most importantly, Unicode identifiers allow people to code <em>in their preferred language</em>.  Certainly there's some benefit to most open-source code being written and documented in English, the lingua-franca of software development, and a coder working in another language will certainly be at a disadvantage, but if it's a question between English and not at all, it may be worth it.

<h2>Unicode's Recommendations for Identifiers</h2>
Fortunately, the Unicode folks have thought a lot about what non-ASCII characters make sense in identifiers, and have issued <a href="http://unicode.org/reports/tr31/">Technical Report #31 - Unicode Identifier and Pattern Syntax</a> (which I'll call the "Identifier Spec") which includes a syntax for identifiers:


	<Identifier> := <Start> <Continue>* (<Medial> <Continue>+)*

The idea is that there are characters that can start an identifier (letters), a larger set that can continue an identifier (numbers, modifiers), and a few medial characters that can only occur in the middle (hyphens and middle dots).

Unicode defines character properties `XID_START` and `XID_CONTINUE` in the Unicode Character Database, which it recommends for use as `&lt;Start&gt;` and `&lt;Continue&gt;`, but the spec lets you define these however you like. For example, '_' (underscore) is not in `XID_START`, but it is suggested you might want to include it in anyway.

<strong>XID_START</strong> is the international equivalent of `/[a-zA-Z]/`, and is made up of:
<ol>
 	<li>Letters (Lt, Lo, Ll, Lu)</li>
 	<li>A dozen exceptions, such as U+212E ( ℮ ) ESTIMATED SYMBOL, 'grandfathered' for backwards compatibility.</li>
</ol>
<strong>XID_CONTINUE</strong> is a superset of XID_START, and adds:
<ol>
 	<li>Marks (Mn, Mc) -- accents, etc. that combine with the characters before them</li>
 	<li>Numbers (Nl, Nd) -- the international equivalent of `/0-9/`</li>
 	<li>Connector Punctuation (Pc) -- the international equivalent of `/_/`</li>
 	<li>A half-dozen miscellaneous characters, such as U+00B7 MIDDLE DOT</li>
</ol>
The identifier spec also suggests a small set of additional <a href="http://unicode.org/reports/tr31/#Table_Optional_Medial">optional punctuation characters to use as &lt;Medial&gt;</a>, including a variety of hyphens, middle-dots,  apostrophes, and format control characters, which it exhorts you to use unless you have a good reason not to (though you probably have a good reason not to use some of them).
<h2>Normalization and Security</h2>
Unicode defines two forms of normalization, NFC or NFKC, which can help address the issue of confusable identifiers (that look the same but aren't).  But normalization intentionally does not eliminate <em>homoglyphs</em> -- two characters with distinct meaning that look the same (for example latin 'o' and cyrillic 'о').

So to further reduce the possibility of identifier confusion, the identifier spec recommends excluding <a href="http://unicode.org/reports/tr31/#Table_Candidate_Characters_for_Exclusion_from_Identifiers">certain obsolete or limited use scripts</a>.

Then there is a completely separate TR, Unicode <a href="http://www.unicode.org/reports/tr39">Technical Report #39, Unicode Security Mechanisms</a> (which I'll call the "Security Spec"), which provides recommendations for further restricting certain characters and mixed-script identifiers.

Implementing these recommendations drastically reduces and sanitizes identifier space, removing most of the weirdness and confusability that comes from invisible characters and homoglyphs.

<h2>Other Considerations</h2>

The identifier also spec includes recommendations and alternatives for addressing:

- stability (backwards compatibility)
- immutability (forward compatibility)
- comparison/equality (case sensitive and insensitive)
- use of format control characters ZWJ and ZWNJ

So this leaves you with a lot of decisions to make. Do you restrict some or all characters recommended in the security spec? Do you allow hyphens, apostrophes, and middle dots in identifiers? Do you allow format control characters in some situations?  Do you NFC or NFKC normalize?  How do you define identifier equality?  Do you make your identifiers "immutable" so that parsers built against different versions of Unicode don't disagree on what an identifier is?

Some of these issues are tricky, and some of the Unicode recommendations are flawed.  I will discuss these issues in detail in followup posts.

But jumping straight to conclusions, here is a specific set of recommendations for a balanced, reasonable definition of a Unicode identifier.
<h2>IPIC9: An Immutable Profile for Identifiers in Code</h2>
<h3>1. Base Syntax</h3>
<ul>
 	<li>&lt;Identifier&gt; := &lt;Start&gt; &lt;Continue&gt;* (&lt;Medial&gt; &lt;Continue&gt;+)*</li>
</ul>
Define &lt;Start&gt; as:
<ul>
 	<li><em>XID_START</em> and <em>U+005F LOW LINE</em></li>
</ul>
Define &lt;Continue&gt; as:
<ul>
 	<li><em>XID_CONTINUE</em>, and U+05F3 HEBREW PUNCTUATION GERESH</li>
 	<li><em>But not</em> U+00B7 MIDDLE DOT or U+0387 GREEK ANO TELEIA (these are Medial instead)</li>
</ul>
Define &lt;Medial&gt; as:
<ul>
 	<li>U+002D, U+2010, U+00B7, U+2027, U+30FB, U+30A0, U+058A, U+05F4, U+0F0B</li>
</ul>
These are: <em>HYPHEN-MINUS, HYPHEN, MIDDLE DOT, HYPHENATION POINT, KATAKANA MIDDLE DOT, KATAKANA DOUBLE HYPHEN, ARMENIAN HYPHEN, HEBREW PUNCTUATION GERSHAYIM</em>, and <em>TIBETAN MARK INTERSYLLABIC TSHEG</em>.
<h3>2. Normalization</h3>
Then <em>Normalize</em> with the following steps:
<ol>
 	<li>NFC normalize</li>
 	<li>Convert U+2010 HYPHEN, U+30A0 KATAKANA DOUBLE HYPHEN, and U+058A ARMENIAN HYPHEN to U+002D HYPHEN-MINUS</li>
 	<li>Convert U+2027 HYPHENATION POINT, U+30FB KATAKANA MIDDLE DOT, and U+0F0B TIBETAN MARK INTERSYLLABIC TSHEG to U+00B7 MIDDLE DOT</li>
 	<li>Convert U+002D HYPHEN-MINUS following ...
<ol>
 	<li>... an Armenian character to U+058A ARMENIAN HYPHEN</li>
 	<li>... a Hiragana or Katakana character to U+30A0 KATAKANA DOUBLE HYPHEN</li>
</ol>
</li>
 	<li>Convert U+00B7 MIDDLE DOT following ...
<ol>
 	<li>... a Tibetan character to U+0F0B TIBETAN MARK INTERSYLLABIC TSHEG</li>
 	<li>... a Hiragana or Katakana character to U+30FB KATAKANA MIDDLE DOT</li>
</ol>
</li>
</ol>
<h3>3. Restrictions</h3>
After normalizing, reject any identifiers that contain characters that:
<ol>
 	<li>are not labeled as "Allowed" in the identifier character <a href="http://www.unicode.org/Public/security/9.0.0/IdentifierStatus.txt">whitelist</a> from the the Security Spec.</li>
 	<li>contain characters from more than one script</li>
</ol>
And don't ever update the whitelist, even as new versions of Unicode are released. This way the definition of an identifier is <strong>immutable</strong>: it will never change even as Unicode evolves.

Don't worry about losing out on important Unicode updates.  First, Unicode guarantees backwards compatibility of identifiers: characters will only be <em>added</em> to this list.  Second, Unicode 9.0.0 is so extremely comprehensive that characters are added to the whitelist very infrequently.  Between Unicode 7.0.0 and 9.0.0, there were are only 14 characters added to Unicode that made it onto this list!

<h2>Sample Code</h2>
The following sample Perl code provides an implementation of this specification:

<h3>Regexes for Matching Identifiers</h3>

{{< highlight perl "linenos=false" >}}
my $START = qr/[\p{XID_START}_]/;
my $MEDIAL = qr/[\x{002d}\x{2010}\x{00b7}\x{2027}\x{30FB}\x{058a}\x{05f4}\x{0f0b}]/;
my $CONTINUE = qr/[\p{XID_CONTINUE}\x{05F3}]/;
my $NONEND = qr/[\x{0387}\x{00B7}]/;
my $WORD_IDENTIFIER_REGEX = qr/$START$CONTINUE*(?:$MEDIAL$CONTINUE+)*(?&lt;!$NONEND)/;
{{< /highlight >}}


<h3>Normalization</h3>
{{< highlight perl "linenos=false" >}}
use Unicode::Normalize 'NFC';

sub normalize_identifier {
	my $string = shift;

	croak "Expected a string argument"
		if not defined $string;

	$string = NFC($string);

	# Hyphen, Armenian Hyphen, and Katakana Double Hyphen to hyphen-minus
	$string =~ s/[\x{2010}\x{058A}\x{30A0}]/\x{002D}/;

	# Hyphenation Point, Katakana middle dot, and Tibetan tsheg to middle dot
	$string =~ s/[\x{2027}\x{30FB}\x{0F0B}]/\x{00B7}/g;

	### Context-specific normalizations
	$string =~ s/[\x{00B7}](?=\p{Tibetan})/\x{0F0B}/g; # middle dot to Tibetan tsheg
	$string =~ s/[\x{00B7}](?=[\p{Katakana}\p{Hiragana}])/\x{30FB}/g; # middle dot to Hiragana/Katakana middle dot
	$string =~ s/[\x{002D}](?=\p{Armenian})/\x{058A}/g; # hyphen to Armenian hyphen
	$string =~ s/[\x{002D}](?=[\p{Katakana}\p{Hiragana}])/\x{30A0}/g; # hyphen to Hiragana/Katakana double hyphen

	return $string;
}

{{< /highlight >}}



<h3>Restriction</h3>

{{< highlight perl "linenos=false" >}}
use Unicode::Normalize 'NFKC';
use Unicode::Security 'mixed_script';

sub is_restricted_identifier {
	my $identifier = shift;

	return 'mixed-script'
		if mixed_script($identifier);

	return 'disallowed'
		if $identifier =~ /\P{InTR39AllowedCharacters}/;

	return 0;
}

sub InTR39AllowedCharacters {
	# List copied from: http://www.unicode.org/Public/security/9.0.0/IdentifierStatus.txt
	return q{0027
	002D 002E
	0030 003A
	...etc...
	};
}
{{< /highlight >}}

<h2>Other Recommendations</h2>
Here are some other things to consider when implementing a Unicode-based language:
<ul>
 	<li>The Unicode identifier spec covers word-like identifiers only.  Create a separate lexical class of math-and-punctuation based identifiers (i.e. operators) comprising sequences of math symbols (Sm) and any ASCII punctuation not reserved for other purposes in your language.</li>
 	<li>This spec disallows non-NKFC characters, such as superscripts and subscripts.  But that means you can't allow superscripts in your source. Just give superscripts and subscripts semantic value.  For example, you can make `xⱼⁿ⁺⁹` syntactic sugar for `exp(nth(x, j), n+9)`.</li>
 	<li>Create a separate class of identifiers for the mathematical alphanumeric symbols such as `𝘽` and non-NFCK font variants of math symbols such as `⅀`. These were designed to be visually non-confusable.</li>
 	<li>Emit syntax errors with specific reasons identifiers are not matched (non-NKFC, mixed script, contains restricted characters per TR39, starts with a Medial character, etc.)</li>
 	<li>If your identifiers are case sensitive, do case-folded comparisons with locale explicitly set to US English or "empty" (root) for consistent behavior.</li>
 	<li>Require space around mathematical minus sign U+2212 to avoid confusion with hyphen.</li>
</ul>
<h2>Summary</h2>
If you implement the above spec, you can't go too far wrong. It conforms to both Unicode's identifier spec (TR31) and security spec (TR39).  It is immutable (both backwards and forwards compatible), and minimizes confusability.  It is highly inclusive, allowing virtually all letters, accents, and medial punctuation in current use in all living languages.  It disallows just a few of Unicode's recommended optional medial punctuation characters that are truly problematic (period, colon, apostrophe, and usually-invisible control characters). It restricts potentially confusable punctuation characters to their appropriate scripts, and reserves non-NFKC variants of characters (superscript, subscript, small, full-width, bold, doublestruck, etc.) for you to assign specific semantic appropriate to your language.

<h2>Followup</h2>
I've written followup posts with my reasoning for these recommendations:
<ul>
 	<li><a href="/punctuation-in-unicode-identifiers">Punctuation in Unicode Identifiers</a></li>
 	<li><a href="immutable-unicode-identifiers/">Immutable Unicode Identifiers</a></li>
</ul>