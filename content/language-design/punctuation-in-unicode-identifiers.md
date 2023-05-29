---
title: 'Punctuation in Unicode Identifiers'
slug: "punctuation-in-unicode-identifiers"
image: /assets/images/preview.png
alias: http://jonathanwarden.com/2016/08/12/802/
date: "2016-08-12T13:25:00-05:00"
series: ["Unicode Identifiers"]

---

In a <a href="http://jonathanwarden.com/2016/07/24/unicode-identifiers-in-your-language">previous post</a> I introduced the Unicode Consortium's specs for <strong>Unicode Identifier</strong> Syntax and Security (TRs <a href="http://www.unicode.org/reports/tr31/">31</a> and <a href="http://www.unicode.org/reports/tr39/">39</a>), and summarized my own recommendations, in cases where the TRs leave you with options.

The Unicode Identifier Spec recommends a handful of <a href="http://www.unicode.org/reports/tr31/#Table_Optional_Start">optional punctuation characters</a> to allow in unicode identifiers.  In this post I make specific recommendations for which of these optional characters you really should allow.
<h1>Hyphenation</h1>
The identifier spec recommends allowing both the hyphen (U+2010) and the ASCII hyphen-minus (U+002D) in the middle of identifiers (<code>&lt;Medial&gt;</code> characters).

They are also included in the security spec's <a href="http://www.unicode.org/Public/security/9.0.0/IdentifierStatus.txt">identifier character whitelist</a>.  However, although it is not clear from the spec, these characters should only be allowed as <code>&lt;Medial&gt;</code> characters.  An identifier should not start or end with a hyphen.

Unfortunately, allowing both hyphen and hyphen-minus can create confusion, since these are not the same under either NFC or NFKC normalization.  For example, the following would be valid, but distinct, identifiers.
<pre><code>first-rate
first-rate
</code></pre>
This possibility is mentioned in <a href="http://unicode.org/reports/tr36/#TableSingleScriptSpoofing">TR36 Single Script Spoofing</a>. However as of Unicode 9.0 the security spec doesn't make recommendations for dealing with this kind of single-script spoofing.
<h2>Recommendation: Allow Hyphen-Minus in Identifiers, Make Hyphen a Non-Canonical Alternative</h2>
If you follow the recommendation in the identifier spec to allow hyphen (U+2010) in medial position, then make the ASCII hyphen-minus the canonical form.

Even though the hyphen was added to Unicode as the 'unambiguous' hyphen, a hyphen-minus sandwiched between two words in an identifier such as <code>pretty-print</code> has unambiguous hyphen semantics, whereas as "pretty - print" obviously means "pretty minus print" (or a formatting mistake)

Plus the ASCII hyphen-minus has a tradition of use inside identifiers in many languages, and it's probably best to make sure the set of valid Unicode identifiers is a superset of valid ASCII identifiers.

Canonicalizing the proper hyphen to ASCII hyphen-minus will make life easier for coders in some cases, such as when documentation formatters convert a hyphen-minus in identifiers to a hyphen for display. A coder who unwittingly copies-and-pastes non-ASCII hyphens into their code should be none the wiser.
<h2>Recommendation: Require Proper Spacing around Mathematical Minus and Hyphen</h2>
Furthermore, I recommend that the mathematical minus sign − (U+2212) be disallowed in places where it looks like a hyphen:
<pre><code>&gt; first−rate
</code></pre>
<strong>ERROR</strong>: <em>Mathematical minus sign (U+2212) used as hyphen. Please use the '-' character (U+002D hyphen-minus) instead, or place space around the minus sign.</em>

And the hyphen should (U+2010) be disallowed in places where it looks like a minus:
<pre><code>&gt; first ‐ rate
</code></pre>
<strong>ERROR</strong>: <em>Hyphen (U+2212) cannot be used by itself as symbol. If you meant minus, use '-' (U+002D hyphen-minus) instead.</em>

Don't parse 'first-rate' differently depending on whether a mathematical minus or a hyphen is used.  Code that looks the same should work the same.  The proper hyphen and mathematical minus were introduced to Unicode to allow clear semantic distinctions between hyphen and minus, but a hidden semantic distinction doesn't justify visual confusion.  Your code should be flexible enough to distinguish between hyphen and minus based on context, but strict enough to reject semantically inappropriate use of either of them.

Furthermore, I recommend considering mathematical minus and hyphen-minus to be canonical equivalent <em>when used as part of non-word identifiers</em>.  For example this is clearly a minus:
<pre><code>a - b
</code></pre>
<h1>Apostrophes</h1>
The identifier spec also recommends apostrophe in <a href="http://unicode.org/reports/tr31/#Table_Optional_Medial">optional medial characters</a>.
<h2>Recommendation: Disallow Apostrophe</h2>
Unlike the hyphen, the apostrophe does not have a tradition of inclusion in identifiers in many computer languages. Plus, the syntax around the apostrophe doubling as a single quote character would make both visual and actual parsing quite  difficult. Programmers who speak languages that have apostrophes in words have been dealing with omitting them from identifiers for a long time, and I don't sense the demand for apostrophes in languages as there is for hyphens.  Solving this problem is probably not worth the problems it can create.

If you follow this recommendation, do not allow the right single quotation mark either.

Disallow:
<pre><code>U+0027 ' APOSTROPHE
U+2019 ’ RIGHT SINGLE QUOTATION MARK
</code></pre>
<h1>Middle Dots</h1>
<h2>Recommendation: Allow Middle Dot</h2>
The Catalans are a passionate people fiercely proud of their language, many of whom will be happy to use Catalan words in their code, which can have middle dots. Variants of the middle dot are also used in other languages including Katakana and Chinese, so it's best to allow them.
<pre><code>00B7  · MIDDLE DOT
0387  · GREEK ANO TELEIA
</code></pre>
U+0387 GREEK ANO TELEIA is a non-NFC equivalent to middle dot, so should be treated the same as the middle dot.
<h2>Recommendation: Normalize Hyphenation Point</h2>
The Hyphenation Point is confusable with U+00B7 middle dot.  So allow it, but normalize to middle dot.
<pre><code>2027  ‧ HYPHENATION POINT
</code></pre>
<h2>Recommendation: Disallow Middle Dots at End of an Identifier</h2>
The middle dot is a recommended medial character, but is also in XID_CONTINUE, which allows it to appear at the end of an identifier too.  I recommend you disallow middle dot  specifically at the end of identifiers.
<h1>Non-ASCII Punctuation</h1>
The identifier spec recommends several other <a href="http://unicode.org/reports/tr31/#Table_Optional_Medial">optional characters</a> from non-latin scripts. Some of these are confusable with the hyphen and middle dot.

For example, U+30FB KATAKANA MIDDLE DOT looks a lot like a regular middle dot, and U+058A ARMENIAN HYPHEN looks like a hyphen-minus if you don't look at it closely.

The confusability issue is partly solved if your language disallows mixed-script identifiers.  It would prevent for example an Armenian hyphen being used anywhere but between Armenian characters.  But because hyphen-minus is part of the Common script, mixed-script detection does not prevent a regular hyphen-minus from being placed between Armenian characters!
<h2>Recommendation: Normalize Non-Ascii Hyphens and Middle Dots Based on Context</h2>
The following three medial characters belong to specific scripts, and should be used in place of hyphens and middle dots in those scripts.
<ol>
 	<li><strong>Hiragana and Katakana</strong>: 30A0 ゠ KATAKANA-HIRAGANA DOUBLE HYPHEN</li>
 	<li><strong>Armenian</strong>: 058A ֊ ARMENIAN HYPHEN</li>
</ol>
Middle Dots:
<ol>
 	<li><strong>Hiragana and Katakana</strong>: 30FB ・ KATAKANA MIDDLE DOT</li>
 	<li><strong>Tibetan</strong>: 0F0B ་ TIBETAN MARK INTERSYLLABIC TSHEG</li>
</ol>
To normalize these characters, use the following rules:
<ol>
 	<li>If any above characters follow a character from a different script, they should be normalized to hyphen minus or latin middle dot, respectively.</li>
 	<li>If any a hyphen or middle dot follows a character from any of these scripts, they should be converted to the appropriate character from that script.</li>
</ol>
Examples:
<ol>
 	<li><code>first֊rate</code> (with Armenian hyphen) =&gt; <code>first-rate</code></li>
 	<li>il་lusio (with Tibetan tsheg) =&gt; il·lusio</li>
 	<li>first・rate (with Hiragana/Katakana middle dot) =&gt; first·rate</li>
 	<li>ウォルドルフ·アストリア (with Latin middle dot) =&gt; ウォルドルフ・アストリア (with Hiragana/Katakana middle dot)</li>
 	<li>ウォルドルフ֊アストリア (with Armenian hyphen) =&gt; ウォルドルフ゠アストリア (with Hiragana/Katakana double hyphen)</li>
 	<li>ཙ·ཚ (with latin middle dot) =&gt; ཙ་ཚ (with Tibetan tsheg)</li>
 	<li>հայերեն֊հայերեն (with hyphen-minus) =&gt; հայերեն֊հայերեն (with Armenian hyphen)</li>
 	<li>il・lusio (with Hiragana/Katakana middle dot) =&gt; il·lusio (with Latin middle dot)</li>
</ol>
<h2>Recommendation: Allow Recommended Hebrew Punctuation Characters</h2>
If your language dis-allows mixed-script identifiers as recommended in my last post (and in the Unicode the security spec), the following characters can only be used after Hebrew characters.  Furthermore, although they could be confused with apostrophes and double-quotes, these characters are not allowed in identifiers.

Medial:
<pre><code>05F4  ״ HEBREW PUNCTUATION GERSHAYIM
</code></pre>
Continue:
<pre><code>05F3    ׳   HEBREW PUNCTUATION GERESH
</code></pre>
<h1>Miscellaneous ASCII Punctuation</h1>
The identifier spec recommends also allowing the following characters unless you have a compelling reason not to.

Start:
<pre><code>0024    $   DOLLAR SIGN
005F    _   LOW LINE
</code></pre>
Medial:
<pre><code>002E    .   FULL STOP
003A    :   COLON
</code></pre>
<h2>Recommendation: Allow _ but not $</h2>
A lot of mainstream languages allow the underscore (or low line) anywhere in identifiers, so there is no compelling reason to disallow it.

The same is not true of the dollar sign.  The dollar sign, like other miscellaneous ASCII characters such as @, %, ~, is often allowed as part of <em>non-word</em> identifiers such as &gt;=, ||, -&gt;, ++, and &lt;$&gt;.  So I recommend allowing these in a separate class of non-word identifiers.  Scala is an example of a language that takes this approach, having word identifiers, and 'operator identifiers' comprising misc. ASCII characters and Unicode math symbols.
<h2>Recommendation: Disallow . and :</h2>
. and :, like /, are often used in languages for separating the parts of 'paths' or 'fully qualified' names.

But, in many languages, it is best to think of these as just syntax for expressing a composite identifier with multiple parts, and not part of the content of the identifier itself.  So I recommend disallowing these characters 'identifiers', and only allowing them in 'paths' or 'namespaces' or 'fully qualified names'.
<h1>Format Control Characters</h1>
The ZWJ (U+200D ZERO WIDTH JOINER) and ZWNJ (U+200C ZERO WIDTH NON-JOINER) characters are invisible, except when they affect the appearance of certain pairs of characters when placed between them. Although initially intended solely for formatting, ZWJ and ZWNJ now can actually change the meaning of some words.

The identifier spec provides a regex for <a href="http://www.unicode.org/reports/tr31/#Layout_and_Format_Control_Characters">identifying places in text</a> when ZWJ/ZWNJ <em>might</em> cause an actual visual distinction. However, there are still many places this doesn't catch, and many terminals and text editors don't know how to render these correctly anyway.
<h2>Recommendation: Elide ZWJ/ZWNJ in Case-Insensitive Identifiers</h2>
<pre><code>200C    ZERO WIDTH NON-JOINER*
200D    ZERO WIDTH JOINER*
</code></pre>
Now, case-folding elides ZWJ/ZWNJ, so if your identifiers are case-insensitive (meaning you are case folding them before comparing them), allowing them will not create confusability issues, since two otherwise equal identifiers will still be equal if one has a ZWJ/ZWNJ character and the other doesn't.  So for purposes of improved readability, I recommend allowing but eliding ZWJ/ZWNJ characters for case-insensitive identifiers.

International domain names (IDN) also allow but elide ZWJ/ZWNJ characters based on the same logic.
<h2>Recommendation: Disallow ZWJ/ZWNJ in Case-Sensitive identifiers</h2>
If identifiers in your language are case-sensitive, then I recommend that you simply disallow these characters for now.

None of the Unicode normalization forms knows how to handle ZWJ/ZWNJ correctly, by removing them when they are invisible or adding them when it's more correct.  I think it might be possible to create a normalization algorithm in the future that can do this. But if in the meantime these characters were allowed, you couldn't incorporate a proper ZWJ/ZWNJ normalization in the future without breaking backwards compatibility.

So disallowing ZWJ/ZWNJ will mean certain words can't be used as identifiers in their proper spelling, but programmers have been dealing with this forever (e.g. I can't use <code>can't</code> as an identifier in most languages but it's ok).  And it leaves open the possibility of a <em>proper</em> implementation of ZWNJ identifiers in the future.
<h1>Summary</h1>
I have summarized all my recommendations for identifiers in a spec I call <a href="http://jonathanwarden.com/2016/07/24/unicode-identifiers-in-your-language/#IPIC9_An_Immutable_Profile_for_Identifiers_in_Code">IPIC9</a> (immutable profile for identifiers in code).