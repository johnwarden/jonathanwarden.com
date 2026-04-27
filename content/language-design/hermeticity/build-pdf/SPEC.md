# Hermeticity PDF build — spec & design notes

What this directory does, what every decision was, and why.

## Goal

Produce a print-ready PDF of `index.md` in **ACM SIGPLAN acmart** format from the same markdown source the Hugo site already publishes. Markdown is the source of truth; everything in this directory is regenerable.

## Pipeline

```
index.md  +  abstract.md
   │
   ▼  preprocess.py
processed.md  (Hugo frontmatter stripped; embedded HTML rewritten;
               figures emitted as raw LaTeX; glossary aside as a
               fenced div; <style> blocks and the draft note dropped)
   │
   ▼  pandoc  +  template.tex  +  transform.lua  +  --listings
hermeticity.tex
   │
   ▼  latexmk -lualatex
hermeticity.pdf
```

Run with `devbox run build` (or `./build.sh` from inside `devbox shell` — `.envrc` auto-loads).

## File map

| File              | Role |
|-------------------|------|
| `devbox.json`     | Pinned tools: `pandoc`, `texlive.combined.scheme-full`, `python3`. |
| `.envrc`          | direnv hook so `cd build-pdf/` enters the devbox shell. |
| `build.sh`        | Runs preprocess → pandoc → latexmk. |
| `preprocess.py`   | Markdown rewriter (see *Decisions* below). |
| `transform.lua`   | Pandoc Lua filter: glossary div, inline code, tables, code-block placement. |
| `template.tex`    | Pandoc LaTeX template: acmart preamble, listings setup, restyles. |
| `out/`            | Generated `.tex`, `.pdf`, `.aux`, etc. (gitignored) |
| `../abstract.md`  | YAML front-matter holding the abstract; merged in via `pandoc --metadata-file`. |

## Build-time decisions

### 1. `lualatex`, not `pdflatex`

pdflatex's `listings` reads byte-by-byte and choked on multi-byte UTF-8 (`∘`, `π`, etc.) inside `\lstinline`. lualatex handles UTF-8 natively. acmart fully supports lualatex.

### 2. `acmart [sigplan, review, nonacm]`

- `sigplan` — the two-column conference layout requested.
- `review` — line numbers + double-spacing for circulating drafts.
- `nonacm` — suppresses ACM copyright/conference boilerplate so we don't have to fake DOIs and ISBNs.

### 3. Figures as raw LaTeX, not pandoc image attributes

`![cap](src){width=95%}` becomes `\includegraphics[width=0.95\textwidth]{...}`. In a 2-column acmart document `\textwidth` is the *full page width* (across both columns), not the column width — so the image renders at ~6.5″ inside a ~3.3″ column, with `keepaspectratio` then scaling it down weirdly. Result: small + apparently left-aligned figures.

`preprocess.py` instead emits a raw-LaTeX block (`\begin{figure}\centering\includegraphics[width=\linewidth]…\caption{…}\Description{…}\end{figure}`) for each `<div class="image-with-caption">`. `\linewidth` inside a single-column figure float in 2-col mode equals the column width.

### 4. Strip `**Figure N**.` from caption text

LaTeX's `\caption{}` auto-numbers ("Figure 1."). Leaving the source's manual `**Figure 1**.` prefix in the caption produced "Figure 1. Figure 1. …" duplication.

### 5. `\Description{...}` on every figure

acmart enforces alt-text for accessibility and warns on every figure missing one. `preprocess.py` populates it with the (LaTeX-stripped) caption text. Good enough for a draft; revisit before submission.

### 6. `mdframed` (default framemethod), not `tcolorbox`, for the glossary aside

A `tcolorbox` captures footnotes inside the box as if it were a self-contained page; the `^[observable]` reference inside the glossary appeared at the bottom of the box instead of the page. `mdframed` *can* let footnotes flow to the page bottom — but only with the **default framemethod**. Initially we loaded it with `[framemethod=tikz]` (for rounded corners), which renders frame content inside a captured TikZ node and exhibits the same footnote-trapping bug as `tcolorbox`. Switching to plain `\usepackage{mdframed}` fixes it. The visible cost is that `roundcorner=` no longer applies — the box now has square corners, which reads fine for an academic look.

### 7. Blockquote restyling — faint tint, italic, no rule

Source uses `> …` for key definitions ("A function is hermetic iff …"). On the web these render with a tinted background and italics. The PDF previously rendered them as plain indented prose. We redefine `\quote` to wrap in `mdframed` with a soft gray fill (`black!4`) and italic body — print-friendly muted version of the web look. (User preference: no left vertical rule.)

### 8. `\paragraph{}` left at acmart's default (run-in)

We considered redefining `\paragraph` as a displayed heading (own line, space below) — necessary if `####` were used as labels before figures or code blocks, since the run-in style would otherwise try to inline with a non-prose block. But once we moved those labels to a different mechanism (see §9), the existing `####` headings ("Inert Packages", "Etymology of 'Hermetic'", …) are all followed by body prose, where run-in works fine. Reverted to acmart default.

### 9. Bold-only label paragraphs → `<div class="example-label">…</div>`

Source has six bold-only single-line paragraphs that label the immediately-following block (e.g. `**Example (Go): live package with global singleton**` before a `go` code listing). They aren't body prose, aren't section headings, and aren't real `####` headings either — they're just labels.

We tried converting them to `####` headings, but that ran into two problems: (1) acmart's `\paragraph{}` is run-in, so the heading attached to the code listing's first line oddly; (2) the cleanup made existing real `####` headings (which the user wanted left alone) collateral. We reverted that approach.

The current convention: wrap the label in a `<div class="example-label">…</div>`. Both engines treat raw HTML as native:

- **Hugo** (with `unsafe: true` in goldmark, which the repo already has): the `<div>` passes through. Style with CSS in your theme:
  ```css
  .example-label { font-weight: bold; margin: 1em 0 0.25em; }
  ```
- **Pandoc**: parses to a `Div` AST node with class `example-label`. `transform.lua` handles it by emitting:
  ```latex
  \par\medskip\noindent\textbf{<text>}\par\nopagebreak\smallskip
  ```
  No first-line indent, small space above, bold text, glued to the following block (no page break between label and figure/code).

Decision applies only to these "label for the next block" patterns. Real `####` headings stay as `####`.

### 10. Inline code → `\texttt{}`, not `\lstinline`

`\lstinline!…!` is fragile: multi-byte UTF-8 (`π_S(trace)`) triggered "lstinline ended by EOL" errors, and it doesn't survive inside fragile macro contexts. `transform.lua` rewrites every `Code` element to a `\texttt{…}` with LaTeX special chars escaped. Plain monospace, no syntax highlighting on inline snippets — fine for a paper. `lstlisting` blocks still get full highlighting.

### 11. Custom `\lstdefinelanguage` for typescript and rust

pandoc's `--listings` only emits `language=…` for a built-in whitelist (Go, Scala, Haskell, Python, …). TypeScript and Rust silently dropped the language attribute, so they came through as plain monospace. The Lua filter overrides every `CodeBlock` and emits raw `\begin{lstlisting}[language=<class>]…` regardless of language; the template's preamble defines `typescript`, `rust`, `go`, `scala` and aliases `haskell`/`python` to listings' built-ins.

### 12. `\needspace{<N>\baselineskip}` before each `lstlisting`

Short code blocks were splitting across column/page boundaries. The Lua filter counts lines in each `CodeBlock` and emits `\needspace{N\baselineskip}` so latex starts a new column if the block won't fit. Doesn't prevent breaking inside genuinely long listings — those still split naturally.

The N calculation has to account for a unit mismatch: listings render in `\footnotesize` (~0.85× the body baselineskip), but `\needspace` measures vertical space against the *body* `\baselineskip` at the call site. The first version asked for `lines + 2` body-baselineskips, which over-reserved by 15–20% and caused spurious column breaks. Even after scaling by 0.85 the demand was still too aggressive — for an 8-line listing we'd ask for ~8 baselineskips, but `\needspace` would still trigger if the column had only 6–7 remaining, leaving the column 1/3 empty.

Current formula: `min(ceil(lines × 0.85), 5)`. The cap means only the *first* ~5 baselineskips of a listing are guaranteed contiguous — beyond that, the listing is allowed to split across columns. This matches the original goal (don't strand short listings at column tops/bottoms) without producing whitespace-dump columns when long listings fall near a boundary. The labeled-listing combiner adds 3 extra baselineskips for the label header (so cap is effectively 8 there).

### 13. `tabular` instead of `longtable`

pandoc emits `longtable` for every markdown table. `longtable` doesn't work in 2-column documents (sigplan). `transform.lua` walks `Table` AST nodes and re-emits them as `\begin{table}\centering\begin{tabular}…\end{tabular}\end{table}`. Only safe because the document has one tiny text-only table; revisit if a wide or multi-page table appears.

### 14. `\hyphenation{…}` + `\sloppy` for the abstract

The abstract has long words ("undermining") and em-dash clusters that wouldn't break in the narrow column. Hyphenation hints help long word breaks; `\sloppy` inside `abstract` relaxes line-break penalties. Cosmetic only — the same overfull-by-a-couple-pt warnings still appear in body paragraphs and are normal for two-column LaTeX.

### 15. Unicode chars declared via `\newunicodechar`

Source uses `∘`, `≈`, `→`, `π`, `σ`, `≠`, `⊆`, etc. lualatex handles these natively if the font has glyphs for them. Linux Libertine (acmart body) does. We still declare math fallbacks (`\ensuremath{\circ}`, etc.) in case any cosmetic glyph goes missing.

### 16. Strip the `## Footnotes` heading from the source

`index.md` ends with `## Footnotes` followed by all `[^foo]: …` definitions. On the web, that heading labels the section where Hugo renders the footnote bodies as a list. In LaTeX, pandoc consumes the definitions and emits `\footnote{…}` calls at each reference site (rendered at the bottom of the page they appear on), so the heading would otherwise leave an empty numbered subsection at the end of the PDF. `preprocess.py` removes the heading line; the definitions below it are left intact for pandoc to consume.

### 17. `cd build-pdf/out/` for graphicspath

LaTeX runs from `out/`, but image PNGs live in `..` (the hermeticity content directory). Template sets `\graphicspath{{../../}}` so `\includegraphics{clock-and-wires-3.png}` resolves.

## Author / metadata

Hardcoded in `template.tex`:

- Author: Jonathan R. Warden
- Affiliation: Social Protocols
- Email: john.warden@gmail.com

Title is pulled from `index.md` frontmatter (`title:`). Abstract is pulled from `abstract.md` via `--metadata-file`.

### 18. First-line indent after figures and code blocks left as acmart default

LaTeX indents the first line of every paragraph by `\parindent`, including paragraphs that immediately follow a `figure` float or an `lstlisting`. Visually this can read as redundant — the figure/code already feels like a paragraph break — and a `\noindent` injected after each float would suppress it.

We considered doing that (via the Lua filter, narrowly scoped to figures and lstlistings) but reverted to the acmart default for submission. Reasoning: ACM SIGPLAN essay guidelines say submissions should "conform to the formatting instructions unless there is a reason founded in the nature of the essay to do otherwise." Suppressing the indent is a cosmetic preference, not a content-driven need; not worth justifying in a preface, and not worth even a small risk of a reviewer flagging it as a non-conforming variation. Revisit only for a post-publication / web-tuned build.

### 19. Single `\clearpage` before the `## Appendices` section

The appendices are substantive enough that we want a clear visual boundary between body and appendix material. We initially put a `\clearpage` before *each* `### Appendix X:` heading, but that bloated the page count and produced too much whitespace.

Current rule: `transform.lua`'s `Header` filter matches the parent `## Appendices` heading (and only that) and emits a single `\clearpage` before it. The five appendices then flow continuously after the boundary. `\clearpage` (rather than `\newpage`) flushes pending floats — important in 2-column mode where deferred figures could otherwise land in the wrong section.

ACM SIGPLAN doesn't mandate either flowing or page-broken appendices; this is a navigation choice, not a conformance one.

### 20. Glossary box: kill `\parindent` and tighten `\leftmargini`

Inside `<aside class="glossary">` the first paragraph (the bold title) sat flush left, but the next paragraph ("Terms used so far in this essay:") was indented by acmart's `\parindent`, and the bullets were indented further by `itemize`'s default `\leftmargini` (~2.5em). Three different left edges in a small box read as visual noise.

The Lua glossary handler now opens the `mdframed` and immediately sets `\parindent=0pt` and `\leftmargini=1.2em` so the box's content has a single flush left edge with bullets indented just enough to be obviously list items.

Scope is local to the box (the settings are inside the `mdframed` group), so body-text indentation elsewhere is unaffected.

### 23. `--shift-heading-level-by=-1` so `##` becomes `\section`

The source uses `##` as the highest-level body heading because `#` is reserved for the Hugo page title (set via frontmatter, not a markdown heading). pandoc's default mapping of `##` → `\subsection` left `\section` empty, producing leading-zero numbering ("0.1 Introduction", "0.10.3 Appendix C"). We shift heading levels by -1 in the pandoc invocation so `##` → `\section`, `###` → `\subsection`, `####` → `\subsubsection`.

Side benefit: `####` headings are now `\subsubsection` (displayed by default in acmart) rather than `\paragraph` (run-in). The earlier "displayed-heading-before-table" rule in the Lua filter is no longer needed and was removed; "Summary of Restrictions" now renders as a normal numbered subsubsection.

The `Header` filter's "match by text == 'Appendices'" rule for `\clearpage` is unaffected (text-based).

### 24. `\needspace` before subsections to prevent orphan headings

Without intervention, an `## Appendix C: Glossary of Terms` heading could land on the very last line of a column, with its body starting in the next column. The `Header` filter now prepends `\needspace{4\baselineskip}` for any level-2 (post-shift, i.e. `\subsection`) heading. 4 baselineskips fits the heading itself plus a couple of body lines — enough to ensure the reader sees something under the heading on the same column.

Section-level headings (`\section`) are left to acmart's defaults (which handle orphans well). Subsubsection-level (`\subsubsection`) is also left alone — they're small enough that an orphan is unobtrusive.

### 22. Bind `example-label` Divs to the following code block / figure

The standalone label and the standalone code listing each emitted their own page-break hint:

* the label ended with `\par\nopagebreak\smallskip` (a soft "please don't break here");
* the listing began with `\needspace{N\baselineskip}` (a hard "break now if N lines won't fit").

When a column ran out, `\needspace` won — break inserted *between* label and listing, label stranded on the old column. Same problem for the one figure that has a preceding label (LaTeX's float placement `[ht]` could float the figure to the next page top, leaving the label behind).

`transform.lua` now ships two filter passes:

1. A `Blocks` pass (`combine_label_with_following`) that walks every block list. When it sees a `Div.example-label` immediately followed by a `CodeBlock` (or by a `RawBlock` containing a `figure` environment), it merges them into one raw-LaTeX block:
   * one `\needspace{(label_overhead + listing_lines)\baselineskip}` *before* the label;
   * the bold label;
   * the lstlisting / figure body verbatim.
   For figures, the placement specifier is rewritten to `[H]` (forced "exactly here" — no float) so the figure can't drift to a page top.
2. The original per-element filters (`Div`, `CodeBlock`, `Figure`, `Code`, `Header`, `Table`) run as a second pass for everything the combiner didn't claim.

`\usepackage{float}` is added to `template.tex` to provide `[H]`.

The combiner is the *only* place we compute `\needspace` for a labeled listing now — the per-element `CodeBlock` filter still does its own `\needspace` for unlabelled listings, but a labeled listing is rewritten before that filter ever sees it.

### 21. Generic-label hyperlinks in footnotes → bare URL

Some footnotes end with hyperlinked label words (`[link]`, `[PDF]`, `[Wikipedia]`, `[GitHub]`, `[docs]`) that read fine on the web but carry no information in print — the URL itself needs to be visible. `preprocess.py`'s `expand_link_placeholders` rewrites these patterns to a pandoc autolink (`<url>`), which becomes `\url{...}` in LaTeX (line-breakable monospace URL).

Multi-word descriptive labels (`[Scala 3 Reference: Context Parameters](...)`, `[SES README](...)`) and informative domain-name labels (`[wasi.dev]`, `[docs.rs]`, `[erights.org]`) are left alone — those names *do* carry information.

## What hasn't been addressed

- **TODO: extract URLs from footnotes into a `## Links` section.** Long URLs in 2-column footnotes cause overflow / awkward `\url{}` line-breaks. We considered (and reverted) splitting *citation footnotes* wholesale into a `## References` section — the URL heuristic flagged ~25 of 29 footnotes as citations, but most of those are explanatory clarifications that *happen* to cite a source, and moving them lost the inline reading flow. The narrower fix is "option C" from the design discussion: keep every footnote inline, but for each `[label](url)` inside a footnote body, replace with a numbered marker `[N]` and append a `## Links` section keyed by those numbers. Footnote prose stays where the reader expects it; URLs render at full column width where line-breaks are cheap. Not implemented; revisit if footnote-URL overflow is actually visible in the final PDF.
- **Color figures in print.** The PNGs are color and tuned for web. They render in the PDF unchanged. Color photocopies fine; B&W print may lose contrast in some figures. Worth eyeballing before submission and possibly redrawing the worst offenders.
- **Eight orphan footnote definitions** in `index.md` (`capmyths`, `confinement`, `confused`, `hermeticity`, `isp`, `ocap`, `starlark`, `wuffs`). pandoc warns about each — they're defined but never cited in the body. Either cite or delete.
- **Cosmetic warnings.** `Inconsolata italic undefined → upright fallback` is a known acmart issue and harmless. A handful of 1–10 pt overfull-hbox warnings in body text are normal for narrow 2-column LaTeX and aren't visible to the eye.
- **Print versions of figures** sized differently for the column. Currently every figure renders at full `\linewidth`; if any look cramped, revisit per-figure widths in `preprocess.py`.
- **TOC depth.** `config.yaml` has `markup.tableOfContents.endLevel: 4`. Hugo's default is 3. With the example-label labels now `<div>`-wrapped instead of `####`, the existing `####` headings ("Inert Packages", "Etymology of 'Hermetic'", …) are still in the TOC at endLevel 4. Lower to 3 if you don't want them there.

## Source-side conventions

If you keep editing `index.md`:

- **Single-line "label" paragraphs before a code block or figure** (e.g. `Example (Go): …`) → wrap in `<div class="example-label">…</div>`. CSS styles it on the web; Lua filter renders it as a no-indent bold pseudo-heading in the PDF. Don't use `####` for these — those are reserved for real H4 headings.
- **Definitions and key statements** → keep as `> …` blockquotes. They get the gray-tint italic treatment in the PDF.
- **Inline code** → backticks. Renders as `\texttt{}` in the PDF.
- **Code blocks** → fenced with language tag. typescript, rust, go, scala, haskell, python all work; add new languages to the template's `\lstdefinelanguage` block if needed.
- **Figures** → keep using `<div class="image-with-caption">…<img …/><div>caption</div></div>`. The preprocessor handles them.
- **Asides / callout boxes** → wrap in `<aside class="glossary" markdown="1">…</aside>` or fenced div `:::{.glossary}…:::` — the glossary class is recognized by the Lua filter.
