#!/usr/bin/env python3
"""Preprocess hermeticity index.md into pandoc-friendly markdown.

Strips Hugo frontmatter and embedded HTML that pandoc can't handle cleanly,
and rewrites figure constructs into native markdown so pandoc emits real
LaTeX figure environments.
"""

import re
import sys


def strip_frontmatter(text: str) -> str:
    """Replace Hugo frontmatter with a minimal pandoc YAML block (title only)."""
    m = re.match(r"^---\s*\n(.*?)\n---\s*\n", text, flags=re.DOTALL)
    if not m:
        return text
    fm = m.group(1)
    title_m = re.search(r'^title:\s*"?(.*?)"?\s*$', fm, flags=re.MULTILINE)
    head = ""
    if title_m:
        # Escape any embedded double-quotes; wrap in double-quotes so that
        # colons, commas, etc. don't confuse the YAML parser.
        title = title_m.group(1).replace('"', '\\"')
        head = f'---\ntitle: "{title}"\n---\n\n'
    return head + text[m.end():]


def strip_styles(text: str) -> str:
    return re.sub(r"<style[^>]*>.*?</style>", "", text, flags=re.DOTALL)


def strip_html_comments(text: str) -> str:
    return re.sub(r"<!--.*?-->", "", text, flags=re.DOTALL)


def strip_footnotes_heading(text: str) -> str:
    """Drop the ``## Footnotes`` heading. The web build uses the heading to
    label the section where footnote *definitions* render; in LaTeX, pandoc
    consumes the definitions and emits ``\\footnote{...}`` calls at each
    reference, so the heading would otherwise leave an empty section in the
    PDF."""
    return re.sub(r"^##\s+Footnotes\s*$\n?", "", text, flags=re.MULTILINE)


def strip_note_aside(text: str) -> str:
    return re.sub(r'<aside class="note">.*?</aside>', "", text, flags=re.DOTALL)


def expand_link_placeholders(text: str) -> str:
    """Replace ``[link](url)`` and ``[PDF](url)`` with the bare URL.

    On the web these labels are clickable, so the short word reads fine. In
    the PDF (especially in print) the word "link" or "PDF" carries no
    information — the URL itself needs to be visible. We rewrite to a
    pandoc autolink (``<url>``) which becomes ``\\url{...}`` in LaTeX.
    """
    return re.sub(
        r"\[(?:link|PDF)\]\((https?://[^)]+)\)",
        r"<\1>",
        text,
        flags=re.IGNORECASE,
    )


def convert_glossary_aside(text: str) -> str:
    return re.sub(
        r'<aside class="glossary"[^>]*>(.*?)</aside>',
        r"\n::: glossary\n\1\n:::\n",
        text,
        flags=re.DOTALL,
    )


_FIGURE_BLOCK = re.compile(
    r'<div class="image-with-caption">(.*?</div>)\s*</div>',
    re.DOTALL,
)


def _latex_escape(s: str) -> str:
    """Minimal LaTeX escaping for caption text emitted into raw LaTeX."""
    return (
        s.replace("\\", "\\textbackslash{}")
         .replace("&", "\\&")
         .replace("%", "\\%")
         .replace("$", "\\$")
         .replace("#", "\\#")
         .replace("_", "\\_")
         .replace("{", "\\{")
         .replace("}", "\\}")
         .replace("~", "\\textasciitilde{}")
         .replace("^", "\\textasciicircum{}")
    )


def convert_image_with_caption(text: str) -> str:
    """Replace each <div class="image-with-caption"> with a raw-LaTeX figure
    block. We bypass pandoc's image rendering entirely because pandoc's
    width=N% emits 0.NN\\textwidth, but in 2-column acmart \\textwidth is the
    full page width, not the column width. Going straight to LaTeX with
    \\linewidth makes figures fill the column.
    """

    def replace(m: re.Match) -> str:
        block = m.group(1)
        img = re.search(r"<img\s+([^>]*?)/?>", block, re.DOTALL)
        if not img:
            return ""
        attrs = img.group(1)
        src_m = re.search(r'src="([^"]+)"', attrs)
        if not src_m:
            return ""
        src = src_m.group(1)
        id_m = re.search(r'id="([^"]+)"', attrs)
        fig_id = id_m.group(1) if id_m else ""

        cap_m = re.search(r"<div>(.*?)</div>", block, re.DOTALL)
        if cap_m:
            caption_html = cap_m.group(1).strip()
            # Drop the leading "<strong>Figure N</strong>." — LaTeX's \caption
            # auto-numbers, so the manual prefix would render as "Figure 1. Figure 1."
            caption_html = re.sub(
                r"^\s*<strong>\s*Figure\s+\d+\s*</strong>\s*\.\s*",
                "",
                caption_html,
                flags=re.IGNORECASE | re.DOTALL,
            )
            caption_html = re.sub(r"<strong>(.*?)</strong>", r"\\textbf{\1}", caption_html, flags=re.DOTALL)
            caption_html = re.sub(r"<em>(.*?)</em>", r"\\emph{\1}", caption_html, flags=re.DOTALL)
            caption_html = re.sub(r"<code>(.*?)</code>", r"\\texttt{\1}", caption_html, flags=re.DOTALL)
            # Markdown backticks for inline code inside the caption HTML.
            caption_html = re.sub(r"`([^`]+)`", r"\\texttt{\1}", caption_html)
            caption_html = re.sub(r"\s+", " ", caption_html).strip()
            caption = caption_html
        else:
            caption = ""

        # Description for acmart accessibility — strip any leftover LaTeX commands.
        plain_desc = re.sub(r"\\[a-zA-Z]+\{([^{}]*)\}", r"\1", caption)
        plain_desc = _latex_escape(plain_desc)

        label_line = f"\\label{{{fig_id}}}\n" if fig_id else ""

        latex = (
            "\n```{=latex}\n"
            "\\begin{figure}[ht]\n"
            "\\centering\n"
            f"\\includegraphics[width=\\linewidth]{{{src}}}\n"
            f"\\caption{{{caption}}}\n"
            f"{label_line}"
            f"\\Description{{{plain_desc}}}\n"
            "\\end{figure}\n"
            "```\n"
        )
        return latex

    return _FIGURE_BLOCK.sub(replace, text)


def main() -> None:
    src = sys.stdin.read() if len(sys.argv) < 2 else open(sys.argv[1]).read()
    text = strip_frontmatter(src)
    text = strip_styles(text)
    text = strip_html_comments(text)
    text = strip_note_aside(text)
    text = strip_footnotes_heading(text)
    text = expand_link_placeholders(text)
    text = convert_image_with_caption(text)
    text = convert_glossary_aside(text)
    text = strip_html_comments(text)
    sys.stdout.write(text)


if __name__ == "__main__":
    main()
