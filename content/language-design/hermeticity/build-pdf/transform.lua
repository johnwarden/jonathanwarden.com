-- Pandoc Lua filter for hermeticity → acmart conversion.
--
-- Handles:
--   * Wrapping ::: glossary fenced divs in a tcolorbox.
--   * Forcing language= on lstlisting blocks for languages that pandoc's
--     --listings writer doesn't whitelist (typescript, rust).

local function raw_latex(s)
  return pandoc.RawBlock("latex", s)
end

local LATEX_SPECIAL = {
  ["\\"] = "\\textbackslash{}",
  ["&"]  = "\\&",
  ["%"]  = "\\%",
  ["$"]  = "\\$",
  ["#"]  = "\\#",
  ["_"]  = "\\_",
  ["{"]  = "\\{",
  ["}"]  = "\\}",
  ["~"]  = "\\textasciitilde{}",
  ["^"]  = "\\textasciicircum{}",
}

function Div(el)
  if el.classes:includes("glossary") then
    local result = pandoc.List()
    -- mdframed (rather than tcolorbox) so footnotes inside the box
    -- still flow to the page bottom instead of being captured.
    -- No roundcorner=… here: that requires framemethod=tikz, which would
    -- capture footnotes inside the box. We use the default framemethod so
    -- footnote text inside the glossary still flows to the page bottom.
    result:insert(raw_latex(
      "\\begin{mdframed}[backgroundcolor=black!4,linecolor=black!50,linewidth=0.4pt,innertopmargin=6pt,innerbottommargin=6pt,skipabove=\\baselineskip,skipbelow=6pt]\\setlength{\\parindent}{0pt}\\setlength{\\leftmargini}{1.2em}"))
    for _, b in ipairs(el.content) do
      result:insert(b)
    end
    result:insert(raw_latex("\\end{mdframed}"))
    return result
  elseif el.classes:includes("example-label") then
    -- Render as a no-indent bold "label" glued to the following block
    -- (figure float, code listing, etc.). Pseudo-heading semantics: looks
    -- like a header but doesn't go in the TOC and isn't numbered.
    local text = pandoc.utils.stringify(el.content)
    local escaped = text:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
    return raw_latex(
      "\\par\\medskip\\noindent\\textbf{" .. escaped .. "}\\par\\nopagebreak\\smallskip"
    )
  end
end

-- We override every CodeBlock so we can:
--   1) prepend \needspace{...} so short listings don't split across pages
--   2) emit language=… for languages that pandoc's --listings whitelist drops
--      (e.g. typescript, rust); the matching \lstdefinelanguage entries live
--      in the template.
function CodeBlock(el)
  -- Estimate line count of the listing.
  local _, newlines = el.text:gsub("\n", "")
  local line_count = newlines + 1
  -- listings is set in \footnotesize, so each line is ~0.85 of the body-text
  -- \baselineskip that \needspace measures against. Two adjustments:
  --   1) scale by 0.85 so the request matches actual listing height,
  --   2) cap at 5 baselineskips total — only short listings are at real risk
  --      of looking bad if split across columns; longer ones can break
  --      naturally without the column above being half-empty. The original
  --      `lines + 2` (no scaling, no cap) caused spurious column breaks when
  --      a column had room for the actual listing but not the inflated demand.
  local needed_lines = math.min(math.ceil(line_count * 0.85), 5)
  local lang_attr = ""
  if #el.classes > 0 then
    lang_attr = "[language=" .. el.classes[1] .. "]"
  end
  local out = string.format(
    "\\needspace{%d\\baselineskip}\n\\begin{lstlisting}%s\n%s\n\\end{lstlisting}",
    needed_lines, lang_attr, el.text
  )
  return raw_latex(out)
end

-- pandoc's LaTeX writer emits longtable for every table, but longtable does
-- not work in 2-column mode (sigplan). Rewrite Table elements to a plain
-- tabular wrapped in a single-column table float.
local function cell_text(cell)
  return pandoc.utils.stringify(cell.contents)
end

local function row_to_latex(row)
  local cells = {}
  for _, c in ipairs(row.cells) do
    table.insert(cells, cell_text(c))
  end
  return table.concat(cells, " & ") .. " \\\\"
end

-- Inline code: render with \texttt instead of \lstinline. \lstinline is
-- fragile (multi-byte UTF-8 chars trigger "ended by EOL" errors, fails in
-- macro arguments) and we don't really need syntax highlighting inline.
function Code(el)
  local s = el.text:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
  return pandoc.RawInline("latex", "\\texttt{" .. s .. "}")
end

-- acmart requires every figure to carry a \Description{} for accessibility.
-- Pandoc emits Figure → \begin{figure}…\caption{…}\end{figure}, but no
-- \Description, which triggers a class warning. We append one whose text is
-- the (alt-text or caption) of the figure.
function Figure(el)
  local desc
  if el.caption and el.caption.long and #el.caption.long > 0 then
    desc = pandoc.utils.stringify(el.caption.long)
  else
    desc = "Figure"
  end
  -- Escape LaTeX specials in description text.
  desc = desc:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
  table.insert(el.content, pandoc.RawBlock("latex", "\\Description{" .. desc .. "}"))
  return el
end

-- Force the "## Appendices" section onto a new page. Single break before the
-- whole appendix block (not before each individual appendix) — keeps the
-- appendices flowing continuously after the boundary while still separating
-- them from the body. \clearpage in 2-column mode flushes pending floats.
--
-- For subsection-level headings, prepend a small \needspace so the heading
-- can't be placed at the very bottom of a column with its body continuing
-- in the next column (orphan-heading). 4 baselineskips comfortably fits
-- the heading itself plus 2–3 lines of body text.
function Header(el)
  local text = pandoc.utils.stringify(el.content)
  if text == "Appendices" or text == "References" then
    return {
      raw_latex("\\clearpage"),
      el,
    }
  elseif el.level == 3 then
    -- The filter sees pre-shift levels: ### in markdown is level 3 here,
    -- which becomes \subsection after --shift-heading-level-by=-1. We add
    -- \needspace to subsection headings only, not \section: \section already
    -- has good orphan protection in acmart, and forcing column breaks before
    -- it caused visible whitespace gaps further up the column (acmart uses
    -- \flushbottom, which stretches glue to fill foreshortened columns —
    -- the stretched glue lands as extra space above downstream subsections).
    return {
      raw_latex("\\needspace{4\\baselineskip}"),
      el,
    }
  end
end

function Table(el)
  local cols = string.rep("l", #el.colspecs)
  local lines = {
    -- [H] (no-float, exactly here) so the table can't drift above the
    -- heading that introduces it. Requires \usepackage{float} (already
    -- loaded for figure binding).
    "\\begin{table}[H]",
    "\\centering",
    "\\begin{tabular}{@{}" .. cols .. "@{}}",
    "\\toprule",
  }
  for _, row in ipairs(el.head.rows) do
    table.insert(lines, row_to_latex(row))
  end
  table.insert(lines, "\\midrule")
  for _, body in ipairs(el.bodies) do
    for _, row in ipairs(body.body) do
      table.insert(lines, row_to_latex(row))
    end
  end
  table.insert(lines, "\\bottomrule")
  table.insert(lines, "\\end{tabular}")
  table.insert(lines, "\\end{table}")
  return raw_latex(table.concat(lines, "\n"))
end

-- Combine an "example-label" Div with the immediately-following code listing
-- or figure into a single raw-LaTeX block. The standalone label/lstlisting
-- emits two separate \needspace/\nopagebreak attempts: \nopagebreak after the
-- label tries to keep them together, but \needspace inside the lstlisting
-- output is a stronger break, so when the column is short the label gets
-- stranded on the old page while the listing jumps to the new one. By
-- merging the two and emitting a single \needspace sized for label+listing
-- before the label, the break (if any) happens in front of the label, so
-- the pair stays together. For figures we additionally force [H] placement
-- (from the float package) so the figure doesn't drift to a page top.
local function combine_label_with_following(blocks)
  local out = pandoc.List()
  local i = 1
  while i <= #blocks do
    local b = blocks[i]
    local nxt = blocks[i + 1]

    local is_label = b.t == "Div"
      and b.classes
      and b.classes:includes("example-label")

    if b.t == "Header"
      and b.level == 4
      and nxt
      and nxt.t == "Table" then
      -- Markdown #### becomes \subsubsection after --shift-heading-level-by=-1.
      -- acmart's \subsubsection (like \paragraph) is RUN-IN (negative third
      -- arg to \@startsection): the bold heading is queued to attach to the
      -- next paragraph's first line. When the next block is a table — not
      -- prose — the heading has nothing to attach to and ends up rendered
      -- after (or visually below) the table. Emit a displayed-style header
      -- instead.
      local text = pandoc.utils.stringify(b.content)
      local escaped = text:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
      local label_part = ""
      if b.identifier and b.identifier ~= "" then
        label_part = "\\label{" .. b.identifier .. "}"
      end
      out:insert(raw_latex(string.format(
        "\\par\\medskip\\noindent\\textbf{%s}%s\\par\\nopagebreak\\smallskip",
        escaped, label_part
      )))
      i = i + 1  -- consume header only; table flows to the Table filter
    elseif is_label and nxt and nxt.t == "CodeBlock" then
      local text = pandoc.utils.stringify(b.content)
      local label = text:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
      local _, newlines = nxt.text:gsub("\n", "")
      -- Same logic as the standalone CodeBlock filter (cap at ~5 listing
      -- baselineskips, scaled to footnotesize) plus 3 baselineskips for the
      -- label (medskip + line + smallskip). Total cap ~8.
      local needed = math.min(math.ceil((newlines + 1) * 0.85), 5) + 3
      local lang_attr = ""
      if #nxt.classes > 0 then
        lang_attr = "[language=" .. nxt.classes[1] .. "]"
      end
      local combined = string.format(
        "\\par\\medskip\\needspace{%d\\baselineskip}\\noindent\\textbf{%s}\\par\\nopagebreak\\smallskip\n\\begin{lstlisting}%s\n%s\n\\end{lstlisting}",
        needed, label, lang_attr, nxt.text
      )
      out:insert(raw_latex(combined))
      i = i + 2
    elseif is_label
      and nxt
      and nxt.t == "RawBlock"
      and nxt.format == "latex"
      and nxt.text:match("\\begin{figure}") then

      local text = pandoc.utils.stringify(b.content)
      local label = text:gsub(".", function(c) return LATEX_SPECIAL[c] or c end)
      -- Force [H] placement so the figure stays where it's placed (no float).
      local fig = nxt.text:gsub("\\begin{figure}%[[^%]]*%]", "\\begin{figure}[H]")
      if not fig:match("\\begin{figure}%[H%]") then
        fig = fig:gsub("\\begin{figure}", "\\begin{figure}[H]", 1)
      end
      -- Conservative reservation; can't read image height from here.
      local combined = string.format(
        "\\par\\medskip\\needspace{14\\baselineskip}\\noindent\\textbf{%s}\\par\\nopagebreak\\smallskip\n%s",
        label, fig
      )
      out:insert(raw_latex(combined))
      i = i + 2
    else
      out:insert(b)
      i = i + 1
    end
  end
  return out
end

return {
  { Blocks = combine_label_with_following },
  {
    Div = Div,
    CodeBlock = CodeBlock,
    Code = Code,
    Figure = Figure,
    Header = Header,
    Table = Table,
  },
}
