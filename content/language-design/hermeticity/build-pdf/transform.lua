-- Pandoc Lua filter for hermeticity → acmart conversion.
--
-- Handles:
--   * Wrapping ::: glossary fenced divs in a tcolorbox.
--   * Forcing language= on lstlisting blocks for languages that pandoc's
--     --listings writer doesn't whitelist (typescript, rust).

local function raw_latex(s)
  return pandoc.RawBlock("latex", s)
end

function Div(el)
  if el.classes:includes("glossary") then
    local result = pandoc.List()
    -- mdframed (rather than tcolorbox) so footnotes inside the box
    -- still flow to the page bottom instead of being captured.
    result:insert(raw_latex(
      "\\begin{mdframed}[backgroundcolor=black!4,linecolor=black!50,linewidth=0.4pt,roundcorner=2pt,innertopmargin=6pt,innerbottommargin=6pt]"))
    for _, b in ipairs(el.content) do
      result:insert(b)
    end
    result:insert(raw_latex("\\end{mdframed}"))
    return result
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
  -- listings is set in \footnotesize, so each line is roughly 0.85 of normal
  -- baselineskip. Ask for line_count + 2 baselineskips of slack — enough to
  -- cover the frame and a buffer line, but not so generous that we fragment
  -- columns unnecessarily.
  local needed_lines = line_count + 2
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

function Table(el)
  local cols = string.rep("l", #el.colspecs)
  local lines = {
    "\\begin{table}[ht]",
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
