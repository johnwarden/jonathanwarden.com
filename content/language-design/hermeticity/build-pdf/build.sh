#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OUT_DIR="$SCRIPT_DIR/out"

mkdir -p "$OUT_DIR"

# 1) Preprocess markdown: strip Hugo frontmatter & embedded HTML, rewrite figures.
python3 "$SCRIPT_DIR/preprocess.py" "$SRC_DIR/index.md" > "$OUT_DIR/processed.md"

# 2) Pandoc → LaTeX with the acmart template + Lua filter.
pandoc "$OUT_DIR/processed.md" \
  --metadata-file="$SRC_DIR/abstract.md" \
  --template="$SCRIPT_DIR/template.tex" \
  --lua-filter="$SCRIPT_DIR/transform.lua" \
  --listings \
  --standalone \
  --shift-heading-level-by=-1 \
  --output="$OUT_DIR/hermeticity.tex"

# 3) LaTeX → PDF (run from out/ so \graphicspath{{../../}} resolves to the
#    hermeticity source dir where the PNGs live).
if ! command -v latexmk >/dev/null 2>&1; then
  echo "latexmk not found. Run inside 'devbox shell' or 'devbox run build'." >&2
  exit 1
fi

cd "$OUT_DIR"
# lualatex handles UTF-8 natively, including inside listings (\lstinline).
# pdflatex's listings is byte-level and chokes on chars like ∘, ≈, π.
latexmk -lualatex -interaction=nonstopmode -halt-on-error hermeticity.tex

echo
echo "PDF written to: $OUT_DIR/hermeticity.pdf"
