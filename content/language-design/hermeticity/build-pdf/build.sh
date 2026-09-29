#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OUT_DIR="$SCRIPT_DIR/out"

# acm = existing Onward/acmart path (default, so prior recipes keep working)
# pj  = Programming Journal Art class (22-page main-body budget)
TARGET="${1:-acm}"

case "$TARGET" in
  acm)
    TEMPLATE="$SCRIPT_DIR/template.tex"
    ABSTRACT="$SRC_DIR/abstract.md"
    JOB="hermeticity"
    ;;
  pj)
    TEMPLATE="$SCRIPT_DIR/template-pj.tex"
    ABSTRACT="$SRC_DIR/pj-art-abstract.md"
    JOB="inert-programming"
    ;;
  *)
    echo "Usage: $0 [acm|pj]" >&2
    exit 2
    ;;
esac

mkdir -p "$OUT_DIR"

# 1) Preprocess markdown: strip Hugo frontmatter & embedded HTML, rewrite figures.
python3 "$SCRIPT_DIR/preprocess.py" "$SRC_DIR/index.md" > "$OUT_DIR/processed.md"

# 2) Pandoc → LaTeX with the selected template + Lua filter.
pandoc "$OUT_DIR/processed.md" \
  --metadata-file="$ABSTRACT" \
  --template="$TEMPLATE" \
  --lua-filter="$SCRIPT_DIR/transform.lua" \
  --listings \
  --standalone \
  --shift-heading-level-by=-1 \
  --output="$OUT_DIR/${JOB}.tex"

# 3) LaTeX → PDF (run from out/ so \graphicspath{{../../}} resolves to the
#    hermeticity source dir where the PNGs live).
if ! command -v latexmk >/dev/null 2>&1; then
  echo "latexmk not found. Run inside 'devbox shell' or 'devbox run build'." >&2
  exit 1
fi

# programming.cls lives next to this script; copy so latexmk finds it from out/.
if [[ "$TARGET" == "pj" ]]; then
  cp "$SCRIPT_DIR/programming.cls" "$OUT_DIR/programming.cls"
fi

cd "$OUT_DIR"
# lualatex (LuaTeX ≥1.17) exits immediately if LC_ALL/LANG is not an
# *exact* generated locale name. This image lists `en_US.utf8` but the
# environment exports `en_US.UTF-8`, which LuaTeX rejects with
# "Unable to read environment locale: exit now." C.UTF-8 is generated
# here and is enough for Unicode source.
export LANG=C.UTF-8 LC_ALL=C.UTF-8
# lualatex handles UTF-8 natively, including inside listings (\lstinline).
# pdflatex's listings is byte-level and chokes on chars like ∘, ≈, π.
latexmk -lualatex -interaction=nonstopmode -halt-on-error "${JOB}.tex"

echo
echo "PDF written to: $OUT_DIR/${JOB}.pdf"
