set shell := ["bash", "-euo", "pipefail", "-c"]
set dotenv-load := true

# List recipes
default:
    @just --list

# Initialize theme and syndication git submodules
submodules:
    git submodule update --init --recursive

# Local preview (hugo server); initializes theme submodules first
serve: submodules
    hugo server

# Production Hugo build into public/
build: submodules
    hugo --gc --minify

# Every CI gate: production Hugo build
check: build

# Optional link check (needs the local server on port 1313)
linkcheck:
    linklint -http -host localhost:1313 /@ -doc linklint.out && open linklint.out/index.html
