# Cursor cloud agent image. glibc >= 2.38 (trixie) so commit signing works.
# Tools match local devbox: Hugo extended 0.145.0 (same as CI), just, git.

FROM debian:trixie-slim

ARG HUGO_VERSION=0.145.0

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# just (static binary)
RUN curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to /usr/local/bin

# Hugo extended, same version as .github/workflows/hugo.yaml
RUN curl -fsSL \
        "https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_extended_${HUGO_VERSION}_linux-amd64.tar.gz" \
        | tar -xz -C /usr/local/bin hugo

RUN command -v just && command -v git && command -v hugo && hugo version
