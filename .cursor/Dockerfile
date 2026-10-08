# Cursor cloud agent image. glibc >= 2.38 (trixie) so commit signing works.
# Tools match local devbox: Hugo extended 0.145.0 (same as CI), just, git.
# Base digest is the current debian:trixie-slim index (registry, 2026-10-08).

FROM debian:trixie-slim@sha256:a29215f6a35e51e22adffa17f89e9d2ef06214e64a2bad10d765c46aea49f11f

ARG TARGETARCH
ARG HUGO_VERSION=0.145.0

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    just \
    && rm -rf /var/lib/apt/lists/*

# Hugo extended, same version as .github/workflows/hugo.yaml.
# sha256 values are the linux-{amd64,arm64} lines from
# https://github.com/gohugoio/hugo/releases/download/v0.145.0/hugo_0.145.0_checksums.txt
RUN set -eux; \
    case "${TARGETARCH}" in \
      amd64) hugo_arch=amd64; hugo_sha256=7c7468946da8fa282508e5bf3b8d6e972f3cae41e826d45b4a9aa9a104a74ae4 ;; \
      arm64) hugo_arch=arm64; hugo_sha256=ec6543e43efc96dd40aacb578413bb3d0532a3e8898d49edff6528f06333866e ;; \
      *) echo "unsupported TARGETARCH=${TARGETARCH}" >&2; exit 1 ;; \
    esac; \
    tarball="hugo_extended_${HUGO_VERSION}_linux-${hugo_arch}.tar.gz"; \
    curl -fsSL -o "/tmp/${tarball}" \
      "https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/${tarball}"; \
    curl -fsSL -o "/tmp/hugo_${HUGO_VERSION}_checksums.txt" \
      "https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_${HUGO_VERSION}_checksums.txt"; \
    grep -E "^${hugo_sha256}  ${tarball}\$" "/tmp/hugo_${HUGO_VERSION}_checksums.txt"; \
    echo "${hugo_sha256}  /tmp/${tarball}" | sha256sum -c -; \
    tar -xz -C /usr/local/bin -f "/tmp/${tarball}" hugo; \
    rm -f "/tmp/${tarball}" "/tmp/hugo_${HUGO_VERSION}_checksums.txt"

RUN command -v just && command -v git && command -v hugo && hugo version && just --version
