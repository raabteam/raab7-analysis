#!/usr/bin/env bash
# Regenerate renv.lock + docker/sysreqs.txt (Steps 1-2) inside a Noble container.
# Wraps docker/generate-deps.R, feeding it the pinned PPM snapshot from versions.env.
set -euo pipefail
cd "$(dirname "$0")/.."
# shellcheck disable=SC1091
source docker/versions.env

# Build arch for generation (lock is arch-independent; amd64 keeps it fast/binary).
GEN_PLATFORM="${GEN_PLATFORM:-linux/amd64}"
R_TAG="$(printf '%s' "$R_BASE" | sed 's/@.*//')"   # rocker/r-ver:4.4.3 (tag, no digest)

echo ">> Generating deps on ${R_TAG} (${GEN_PLATFORM}), PPM snapshot ${PPM_SNAPSHOT}"
docker run --rm --platform "$GEN_PLATFORM" \
  -e PPM_SNAPSHOT="$PPM_SNAPSHOT" \
  -v "$PWD":/work -w /tmp/proj "$R_TAG" bash -lc '
    apt-get update -qq && apt-get install -y --no-install-recommends \
      build-essential cmake pkg-config libssl-dev libsasl2-dev \
      libcurl4-openssl-dev libxml2-dev libfontconfig1-dev libharfbuzz-dev \
      libfribidi-dev libfreetype-dev libpng-dev libtiff5-dev libjpeg-dev \
      libglpk-dev libgmp-dev libv8-dev zlib1g-dev libbz2-dev >/dev/null &&
    Rscript /work/docker/generate-deps.R'
echo ">> Updated: renv.lock, .Rprofile, renv/activate.R, docker/sysreqs.txt"
