#!/usr/bin/env bash
# Run a RAAB7 report locally against the published image, WITHOUT rebuilding dependencies.
#
# Pulls (or reuses) ghcr.io/raabteam/raab-analysis and runs Rscript directly — skipping the
# image's default HTTP-server entrypoint. Your working tree is mounted at /raab7, so edits to
# .R/.Rmd take effect immediately (the R library lives at /opt/R-lib, outside /raab7, so this
# mount can't shadow it). data/ and outputs/ are subdirs of the mounted tree.
#
#   docker/run-local.sh <RAAB_ID>
#   RAAB_IMAGE=ghcr.io/raabteam/raab-analysis@sha256:<digest> docker/run-local.sh <RAAB_ID>
set -euo pipefail

RAAB_ID="${1:?usage: run-local.sh <RAAB_ID>}"
# Image to run. ':latest' floats to the newest published build. To run EXACTLY what
# Peek runs, override per-invocation with RAAB_IMAGE=...@sha256:<digest>, or pin the
# digest here and commit. See README → "Adopt a new image (pin the digest)".
IMAGE="${RAAB_IMAGE:-ghcr.io/raabteam/raab-analysis:latest}"
REPO="$(cd "$(dirname "$0")/.." && pwd)"

exec docker run --rm \
  -v "$REPO":/raab7 \
  "$IMAGE" \
  Rscript rmd_wrapper_PEEK_server.R "$RAAB_ID"
