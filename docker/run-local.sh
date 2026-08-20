#!/usr/bin/env bash
# Run a RAAB7 report locally against the published image, WITHOUT rebuilding dependencies.
#
# Pulls (or reuses) ghcr.io/raabteam/raab-analysis and runs Rscript directly — skipping the
# image's default HTTP-server entrypoint. Your working tree is mounted at /raab7, so edits to
# .R/.Rmd take effect immediately (the R library lives at /opt/R-lib, outside /raab7, so this
# mount can't shadow it). Outputs land in outputs/<raab_id>/.
#
#   docker/run-local.sh <survey>      # <survey> = a folder under data/ (e.g. data/Nord/)
#   docker/run-local.sh <raab_id>     # or a raw raab_id, if data/ holds flat merged CSVs
#   RAAB_IMAGE=ghcr.io/raabteam/raab-analysis@sha256:<digest> docker/run-local.sh <survey>
#
# The R code expects prod's layout: data/surveys.csv + population.csv + meta.csv, filtered by
# a UUID raab_id (that's what Peek provides). Locally you keep one folder per survey instead
# (data/Nord/, data/Nakuru/, ... — gitignored), so when the argument names such a folder this
# script bind-mounts it over /raab7/data and reads the raab_id out of its meta.csv. The R code
# stays byte-identical to what prod runs.
set -euo pipefail

ARG="${1:?usage: run-local.sh <survey-folder-under-data/ or raab_id>}"
# Image to run. ':latest' floats to the newest published build. To run EXACTLY what
# Peek runs, override per-invocation with RAAB_IMAGE=...@sha256:<digest>, or pin the
# digest here and commit. See README → "Adopt a new image (pin the digest)".
IMAGE="${RAAB_IMAGE:-ghcr.io/raabteam/raab-analysis:latest}"
REPO="$(cd "$(dirname "$0")/.." && pwd)"

DATA_MOUNT=()
if [[ -d "$REPO/data/$ARG" ]]; then
  DATA_DIR="$REPO/data/$ARG"
  for f in surveys.csv population.csv meta.csv; do
    [[ -f "$DATA_DIR/$f" ]] || { echo "error: $DATA_DIR/$f is missing" >&2; exit 1; }
  done
  # The report is keyed by the survey's UUID raab_id, not the folder name — take it
  # from meta.csv (first field of the single data row; strip any CRLF).
  RAAB_ID="$(awk -F, 'NR==2{print $1; exit}' "$DATA_DIR/meta.csv" | tr -d '\r')"
  [[ -n "$RAAB_ID" ]] || { echo "error: no raab_id found in $DATA_DIR/meta.csv" >&2; exit 1; }
  echo "survey '$ARG' -> raab_id $RAAB_ID (mounting $DATA_DIR as /raab7/data)"
  DATA_MOUNT=(-v "$DATA_DIR":/raab7/data)
else
  RAAB_ID="$ARG"
fi

# A floating tag (e.g. ':latest') can go stale in the local docker cache — `docker run`
# never re-checks the registry once a tag exists locally, and a stale image fails late
# and confusingly (e.g. "LaTeX Error: File `lmodern.sty' not found" mid-render). Refresh
# it up front; if offline, fall back to the cached image. Pinned @sha256 refs are
# immutable, so no pull needed.
if [[ "$IMAGE" != *"@sha256:"* ]]; then
  docker pull "$IMAGE" || echo "warning: could not pull $IMAGE; using the locally cached image" >&2
fi

# RENV_CONFIG_AUTOLOADER_ENABLED=FALSE: the mounted tree carries .Rprofile ->
# renv/activate.R (used for local interactive dev), but in the container packages
# must come from /opt/R-lib via R_LIBS_SITE, exactly as in prod (whose image has
# no .Rprofile). Without this, renv activates against an empty renv/library in
# the mount and every library() call fails.
exec docker run --rm \
  -v "$REPO":/raab7 \
  ${DATA_MOUNT[@]+"${DATA_MOUNT[@]}"} \
  -e RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  "$IMAGE" \
  Rscript rmd_wrapper_PEEK_server.R "$RAAB_ID"
