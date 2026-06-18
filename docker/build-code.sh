#!/usr/bin/env bash
# Build peek-raab (private ECR) = code layered on a frozen base.
# Pass the base by DIGEST for reproducibility (a mutable tag is warned about).
#   BASE_REF=<repo>@sha256:... ./docker/build-code.sh
#   Multi-arch push:  PUSH=true BASE_REF=...@sha256:... ./docker/build-code.sh
set -euo pipefail
cd "$(dirname "$0")/.."
# shellcheck disable=SC1091
source docker/versions.env

BASE_REF="${BASE_REF:-${BASE_REPO}:latest}"
CODE_TAG="${CODE_TAG:-$(git rev-parse --short HEAD 2>/dev/null || echo dev)}"
TARGET="${TARGET:-prod}"            # app (dev) | prod (adds SSH)
PUSH="${PUSH:-false}"
PLAT="${PLATFORMS}"

case "$BASE_REF" in
  *@sha256:*) : ;;
  *) echo "WARNING: BASE_REF '$BASE_REF' is a mutable tag, not a @sha256 digest — the code image won't be reproducibly pinned to a base (plan §2)." ;;
esac

OUT=(--load)
if [ "$PUSH" = "true" ]; then OUT=(--push); fi
case "$PLAT" in
  *,*) [ "$PUSH" = "true" ] || { echo "ERROR: multi-arch ($PLAT) needs PUSH=true (or PLATFORMS=linux/amd64 for local --load)"; exit 1; } ;;
esac

GIT_SHA="$(git rev-parse --short HEAD 2>/dev/null || echo nogit)"
set -x
docker buildx build -f docker/Dockerfile \
  --platform "$PLAT" \
  --target "$TARGET" \
  --build-arg BASE_IMAGE="$BASE_REF" \
  --label "org.opencontainers.image.title=peek-raab" \
  --label "org.opencontainers.image.revision=${GIT_SHA}" \
  --label "raab.base-image=${BASE_REF}" \
  -t "${CODE_REPO}:${CODE_TAG}" \
  -t "${CODE_REPO}:latest" \
  "${OUT[@]}" .
set +x

echo
echo "Built ${CODE_REPO}:${CODE_TAG} (target=${TARGET})"
echo "  base: ${BASE_REF}"
