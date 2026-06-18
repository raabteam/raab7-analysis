#!/usr/bin/env bash
# Build peek-raab-base (public ECR). All version inputs come from versions.env.
#   Local single-arch (loads into docker):   ./docker/build-base.sh
#   Multi-arch push to ECR:                  PUSH=true ./docker/build-base.sh
set -euo pipefail
cd "$(dirname "$0")/.."
# shellcheck disable=SC1091
source docker/versions.env

GIT_SHA="$(git rev-parse --short HEAD 2>/dev/null || echo nogit)"
BUILD_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
PUSH="${PUSH:-false}"
PLAT="${PLATFORMS}"

# buildx --load cannot load a multi-arch manifest; require PUSH for multi-arch.
OUT=(--load)
if [ "$PUSH" = "true" ]; then OUT=(--push); fi
case "$PLAT" in
  *,*) [ "$PUSH" = "true" ] || { echo "ERROR: multi-arch ($PLAT) needs PUSH=true (or set PLATFORMS=linux/amd64 for a local --load build)"; exit 1; } ;;
esac

set -x
docker buildx build -f docker/Dockerfile.base \
  --platform "$PLAT" \
  --build-arg R_BASE="$R_BASE" \
  --build-arg PPM="https://packagemanager.posit.co/cran/__linux__/noble/${PPM_SNAPSHOT}" \
  --label "org.opencontainers.image.title=peek-raab-base" \
  --label "org.opencontainers.image.version=${BASE_VERSION}" \
  --label "org.opencontainers.image.revision=${GIT_SHA}" \
  --label "org.opencontainers.image.created=${BUILD_DATE}" \
  --label "raab.r-base=${R_BASE}" \
  --label "raab.ppm-snapshot=${PPM_SNAPSHOT}" \
  --label "raab.texlive-bundle=${TEXLIVE_BUNDLE}" \
  -t "${BASE_REPO}:${BASE_VERSION}" \
  -t "${BASE_REPO}:git-${GIT_SHA}" \
  -t "${BASE_REPO}:latest" \
  "${OUT[@]}" .
set +x

echo
echo "Built ${BASE_REPO}:${BASE_VERSION} (git-${GIT_SHA})"
if [ "$PUSH" = "true" ]; then
  echo "Pin the immutable digest for the code build with:"
  echo "  docker buildx imagetools inspect ${BASE_REPO}:${BASE_VERSION} | grep Digest"
fi
