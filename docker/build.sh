#!/usr/bin/env bash
# Build the single raab-analysis image (docker/Dockerfile, target=app).
# All version inputs come from docker/versions.env. Consolidates the old
# build-base.sh + build-code.sh. CI (.github/workflows/image.yml) does the
# multi-arch publish; this script is for local builds and manual pushes.
#
#   Local single-arch (loads into docker):   ./docker/build.sh
#   Multi-arch push to GHCR:                  PUSH=true ./docker/build.sh
#   Pin a tag / platform:  IMAGE_TAG=foo PLATFORMS=linux/arm64 ./docker/build.sh
set -euo pipefail
cd "$(dirname "$0")/.."
# shellcheck disable=SC1091
source docker/versions.env

GIT_SHA="$(git rev-parse --short HEAD 2>/dev/null || echo nogit)"
BUILD_DATE="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
IMAGE_TAG="${IMAGE_TAG:-git-${GIT_SHA}}"
PUSH="${PUSH:-false}"
PLAT="${PLATFORMS}"

# buildx --load cannot load a multi-arch manifest; require PUSH for multi-arch.
OUT=(--load)
if [ "$PUSH" = "true" ]; then OUT=(--push); fi
case "$PLAT" in
  *,*) [ "$PUSH" = "true" ] || { echo "ERROR: multi-arch ($PLAT) needs PUSH=true (or set PLATFORMS=linux/amd64 for a local --load build)"; exit 1; } ;;
esac

set -x
docker buildx build -f docker/Dockerfile \
  --target app \
  --platform "$PLAT" \
  --build-arg R_BASE="$R_BASE" \
  --build-arg PPM="https://packagemanager.posit.co/cran/__linux__/noble/${PPM_SNAPSHOT}" \
  --label "org.opencontainers.image.title=raab-analysis" \
  --label "org.opencontainers.image.version=${IMAGE_VERSION}" \
  --label "org.opencontainers.image.revision=${GIT_SHA}" \
  --label "org.opencontainers.image.created=${BUILD_DATE}" \
  --label "raab.r-base=${R_BASE}" \
  --label "raab.ppm-snapshot=${PPM_SNAPSHOT}" \
  --label "raab.texlive-bundle=${TEXLIVE_BUNDLE}" \
  -t "${IMAGE}:${IMAGE_TAG}" \
  -t "${IMAGE}:${IMAGE_VERSION}" \
  -t "${IMAGE}:latest" \
  "${OUT[@]}" .
set +x

echo
echo "Built ${IMAGE}:${IMAGE_TAG} (${IMAGE_VERSION}, git-${GIT_SHA})"
if [ "$PUSH" = "true" ]; then
  echo "Pin the immutable multi-arch digest for consumers with:"
  echo "  docker buildx imagetools inspect ${IMAGE}:${IMAGE_VERSION} | grep Digest"
fi
