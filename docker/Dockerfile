# syntax=docker/dockerfile:1
# =============================================================================
# raab-analysis  (PUBLIC GHCR: ghcr.io/raabteam/raab-analysis)
#
# ONE multi-stage image (see docker/DL-2425-rabb-image-ci-plan.md):
#   - builder : restores the R library (renv.lock). Full toolchain + -dev
#               headers; discarded from the final image.
#   - app     : slim runtime. Runtime shared libs + TeXLive (Ubuntu apt packages)
#               + Node. COPYs the R library from builder, then the RAAB7 code LAST
#               (code changes only bust the final COPY layers). No CTAN/tinytex
#               downloads at build — TeXLive comes from the Ubuntu mirrors.
#
# Default CMD = the HTTP invocation server (http-server.js) on :8000. Local
# runs / K8s Jobs override the command to run Rscript directly (docker/run-local.sh).
#
#   Build (single-arch, loads into docker):
#     docker build --target app -t raab-analysis:dev .
#   Multi-arch publish is done by .github/workflows/image.yml (see docker/build.sh
#   for the equivalent local buildx invocation).
# =============================================================================

# Pinned multi-arch base (rocker/r-ver:4.4.3, Ubuntu 24.04 "noble", amd64+arm64).
ARG R_BASE=rocker/r-ver:4.4.3@sha256:3dae5d2eeddf74f10e0a81fb6b7ae350295e288000304f438b844b2c1e00fe2c

# ============================ builder stage ==================================
# Full toolchain (so arm64 can compile any package lacking a binary) + the
# generated -dev set (docker/sysreqs.txt). This whole stage is discarded.
FROM ${R_BASE} AS builder

ARG PPM=https://packagemanager.posit.co/cran/__linux__/noble/2026-06-01
ENV DEBIAN_FRONTEND=noninteractive

# Build-time system deps: toolchain + the generated -dev set (docker/sysreqs.txt).
COPY docker/sysreqs.txt /tmp/sysreqs.txt
RUN apt-get update && \
    apt-get install -y --no-install-recommends build-essential gfortran && \
    grep -vE '^[[:space:]]*#|^[[:space:]]*$' /tmp/sysreqs.txt \
      | xargs apt-get install -y --no-install-recommends && \
    rm -rf /var/lib/apt/lists/* /tmp/*

# Restore the R library from the committed lockfile into a FLAT library dir.
# renv auto-serves Linux binaries from PPM on noble (falls back to source on
# arm64 if a pinned version has no binary — hence the toolchain above).
COPY renv.lock /build/renv.lock
RUN R -e "options(repos=c(CRAN='${PPM}')); install.packages('renv')" && \
    R -e "options(repos=c(CRAN='${PPM}'), \
            HTTPUserAgent=sprintf('R/%s R (%s)', getRversion(), \
              paste(getRversion(), R.version[['platform']], R.version[['arch']], R.version[['os']]))); \
          renv::restore(lockfile='/build/renv.lock', library='/opt/R-lib', prompt=FALSE)"

# ============================ app (runtime) stage ============================
# The published image. Slim: runtime shared libs + TeXLive + Node — no compilers,
# no -dev headers. Carries the restored R library from the builder, then the
# RAAB7 code (copied LAST so code changes don't bust the deps layers).
FROM ${R_BASE} AS app

ENV DEBIAN_FRONTEND=noninteractive \
    R_LIBS_SITE=/opt/R-lib

# Runtime shared libs (names derived from apt — note Noble's t64 renames), Node
# (http-server.js), and TeXLive via APT. TeXLive is installed from Ubuntu's own
# packages (served by the same reliable Ubuntu mirrors as every other apt dep) —
# NOT from CTAN/tinytex — so the build has no flaky external TeX mirror dependency.
# noble ships TeXLive 2023, frozen with the distro. The texlive-* set below covers
# every LaTeX package the report needs (verified by the compile guard). tinytex
# (the R package, from renv.lock) drives this system TeXLive via latexmk/xelatex.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates fontconfig pandoc nodejs \
      libcurl4t64 libssl3t64 libxml2 zlib1g \
      libfontconfig1 libfreetype6 libfribidi0 libharfbuzz0b libpng16-16t64 \
      libtiff6 libjpeg-turbo8 libicu74 libglpk40 libuv1t64 libwebp7 libx11-6 \
      texlive-latex-base texlive-latex-recommended texlive-latex-extra \
      texlive-fonts-recommended texlive-xetex texlive-plain-generic lmodern latexmk \
    && rm -rf /var/lib/apt/lists/* /tmp/*

COPY --from=builder /opt/R-lib /opt/R-lib

# Build-time guard on the FINAL image: fail here (never in prod) if a runtime lib
# is missing or the LaTeX toolchain can't produce a PDF. (1) load every top-level
# R package (checks runtime shared libs); (2) render docker/latex-smoke.Rmd through
# the REAL rmarkdown -> pandoc -> xelatex path — this exercises the rmarkdown template
# preamble (e.g. lmodern) + kableExtra's injected deps + a figure, which a bare .tex
# missed. Prod has NO network, so a missing package at render is unrecoverable; this
# gate stops a broken image shipping (stale ones once had multirow.sty / lmodern.sty missing).
COPY docker/latex-smoke.Rmd /tmp/latex-smoke.Rmd
RUN R -e "pkgs <- c('rmarkdown','readxl','knitr','tinytex','kableExtra','RColorBrewer', \
            'RCurl','jsonlite','tidyverse','stringr','treemap','maditr','here', \
            'extrafont','float','systemfonts','svglite'); \
          invisible(lapply(pkgs, function(p) library(p, character.only=TRUE))); \
          cat('OK: all top-level packages load\n')" \
 && R -e "rmarkdown::render('/tmp/latex-smoke.Rmd', output_dir='/tmp', quiet=TRUE); \
          if (!file.exists('/tmp/latex-smoke.pdf')) stop('no PDF produced'); \
          cat('OK: rmarkdown -> xelatex renders a PDF\n')" \
 && rm -f /tmp/latex-smoke.*

# Belt-and-suspenders: the render session (Rscript, spawned by http-server.js)
# must never try to auto-install a LaTeX package (prod has no egress). Bake it
# into the site profile so it persists to runtime (build-session options() don't).
RUN printf '\n# Baked by docker/Dockerfile: no network TeX installs at render time.\noptions(tinytex.install_packages = FALSE, tinytex.tlmgr_update = FALSE)\n' \
      >> "$(R RHOME)/etc/Rprofile.site"

WORKDIR /raab7

# Code LAST. .here pins here() to /raab7; renv.lock kept for traceability
# (packages come from /opt/R-lib via R_LIBS_SITE — no renv activation at runtime).
# data/ (input, ro) and outputs/ (output, rw) are MOUNTED at runtime, not copied.
COPY RAAB7_scripts/ ./RAAB7_scripts/
COPY rmd_wrapper_PEEK_server.R renv.lock .here docker/http-server.js ./

# OCI + provenance metadata (no runtime effect; readable via `docker inspect`).
# ARGs declared LAST so a changed SHA/date rebuilds only this tiny layer. Values
# flow from docker/versions.env via build.sh / the CI workflow (build-args); the
# static ones are inline. Empty on a bare `docker build` that passes no args.
ARG R_BASE
ARG PPM_SNAPSHOT
ARG IMAGE_VERSION
ARG VCS_REF
ARG BUILD_DATE
LABEL org.opencontainers.image.title="raab-analysis" \
      org.opencontainers.image.description="RAAB7 report-generation environment (R + TeXLive)" \
      org.opencontainers.image.source="https://github.com/raabteam/raab-analysis" \
      org.opencontainers.image.version="${IMAGE_VERSION}" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.created="${BUILD_DATE}" \
      raab.r-base="${R_BASE}" \
      raab.ppm-snapshot="${PPM_SNAPSHOT}" \
      raab.texlive="apt-noble"

# TODO(non-root): running as an unprivileged user is a cheap hardening win, but
# the runtime-mounted outputs/ dir must then be writable by that uid. Until the
# caller (Peek deploy / K8s) is confirmed to own/allow that path, we stay root to
# avoid breaking the outputs/ write path. Revisit once the mount ownership is known.

EXPOSE 8000
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD node -e "require('http').get('http://127.0.0.1:8000/health',r=>process.exit(r.statusCode==200?0:1)).on('error',()=>process.exit(1))"

# Default = the HTTP invocation server. Local runs / K8s Jobs override with Rscript.
CMD ["node", "/raab7/http-server.js"]
