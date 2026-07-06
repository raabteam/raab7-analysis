# syntax=docker/dockerfile:1
# =============================================================================
# raab-analysis  (PUBLIC GHCR: ghcr.io/raabteam/raab-analysis)
#
# ONE multi-stage image (see docker/DL-2425-RAAB_IMAGE_CI_PLAN.md):
#   - builder : installs the R library (renv.lock) + TeXLive.  Heavy, rarely
#               changes.  Holds the full toolchain + -dev headers; discarded.
#   - app     : slim runtime.  Runtime shared libs + Node only.  COPYs the R
#               library + TeXLive from builder, then the RAAB7 code LAST so a
#               code change only busts the final COPY layers (deps stay cached).
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

# TeXLive via tinytex (renv does NOT manage LaTeX). TinyTeX publishes versioned
# bundles only for x86_64, so for cross-arch (amd64+arm64) consistency we install
# the 'daily' bundle on BOTH arches from the default current repository.
# Reproducibility comes from the frozen image digest (plan §2), not a pinned
# TeXLive year. extra_packages = the LaTeX packages the RAAB7 report needs.
ENV R_LIBS_SITE=/opt/R-lib
RUN R -e "library(tinytex); \
    deps <- c('fancyhdr','tabu','float','lipsum','multirow','wrapfig','colortbl', \
              'pdflscape','threeparttable','threeparttablex','environ','ulem', \
              'makecell','caption','bookmark'); \
    install_tinytex(force=TRUE, dir='/opt/TinyTeX', version='daily', extra_packages=deps); \
    options(tinytex.tlmgr_update=FALSE, tinytex.install_packages=FALSE)"

# ============================ app (runtime) stage ============================
# The published image. Slim: runtime shared libs + Node only — no compilers,
# no -dev headers. Carries the restored R library + TeXLive from the builder,
# then the RAAB7 code (copied LAST so code changes don't bust the deps layers).
FROM ${R_BASE} AS app

ENV DEBIAN_FRONTEND=noninteractive \
    R_LIBS_SITE=/opt/R-lib \
    PATH=/opt/TinyTeX/bin/x86_64-linux:/opt/TinyTeX/bin/aarch64-linux:$PATH

# Runtime shared libs (names derived from apt — note Noble's t64 renames),
# pandoc (rmarkdown), fontconfig/ca-certificates, full perl (tlmgr needs
# File::Find, not just perl-base), and nodejs (for http-server.js).
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates fontconfig pandoc \
      perl \
      nodejs \
      libcurl4t64 libssl3t64 libxml2 zlib1g \
      libfontconfig1 libfreetype6 libfribidi0 libharfbuzz0b libpng16-16t64 \
      libtiff6 libjpeg-turbo8 libicu74 libglpk40 libuv1t64 libwebp7 libx11-6 \
    && rm -rf /var/lib/apt/lists/* /tmp/*

COPY --from=builder /opt/R-lib   /opt/R-lib
COPY --from=builder /opt/TinyTeX /opt/TinyTeX

# Build-time guard: fail the build here (never in prod) if a runtime lib is
# missing or TeXLive isn't wired up. Loads every top-level package + checks tlmgr.
RUN R -e "pkgs <- c('rmarkdown','readxl','knitr','tinytex','kableExtra','RColorBrewer', \
            'RCurl','jsonlite','tidyverse','stringr','treemap','maditr','here', \
            'extrafont','float','systemfonts','svglite'); \
          invisible(lapply(pkgs, function(p) library(p, character.only=TRUE))); \
          cat('OK: all top-level packages load\n')" \
 && tlmgr --version > /dev/null && echo "OK: tlmgr on PATH"

WORKDIR /raab7

# Code LAST. .here pins here() to /raab7; renv.lock kept for traceability
# (packages come from /opt/R-lib via R_LIBS_SITE — no renv activation at runtime).
# data/ (input, ro) and outputs/ (output, rw) are MOUNTED at runtime, not copied.
COPY RAAB7_scripts/ ./RAAB7_scripts/
COPY rmd_wrapper_PEEK_server.R renv.lock .here docker/http-server.js ./

# TODO(non-root): running as an unprivileged user is a cheap hardening win, but
# the runtime-mounted outputs/ dir must then be writable by that uid. Until the
# caller (Peek deploy / K8s) is confirmed to own/allow that path, we stay root to
# avoid breaking the outputs/ write path. Revisit once the mount ownership is known.

EXPOSE 8000
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD node -e "require('http').get('http://127.0.0.1:8000/health',r=>process.exit(r.statusCode==200?0:1)).on('error',()=>process.exit(1))"

# Default = the HTTP invocation server. Local runs / K8s Jobs override with Rscript.
CMD ["node", "/raab7/http-server.js"]
