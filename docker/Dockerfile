# syntax=docker/dockerfile:1
# =============================================================================
# peek-raab  (PRIVATE ECR)  —  the RAAB7 code, layered on the frozen base (~quarterly)
#
# Installs NOTHING. Just COPYs code onto peek-raab-base. If a new dependency is
# ever needed it goes in docker/Dockerfile.base, not here (that keeps this image
# unable to drift from the base). See docker/RENV_MIGRATION_PLAN.md (§3, §8).
#
#   Build (multi-arch; pins the exact base by digest):
#     docker buildx build -f docker/Dockerfile \
#       --build-arg BASE_IMAGE=<ecr>/peek-raab-base@<digest> \
#       --platform linux/amd64,linux/arm64 \
#       --target prod -t <ecr>/peek-raab:<tag> --push .
#
#   Run a report (data/ + outputs/ mounted at runtime):
#     docker run --rm -v ./data:/raab7/data:ro -v ./outputs:/raab7/outputs \
#       <ecr>/peek-raab:<tag> Rscript rmd_wrapper_PEEK_server.R <RAAB_ID>
# =============================================================================

ARG BASE_IMAGE=peek-raab-base:latest

# ---- app: code on the base (dev target; no SSH) ----
FROM ${BASE_IMAGE} AS app
WORKDIR /raab7
# .here pins here() to /raab7; renv.lock kept for traceability (packages come
# from the base's /opt/R-lib via R_LIBS_SITE — no renv activation at runtime).
COPY RAAB7_scripts/ ./RAAB7_scripts/
COPY rmd_wrapper_PEEK_server.R renv.lock .here ./
# data/ (input, ro) and outputs/ (output, rw) are MOUNTED at runtime — not copied.

# ---- prod: app + SSH server (matches the existing cloud invocation model) ----
FROM app AS prod
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends openssh-server sudo \
    && rm -rf /var/lib/apt/lists/* && mkdir -p /var/run/sshd
EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
