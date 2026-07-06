# RAAB analysis Docker image

This repo builds **one** public image on **GitHub Container Registry (GHCR)** and publishes it from its
own GitHub Actions — no AWS, no external registry credentials. Consumers (the Peek servers, other
maintainers) pull it without any credentials.

> The full design + setup (image, workflow, `http-server.js`, validation) is in
> `DL-2425-RAAB_IMAGE_CI_PLAN.md`. This README is the day-to-day operational guide.

## One image

`ghcr.io/raabteam/raab-analysis` — a single **multi-stage** image: a builder stage installs the R library
+ TeXLive; a slim runtime stage copies them in and adds the RAAB7 code + `http-server.js`. Ordering +
buildx **layer caching** keep code rebuilds fast, so there's no separate base image:

- **Code change** (merge to `main`) → only the final `COPY` layers rebuild (deps restored from cache).
- **Dependency change** (`renv.lock` / `sysreqs.txt`) → the deps layer is invalidated and reinstalled
  automatically. No version-tag bookkeeping.

## Who does what

Everything — code, dependencies, **and** image builds — lives in this repo. GitHub Actions publishes the
image to GHCR on every merge to `main` (and on manual dispatch). Peek consumes the published image
**pinned by digest** and builds nothing.

## Local development

**A. Run reports without rebuilding dependencies (common).** A one-line wrapper pulls the image (it
carries R + all packages at `/opt/R-lib`), mounts your working tree, and runs `Rscript` directly
(skipping the HTTP-server entrypoint):
```bash
docker/run-local.sh "<RAAB_ID>"
# pin a specific build: RAAB_IMAGE=ghcr.io/raabteam/raab-analysis@sha256:… docker/run-local.sh "<RAAB_ID>"
```
Edit `.R`/`.Rmd` and re-run instantly. (The library at `/opt/R-lib` is outside `/raab7`, so mounting your
code at `/raab7` can't shadow it; `.here` keeps `here()` deterministic.) No compose needed.

**B. Change dependencies → new image (rare).**
1. `renv::install(<pkg>)` / `renv::snapshot()` (or `docker/gen-deps.sh`) — updates `renv.lock` (+
   `docker/sysreqs.txt`) against current PPM (→ binaries on both arches on the next build). Bumping
   R/snapshot? edit `docker/versions.env`.
2. Commit + PR (the `renv.lock` diff is the review gate).
3. On merge, Actions rebuilds (deps layer invalidated). Validate report output before Peek adopts the
   new digest.

## renv is the source of truth for versions

`renv::restore()` installs exactly what `renv.lock` pins. PPM only decides **binary vs. source** for that
version — it never changes the version. Regenerating the lock at dependency-bump time (step B1) and
building right after means "locked == current" → binaries on both amd64 and arm64. Peek pins the image
by `@sha256` digest for production.

## File map

| File | Purpose |
|---|---|
| `docker/Dockerfile` | Single multi-stage image: deps (builder) + code + `http-server.js` (runtime). All dep logic here. |
| `docker/versions.env` | Build inputs: R base digest, PPM snapshot, TeXLive bundle, platforms. |
| `docker/gen-deps.sh` | (Optional) regenerate `renv.lock` + `sysreqs.txt` in a pinned Noble container. |
| `docker/build.sh` | Build/push helper (consolidated from build-base/build-code; called by the workflow / used locally). |
| `docker/run-local.sh` | Local wrapper: `docker run` the image with mounts, run `Rscript` directly. |
| `docker/sysreqs.txt` | **Generated** apt list (`pak::pkg_sysreqs`). Do not hand-edit. |
| `http-server.js` | The invocation server (default CMD; see the CI plan §7). |
| `.github/workflows/image.yml` | The GHCR publish pipeline (see the CI plan §8). |
