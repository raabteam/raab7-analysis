# RAAB analysis Docker image

This repo builds **one** public image on **GitHub Container Registry (GHCR)** and publishes it from its
own GitHub Actions — no AWS, no external registry credentials. Consumers (the Peek servers, other
maintainers) pull it without any credentials.

> The full design + setup (image, workflow, `http-server.js`, validation) is in
> `.claude/specs/DL-2425-rabb-image-ci-plan.md`. This README is the day-to-day operational guide.

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

### Run reports without rebuilding dependencies (common)

A one-line wrapper pulls the image (it carries R + all packages at `/opt/R-lib`), mounts your working tree, and 
runs `Rscript` directly (skipping the HTTP-server entrypoint):
```bash
docker/run-local.sh "<RAAB_ID>"
```
Edit `.R`/`.Rmd` and re-run instantly. (The library at `/opt/R-lib` is outside `/raab7`, so mounting your
code at `/raab7` can't shadow it; `.here` keeps `here()` deterministic.) No compose needed.

### Update dependencies and generate a new image (infrequent)

There are two kinds of dependency and they are changed differently.

#### Changes to R package dependencies (add/remove a package, or bump a version). 

The set of R packages lives in ONE place: the
`pkgs <- c(...)` list near the top of `docker/generate-deps.R`. 
A helper script materialises that list into two committed files:
  - `renv.lock` — the exact version of every package *and* its dependencies (the lockfile), and
  - `docker/sysreqs.txt` — the system libraries those packages need to build.

Steps:
1. **Edit the `pkgs` list** in `docker/generate-deps.R` (add/remove the package name). To bump the R
   version or the PPM snapshot instead, edit `docker/versions.env`.
2. **Run `docker/gen-deps.sh`.** It starts the pinned Noble container (identical to the build), installs
   the set from the pinned PPM snapshot, and **regenerates `renv.lock` + `docker/sysreqs.txt`**. You need
   Docker running; you do **not** need R installed on your machine.
3. **Review the `renv.lock` diff** (this is the gate — it shows exactly which versions changed), then
   commit `generate-deps.R` + `renv.lock` + `sysreqs.txt` and open a PR.
4. On merge, GitHub Actions rebuilds the image (the deps layer is invalidated and reinstalled from the
   lock). **Validate the report output** before Peek pins the new digest.

#### A system or LaTeX package (a shared library, a font, or a `texlive-*` package

These are **not** managed by renv. Edit the `apt-get install` list in the **app stage of
`docker/Dockerfile`** directly, then rebuild. The build-time guard renders a test report, so a missing
LaTeX package fails the build instead of reaching prod. (If a newly added R package needs a system
library *at runtime*, the guard's package-load check fails with a clear "cannot load X" — add that
library to the same app-stage apt list.)

> **Why not `renv::install()` locally?** This project has no live renv project on your host — the package
> set is the `pkgs` list, and `gen-deps.sh` rebuilds the lock from it inside a container that matches the
> build. Installing on your host (macOS/Windows) would update `renv.lock` but not `sysreqs.txt`, would
> drift from the `pkgs` list, and would be **overwritten** the next time anyone runs `gen-deps.sh`.
> Editing the list + regenerating in the container is what keeps the result reproducible and identical
> to the built image.

## renv is the source of truth for versions

`renv::restore()` installs exactly what `renv.lock` pins. PPM only decides **binary vs. source** for that
version — it never changes the version. Regenerating the lock at dependency-bump time and
building right after means "locked == current" → binaries on both amd64 and arm64. Peek pins the image
by `@sha256` digest for production.

## File map

| File | Purpose |
|---|---|
| `docker/Dockerfile` | Single multi-stage image: deps (builder) + code + `http-server.js` (runtime). System/LaTeX packages (apt, incl. `texlive-*`) are edited here (§B2). |
| `docker/versions.env` | Build inputs: R base digest, PPM snapshot, platforms. Edit to bump R/snapshot. |
| `docker/generate-deps.R` | The R-package list (`pkgs <- c(...)`) + logic; edit the list to add/remove an R package (§B1). Run in-container by `gen-deps.sh`. |
| `docker/gen-deps.sh` | Regenerate `renv.lock` + `sysreqs.txt` by running `generate-deps.R` in the pinned Noble container. Run after editing the `pkgs` list. |
| `docker/build.sh` | Build/push helper (called by the workflow / used locally). |
| `docker/run-local.sh` | Local wrapper: `docker run` the image with mounts, run `Rscript` directly. |
| `docker/sysreqs.txt` | **Generated** apt list (`pak::pkg_sysreqs`). Do not hand-edit. |
| `http-server.js` | The invocation server (default CMD; see the CI plan §7). |
| `.github/workflows/image.yml` | The GHCR publish pipeline (see the CI plan §8). |
