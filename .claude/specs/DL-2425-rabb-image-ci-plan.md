# RAAB analysis image — design, CI, and local dev (raabteam-owned)

**Status:** Implemented (2026-07) — this repo builds & publishes the image via GitHub Actions. A record
of what was built, how, and why; kept for future changes, not a live proposal.

**Repo:** `raab7-analysis` (this repo).

**Companion:** the Peek-side record (`.claude/specs/DL-2425-raab-integration-uplift.md`, peek repo)
consumes what this repo produces — a public GHCR image, **pinned by commit-sha tag** — and dropped the
submodule.

**Goal:** one reproducible R environment, identical on dev machines (Apple Silicon arm64 + Windows amd64)
and in the cloud (amd64 + arm64 prod), built and published by **this repo** as **one** public image on
**GitHub Container Registry (GHCR)** via GitHub Actions — no AWS, no external credentials, consumers pull
credential-free. Maintainers get a simple local workflow: run reports without rebuilding dependencies,
and a clear path to upgrade dependencies and cut a new image.

---

## 1. Final design

- **One image, one multi-stage Dockerfile** — `ghcr.io/raabteam/raab-analysis` (public GHCR). Builder
  stage installs the R library + TeXLive; slim runtime stage copies them in and adds the RAAB7 code +
  `http-server.js`. **Layer ordering by change-frequency + buildx layer caching** give fast code
  rebuilds without a separate base image.
- **Base:** `rocker/r-ver:4.4.x` → Ubuntu 24.04 "noble", native amd64 + arm64, pinned by digest.
- **Packages:** **renv** lockfile (**source of truth for versions**) + **PPM** for binary installs
  (speed). PPM never overrides a locked version (§3).
- **System deps:** generated with `pak::pkg_sysreqs` (explicit, committed as `docker/sysreqs.txt`).
- **Multi-arch:** **native GitHub runners per arch, in parallel**, stitched into one manifest (§8).
- **Reproducibility anchor:** the immutable image pinned by **digest** in GHCR. `renv.lock` is the
  authoritative version list + change-control ledger.
- **Build/run:** Actions builds on merge to `main`; devs **pull the image** and mount their working tree
  for local iteration (no dependency rebuild — §9).

*Why not two images (the earlier plan):* the split existed to (a) avoid rebuilding heavy deps on every
code change and (b) survive amd64-only Bitbucket + QEMU. Native arm runners kill (b); buildx cache solves
(a) inside one Dockerfile. No `BASE_VERSION`, no base-digest pinning, no cross-image coordination.

---

## 2. Why we're doing this

Two Dockerfiles silently diverged — the root cause of "works locally, breaks in prod":

| Axis | old dev `Dockerfile` | old prod `Dockerfile.r-raab` |
|---|---|---|
| Base image | `rocker/r-base:4.3.1` (Debian, **rolling**) | `rocker/r-ver:$R_VERSION` (Ubuntu, pinned) |
| R version | Hardcoded 4.3.1 (`ARG R_VERSION` **ignored**) | Parametrised |
| CRAN source | live CRAN (**floating**) | PPM snapshot `2024-12-01` (pinned) |
| tinytex / TeXLive | 0.43 / TeXLive 2022 | 0.49 / TeXLive 2024 |
| apt deps | no `cmake`, `libglpk-dev`, `libgmp-dev` | has them |

The hand-maintained `apt-get` list also drifted from the real package set — a missing lib surfaced only
in prod. Fix: stop hand-maintaining it (generate from the locked set), and keep **one** environment
definition so nothing can diverge.

---

## 3. renv ↔ PPM (versions truth)

`renv::restore()` installs **exactly** the version pinned in `renv.lock`; PPM cannot override it. PPM only
decides whether that version arrives as a fast **binary** or is compiled from **source** (PPM mirrors all
historical CRAN sources, so any locked version is always installable):

- **amd64:** PPM keeps **dated snapshots** → a lock aligned to a snapshot → exact-version **binaries**.
- **arm64:** PPM serves **latest binaries only**. Locked==latest → binary; an *older* locked version
  compiles **from source on arm64** (correct version, slower).

**Guarantee:** regenerate the lock and build in the same pass → "locked==latest" on both arches →
binaries everywhere. Rebuilding an *old* lock on arm64 is the only source-fallback case, and it's rare
because the anchor is the image digest, not re-restoring. On code-only changes the deps layer is cached,
so PPM isn't even contacted.

---

## 4. Image architecture: one multi-stage Dockerfile + layer caching

```dockerfile
# ---- builder: restore the R library (heavy, rarely changes); whole stage discarded ----
FROM ${R_BASE} AS builder
COPY docker/sysreqs.txt /tmp/    # apt install toolchain + -dev set; busts only when sysreqs change
COPY renv.lock /build/           # deps layer key
#   repos → PPM (noble snapshot); renv::restore(library='/opt/R-lib') → PPM binaries

# ---- app (runtime): slim; runtime libs + TeXLive(apt) + Node, then code ----
FROM ${R_BASE} AS app
ENV R_LIBS_SITE=/opt/R-lib
#   apt install: runtime shared libs (Noble t64 names) + nodejs + TeXLive from UBUNTU
#   noble packages (texlive-latex-*/xetex/lmodern/latexmk) — NOT tinytex/CTAN, so no
#   flaky external TeX mirror at build. tinytex (the R pkg) drives system TeXLive.
COPY --from=builder /opt/R-lib /opt/R-lib
#   Build-time guard: load every top-level R pkg AND render docker/latex-smoke.Rmd through
#   the real rmarkdown→pandoc→xelatex path (must emit a PDF) → a missing runtime lib or
#   LaTeX package fails the BUILD, never prod (which has no egress). Rprofile.site bakes
#   tinytex.install_packages=FALSE so the render session never tries a network TeX install.
WORKDIR /raab7
COPY RAAB7_scripts/ ./RAAB7_scripts/                      # code LAST → hits the deps cache
COPY rmd_wrapper_PEEK_server.R renv.lock .here docker/http-server.js ./
EXPOSE 8000
HEALTHCHECK CMD node -e "…/health → exit 0 if 200…"
CMD ["node", "/raab7/http-server.js"]      # default = HTTP server; local/K8s override w/ Rscript
```
> TeXLive comes from Ubuntu noble apt packages (TeXLive 2023), **installed in the runtime stage** (not
> copied from the builder, not tinytex/CTAN) — chosen to remove a flaky external TeX-mirror dependency at
> build time. The `docker/latex-smoke.Rmd` render guard proves the full LaTeX toolchain produces a PDF.

- **Cache (CI):** buildx `cache-from`/`cache-to` (`type=gha`). A **code-only change rebuilds only the
  final `COPY` layers**; a `renv.lock`/`sysreqs.txt` change invalidates the deps layer automatically.
- **Flat library + `R_LIBS_SITE`, not renv runtime activation:** builder runs
  `renv::restore(library='/opt/R-lib')`; both stages set `R_LIBS_SITE=/opt/R-lib`. Sidesteps renv's
  per-arch path nesting. Runtime carries `renv.lock` for traceability, not `.Rprofile`/`renv/activate.R`.

### Package surface (seeds the lock)
Loaded by `RAAB7_reporter.Rmd` + wrapper: `rmarkdown, readxl, knitr, tinytex, kableExtra, RColorBrewer,
RCurl, jsonlite, tidyverse, stringr, treemap, maditr, here`; fonts/graphics: `extrafont, float,
systemfonts, svglite`. **Scope: RAAB7 only** (RAAB5/6 not copied; bind-mount if needed).

### Files
```
renv.lock  .here  renv/activate.R .Rprofile renv/.gitignore   # renv (activate.R/.Rprofile used at gen time)
docker/Dockerfile         # single multi-stage: deps (builder) + code (runtime). ALL dep logic here.
docker/versions.env       # R base digest, PPM snapshot, IMAGE, IMAGE_VERSION, platforms
docker/gen-deps.sh        # (optional) regenerate renv.lock + sysreqs.txt in a Noble container — §10
docker/build.sh           # build/push helper (consolidates old build-base.sh + build-code.sh)
docker/sysreqs.txt        # GENERATED apt list (pak::pkg_sysreqs)
docker/run-local.sh       # local invocation wrapper (§9)
docker/http-server.js     # invocation server (§7); COPYed to /raab7/http-server.js
docker/latex-smoke.Rmd    # build-time LaTeX compile guard (§4 / §6)
.dockerignore             # exclude .git, RAAB5/6, data, outputs (lean context + image)
```

---

## 5. Hardening & slimming (simple wins)

- **Multi-stage (biggest win):** the compiler toolchain + `-dev` headers stay in the builder; the runtime
  gets runtime shared libs only. Already the plan.
- **Non-root:** create an unprivileged `raab` user and run as it (`USER raab`). Note: the mounted
  `outputs/` must be writable by that uid — simplest is to have the caller pass `--user`/own the dir, or
  keep outputs group-writable. Optional but a cheap hardening step.
- **TeXLive:** the minimal Ubuntu `texlive-*` set the report needs (latex-base/-recommended/-extra,
  fonts-recommended, xetex, plain-generic, lmodern, latexmk) — no docs, no full scheme. (Implemented as
  apt packages rather than tinytex; see §4.)
- **Non-root:** intended, but **deferred** — the runtime-mounted `outputs/` must be writable by the
  container uid, which depends on the Peek/K8s caller. The Dockerfile carries a `TODO(non-root)` with the
  rationale; it stays root until the mount ownership is confirmed.
- **Housekeeping:** `--no-install-recommends`, `rm -rf /var/lib/apt/lists/*` and `/tmp/*` in the same
  layer, `.dockerignore` to keep `.git`/RAAB5-6/data/outputs out of the build context.
- **Pin the base by digest** (`R_BASE`, done). SSH server + `sudo` removed (no longer needed).
- **Follow-up (higher audit risk):** replace the `tidyverse` meta-package with the members actually used;
  re-snapshot + regenerate sysreqs after. Measure with `dive` first.

---

## 6. What's been built & verified (carries over to the single image)

The two-Dockerfile prototype on `DL-2425-try-renv` proved the mechanics the single image reuses (builder
= old base; runtime+code = old code image, now one file):

- **amd64 ✅:** renv pulled **binaries** from PPM (124 pkgs, **0 source**, ~4 min); load-test guard green;
  ~590 MB compressed. Guard caught a missing dep — `tlmgr` needs full **`perl`** (`File::Find`), not just
  `perl-base` — now in the runtime stage.
- **arm64 ✅** (native, Apple Silicon): **arm64 Noble binaries** (124 pkgs, **0 source**, 26 s) → native/
  QEMU builds are cheap binary-unpacks. Size-equivalent (~581 MB compressed).
- **TeXLive (final choice):** dropped the tinytex/CTAN bundle in favour of Ubuntu noble apt `texlive-*`
  (TeXLive 2023, frozen with the distro) — removes the flaky external TeX-mirror dependency at build.
  `Rprofile.site` bakes `tinytex.install_packages=FALSE` so the render session never reaches for CTAN.
- **Code layering ✅:** `WORKDIR`+`.here` → `here()` = `/raab7`; `.libPaths()` includes `/opt/R-lib`; all
  packages load (tidyverse 2.0.0, ggplot2 4.0.3, kableExtra…).
- **Retained robustness:** flat `/opt/R-lib`+`R_LIBS_SITE`; **build-time guard** — load every top-level
  pkg AND render `docker/latex-smoke.Rmd` through the real rmarkdown→pandoc→xelatex path (must emit a
  PDF; catches missing `.sty` like `multirow`/`lmodern`) → fails the build, not prod; runtime lib names
  from apt (Noble t64: `libcurl4t64`, `libssl3t64`, `libpng16-16t64`, `libuv1t64`); base pinned by digest.

**Consolidation task:** merge `Dockerfile.base` + `Dockerfile` → one multi-stage `docker/Dockerfile`;
merge `build-base.sh` + `build-code.sh` → `build.sh`; add `nodejs` + `http-server.js` to the runtime
stage; drop the SSH `prod` target; wire buildx layer caching.

---

## 7. The contract with Peek (interface — keep stable)

- **Default `CMD` = an HTTP server on `:8000`:**
  - `POST /run` `{ "raabId": "<id>" }` → runs `Rscript rmd_wrapper_PEEK_server.R <raabId>` in `/raab7`,
    **streams** combined stdout+stderr as a chunked `text/plain` response, ends with a final line
    `__EXIT__ <code>` (the exit status).
  - `GET /health` → `200 ok`.
  - **No auth token** — the server is only reachable on the internal overlay/pod network and must never
    be published externally (deploy concern).
- **Mounts:** `/raab7/data` (ro input), `/raab7/outputs` (rw output). `.here` pins root; workdir `/raab7`.
- **Env expected:** `USERNAME` / `PASSWORD` / `BASIC_AUTH` (raab.world creds, inherited by the spawned
  Rscript).
- **Dual-mode:** default CMD = HTTP server; local runs (§9) and a future K8s Job override the command to
  run `Rscript` directly.

### `http-server.js` (zero-dependency, Node core only)
```js
const http = require('http');
const { spawn } = require('child_process');

http.createServer((req, res) => {
  if (req.method === 'GET' && req.url === '/health') return res.end('ok');
  if (req.method !== 'POST' || req.url !== '/run') { res.writeHead(404); return res.end(); }

  let buf = '';
  req.on('data', (c) => (buf += c));
  req.on('end', () => {
    let raabId;
    try { ({ raabId } = JSON.parse(buf || '{}')); } catch { res.writeHead(400); return res.end(); }
    if (!raabId) { res.writeHead(400); return res.end('missing raabId'); }
    // USERNAME/PASSWORD/BASIC_AUTH are inherited from the process env.
    const child = spawn('Rscript', ['rmd_wrapper_PEEK_server.R', String(raabId)], { cwd: '/raab7' });
    res.writeHead(200, { 'Content-Type': 'text/plain', 'Transfer-Encoding': 'chunked' });
    child.stdout.pipe(res, { end: false });
    child.stderr.pipe(res, { end: false });                       // combined, ordered-ish
    child.on('close', (code) => res.end(`\n__EXIT__ ${code}\n`)); // sentinel carries exit status
    child.on('error', (e) => res.end(`\n__EXIT__ 1 ${e.message}\n`));
  });
}).listen(8000, '0.0.0.0');
```

---

## 8. GitHub Actions — parallel native build → one multi-arch manifest

`GITHUB_TOKEN` (`packages: write`) pushes to this repo's GHCR namespace — no external secrets. The two
arches build **in parallel on native runners**, each pushes **by digest**, then a dependent job stitches
**one multi-arch tag**. Per-arch buildx layer caching keeps code-only builds fast.

### `.github/workflows/image.yml`
```yaml
name: image
on:
  push: { branches: [main] }        # merge to main → new image
  workflow_dispatch: {}             # manual (first build, or force a dep rebuild)
permissions: { contents: read, packages: write }
env:
  IMAGE: ghcr.io/raabteam/raab-analysis
jobs:
  build:
    strategy:
      matrix:
        include:
          - { arch: amd64, runner: ubuntu-latest }
          - { arch: arm64, runner: ubuntu-24.04-arm }   # native arm64, free for public repos
    runs-on: ${{ matrix.runner }}
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - uses: docker/login-action@v3
        with: { registry: ghcr.io, username: ${{ github.actor }}, password: ${{ secrets.GITHUB_TOKEN }} }
      - id: build
        uses: docker/build-push-action@v6
        with:
          context: .
          file: docker/Dockerfile
          target: app
          platforms: linux/${{ matrix.arch }}
          cache-from: type=gha,scope=${{ matrix.arch }}
          cache-to: type=gha,mode=max,scope=${{ matrix.arch }}
          outputs: type=image,name=${{ env.IMAGE }},push-by-digest=true,name-canonical=true,push=true
      - run: |               # save each arch's digest for the manifest job
          mkdir -p /tmp/digests
          touch "/tmp/digests/${{ matrix.arch }}-${{ steps.build.outputs.digest }}"
      - uses: actions/upload-artifact@v4
        with: { name: digest-${{ matrix.arch }}, path: /tmp/digests/*, retention-days: 1 }
  manifest:
    needs: build
    runs-on: ubuntu-latest
    steps:
      - uses: docker/login-action@v3
        with: { registry: ghcr.io, username: ${{ github.actor }}, password: ${{ secrets.GITHUB_TOKEN }} }
      - uses: actions/download-artifact@v4
        with: { path: /tmp/digests, pattern: digest-* , merge-multiple: true }
      - id: tags
        run: echo "sha=${GITHUB_SHA::12}" >> "$GITHUB_OUTPUT"
      - run: |               # stitch the per-arch digests into ONE multi-arch tag
          DIGESTS=$(for f in /tmp/digests/*; do d="${f##*-}"; echo "${IMAGE}@${d%%.*}"; done)
          docker buildx imagetools create \
            -t ${IMAGE}:latest -t ${IMAGE}:${{ steps.tags.outputs.sha }} $DIGESTS
          docker buildx imagetools inspect ${IMAGE}:${{ steps.tags.outputs.sha }} >> "$GITHUB_STEP_SUMMARY"
```
> The final multi-arch **tag digest** (from `imagetools inspect`) is what Peek pins. The digest-plumbing
> above is the standard build-push-action multi-arch-via-matrix recipe; treat it as a sketch to verify.
> **Simpler fallback:** a single job with `platforms: linux/amd64,linux/arm64` + QEMU — one tag, auto
> manifest, but sequential/emulated (cheap here since installs are binaries). Use if the matrix is fiddly.

---

## 9. Local development

**A. Run reports without rebuilding dependencies (common).** A wrapper `docker run`s the pulled image with
local mounts and **overrides the default CMD to run `Rscript` directly** (skips the HTTP server). Mounting
the working tree over `/raab7` means code edits take effect with no rebuild (the library at `/opt/R-lib`
is outside `/raab7`, so the mount can't shadow it; `.here` keeps `here()` deterministic).

`docker/run-local.sh`:
```bash
#!/usr/bin/env bash
set -euo pipefail
RAAB_ID="${1:?usage: run-local.sh <RAAB_ID>}"
IMAGE="${RAAB_IMAGE:-ghcr.io/raabteam/raab-analysis:latest}"
REPO="$(cd "$(dirname "$0")/.." && pwd)"
docker run --rm \
  -v "$REPO":/raab7 \
  "$IMAGE" \
  Rscript rmd_wrapper_PEEK_server.R "$RAAB_ID"
```
Usage: `docker/run-local.sh <RAAB_ID>` (set `RAAB_IMAGE=…@sha256:…` to pin a specific build). No compose
needed. To test the server locally instead: `docker run --rm -p 8000:8000 -v "$PWD/data:/raab7/data:ro"
-v "$PWD/outputs:/raab7/outputs" <image>` then `curl -XPOST localhost:8000/run -d '{"raabId":"…"}'`.

**B. Change dependencies → new image (rare).**
1. Update the lock — `renv::install(<pkg>)` / `renv::snapshot()` (or `docker/gen-deps.sh`, §10) against
   current PPM → binaries on both arches on the next build. Bumping R/snapshot? edit `docker/versions.env`.
2. Commit + PR (the `renv.lock` diff is the review gate).
3. On merge, Actions rebuilds (deps layer invalidated). Validate report output (§11) before Peek adopts
   the new digest.

---

## 10. Is `gen-deps.sh` needed?

**Not strictly.** `renv.lock` is maintained by `renv::snapshot()`; the apt list is
`pak::pkg_sysreqs(<pkgs>, "ubuntu-24.04")`. `gen-deps.sh` just runs both in a **pinned Noble container**
so the lock + `sysreqs.txt` stay in sync and match the target platform in one reproducible step. Keep it
(it exists and works) — but a maintainer can equally regenerate by hand. *Optional further simplification:*
compute `sysreqs` at build time in the Dockerfile and stop committing `sysreqs.txt` — removes a file and a
step, but loses the reviewable diff; not recommended unless the sync becomes a burden.

---

## 11. Validation harness (gates the R-version bump and every future dep bump)

`RAAB7_reporter.Rmd` writes ~30 intermediate CSVs of computed results — diff those as data (precise,
automatable) instead of diffing PDFs. Run the same input corpus through the current-prod image and the
new image:
- **L0 Smoke** — runs, exits 0, emits a PDF.
- **L1 Golden CSV diff** *(key)* — every `summary/data/*.csv` + `raw/*.csv` with a **float tolerance**
  (`all.equal`). Any diff is a flag.
- **L2 PDF/visual diff** — `pdftotext`+diff (content/order); rasterize+pixel-diff (layout, fonts, treemap
  colours, kableExtra/ggplot styling).
- **L3 Corpus** — different countries, with/without DR module, WGQ variants, **zero-denominator** cases.
- **L4 Warnings** — capture + diff the render's warning stream.

Triage by the `renv.lock` diff — scrutinise major jumps: `ggplot2` 3→4 (figures), `kableExtra` 1.3→1.4
(LaTeX tables), `tidyverse`→2.0 (computed values), `jsonlite`→2.0 (wrapper). TeXLive is Ubuntu noble
(TeXLive 2023, apt) → L2 covers LaTeX shifts. Scrub non-determinism (dates, temp paths, `<ID>`); decide an ordering
policy. **Prereq:** a representative test corpus (anonymised/synthetic); capture golden outputs from the
current prod image now.

---

## 12. Migration steps & decisions

**Steps (this repo):**
1. Consolidate the two Dockerfiles → one multi-stage `docker/Dockerfile`; merge build scripts → `build.sh`.
2. Add `nodejs` + `http-server.js` to the runtime stage; drop `entrypoint.sh`, `analysis-wrapper.sh`, SSH.
3. Add `docker/run-local.sh`; remove `docker-compose.yml` (wrapper replaces it).
4. Add `.github/workflows/image.yml` (§8) with per-arch caching + manifest stitch.
5. Update `docker/README.md`.
6. First run: `workflow_dispatch` to publish; thereafter on merge to `main`.
7. Hand the published multi-arch tag (the commit short-sha) to Peek to pin. Peek removed the submodule
   after that.

**Decided:** one public GHCR image `raab-analysis`; single multi-stage Dockerfile + buildx cache; renv
(version truth) + PPM (binary speed); TeXLive from Ubuntu noble apt (not tinytex/CTAN) + a `latex-smoke.Rmd`
build guard; native parallel multi-arch → one manifest; `http-server.js` (no auth token, internal-network
only); local `run-local.sh` (no compose). Each image is content-addressed by digest, but **Peek pins the
commit-sha tag** (immutable by convention; GHCR has no enforced tag immutability).

**Open / verify:** GHCR package visibility must be **public** (flip after first push); confirm layer-cache
hit on code-only changes; confirm native-arm runner build time; report-output parity across the R bump.
