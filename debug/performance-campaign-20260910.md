# Linux package-set performance campaign

Campaign branch: `perf/linux-package-set-20260910` in `vtrl/purescript`.
Compiler baseline: `9160ce1518b5f11f9ebe32b445019f8dbb4f435a`, verified
against the fork's `master` and default branch on 2026-09-10. Do not push to
upstream or merge this campaign to master without separate authorization.

## Reproduction

The corpus adapts `ci/build-package-set.sh`: use CI's Spago 0.93.43 to fetch
every package in a package set. Unlike CI, pin set **60.4.0** (2024-10-08,
PureScript 0.15.15) and checksum its JSON from registry commit
`5d834cd364da1d49a1bd1b0219ab49fb15f20601`. It contains 506 packages,
4,084 PureScript sources and 4,901 total source/FFI inputs. The prepared
`inputs.json` SHA256 is
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

Run on an otherwise idle Linux benchmark machine:

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal --jobs=6 build purescript:exe:purs --lock-file=error-on-write
mkdir -p .build/perf
cp "$(stack path --local-install-root)/bin/purs" .build/perf/purs-baseline
npm install --prefix .build/perf/tools --no-audit --no-fund spago@0.93.43
export PATH="$PWD/.build/perf/tools/node_modules/.bin:$PWD/.build/perf:$PATH"
# Put an actual purs executable on PATH for Spago's version check.
export PATH="$(stack path --local-install-root)/bin:$PATH"
bash ci/prepare-package-set-benchmark.sh .build/perf/corpus
python3 ci/benchmark-compiler.py \
  --corpus .build/perf/corpus --compiler .build/perf/purs-baseline \
  --results .build/perf/baseline-n1-warm --label 9160ce1-O2 \
  --capabilities 1 --samples 5
```

Build dependencies and fetch sources **before** measurement. Stack 3.3.1,
GHC 9.6.6, resolver lts-22.43; repository-local Haskell code uses `-O2`
from `stack.yaml`, dependencies use Stack/Cabal's normal optimized settings.
Never use `--fast` or a profiling executable for final comparisons. Preserve
each compiler binary separately; the harness records its SHA256 and version.
A `DIRTY` version suffix alone is not a different compiler algorithm: record
whether there were compiler-source changes when building.

The harness directly invokes `purs compile` with the sorted source manifest,
`--codegen js`, and `+RTS -N1 -s -RTS` (or explicit `-N4`). No Spago, npm,
Stack, downloads, or shell glob expansion occur inside the measured process.
Compiler output is deleted before **every** run, including warm-up. Thus
"warm" means warm OS page cache, never an incremental/no-op build. A full
clean warm-up is excluded from warm samples. `--cache cold` instead runs
`sync; echo 3 > /proc/sys/vm/drop_caches` via noninteractive sudo before
every sample, failing if not permitted. Use this only on an otherwise idle,
disposable machine. It does not claim to reset remote/storage-server caches.

GNU time records wall, user, system seconds and peak RSS (KiB). RTS `-s`
records allocation and maximum sampled residency (not the same as peak RSS).
The harness saves raw stdout/stderr, command, CPU/memory metadata, individual
samples, mean, median, sample standard deviation, and range. It validates all
input hashes and refuses simultaneous runs on the same corpus. Final-output
hashes cover generated JS, FFI, externs, and requested CoreFn, not build-cache
timestamps. Compare candidate/baseline samples on the **same machine**, with
identical capabilities/cache/codegen flags; alternate repeated blocks to
check drift. Worker-machine timings are only provisional until reproduced
by the lead.

## Machine and ownership

Lead orb: 8 vCPUs (4 cores × 2 SMT), Intel Xeon @ 2.60 GHz, family 6/model
106; about 16 GiB physical RAM, 14 GiB workload cgroup limit, no swap;
Linux 6.1.158+, x86-64, KVM. No explicit CPU quota. Record machine metadata
with each run rather than assuming every large orb is identical.

Concurrency ceiling: eight Ultra orbs including the lead; workers are
`a1.large` or larger. Four orbs currently:

- Lead owns the benchmark harness, non-profiled baseline, integration, and
  combined correctness.
- `perf/profile-baseline-20260910`: baseline GHC CPU/allocation/heap profiles.
- `perf/baseline-correctness-20260910`: optimized baseline full test suite.
- `perf/nursery-trial-20260910`: default versus larger allocation areas,
  justified by 45% baseline GC time. Owns RTS/defaults experiments only.

All compiler source is still at the original baseline. Shared harness/docs
checkpoint is `c2834c2f`; algorithm ownership will follow measured hotspots.

## Results and trials

### Optimized baseline, warm page cache, clean JS build, N1

Five measured runs, after one excluded full warm-up:

| Sample | Wall (s) | Peak RSS (KiB) | Allocated (bytes) |
| --- | ---: | ---: | ---: |
| 1 | 239.23 | 3,266,840 | 397,431,633,280 |
| 2 | 242.18 | 3,240,368 | 397,436,996,408 |
| 3 | 242.69 | 3,415,480 | 397,423,962,632 |
| 4 | 237.20 | 3,257,464 | 397,431,441,456 |
| 5 | 242.39 | 3,292,464 | 397,430,732,496 |

Wall mean **240.738 s**, median 242.18 s, sample SD 2.419 s, CV 1.005%.
Mean RSS 3,294,523 KiB (3.142 GiB), SD 70,191 KiB. Mean allocation
397,430,953,254 bytes. All 4,084 modules compiled successfully every run.
The excluded warm-up took 236.71 s, with 105.58 s GC (45% elapsed), 94,349
minor and 37 major collections. This suggests investigating allocation
provenance and nursery pressure; it is not itself an optimization result.

Preserved compiler SHA256:
`d105283e23997dc02c531fcfce7036554c6d68599ee528536bfafd35375e3d00`.
Its version reports original `9160ce1` plus `DIRTY`: only untracked benchmark
harness/docs existed when it was built; `src`, `app`, and `lib` were unchanged.
Raw logs, metadata, product hashes and samples are retained at
`.build/perf/baseline-n1-warm`; review bundle:
`.amp/in/artifacts/performance-20260910/baseline-n1-warm.tar.gz`.

N4 warm and N1 cold baselines are queued serially. Profile-backed trials are
in progress; no compiler optimization has been accepted yet. Instrumented
profiling results will not be compared directly to these optimized timings.
