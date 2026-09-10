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
  combined correctness; `Make.hs` parsing/AST lifetime trial.
- `perf/profile-baseline-20260910`: baseline GHC CPU/allocation/heap profiles.
- Baseline correctness/native CPU worker now owns `perf/cst-unpack-20260910`:
  representation of already-strict products in `CST/Types.hs` only.
- `perf/nursery-trial-20260910`: default versus larger allocation areas,
  justified by 45% baseline GC time. Owns RTS/defaults experiments only.

Shared source-equivalent harness checkpoint: `b89f5b49`. Candidate compiler
changes are under development, not yet accepted into the campaign baseline.

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

### Parallel and cold-cache baselines

| RTS/cache | Samples | Wall mean ± sample SD (s) | Mean peak RSS (KiB) | Mean allocation (GB) |
| --- | ---: | ---: | ---: | ---: |
| N1 warm | 5 | 240.738 ± 2.419 | 3,294,523 | 397.431 |
| N4 warm | 3 | 109.893 ± 0.297 | 3,445,921 | 397.520 |
| N1 cold | 3 | 239.697 ± 2.695 | 3,290,953 | 397.433 |

N4 samples: 109.78, 110.23, 109.67 s. Cold samples: 236.62, 240.83,
241.64 s, each after successful guest `drop_caches`. All three product
manifests are identical (8,985 files). The cold run also checked product
identity between repetitions. Cold/warm differences are below the observed
noise; they do not support a filesystem-cache explanation of the cost.
N4 scaling is existing compiler behavior, not a campaign optimization.
Raw review bundles accompany the N1 warm bundle under the artifacts directory.

### Baseline correctness

Optimized full suite passed: **1,301 examples, zero failures or pending**,
all 13 groups, including 16 QuickCheck examples with 100 cases each.

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

No fixture patches or golden acceptance. Hspec elapsed 127.5614 s (not a
compiler benchmark). Report and full log are copied to this lead thread's
`baseline-correctness.md` and `baseline-full-tests.log` review artifacts.

### Profiles and evidence-backed trials

Native optimized `perf record -e cpu-clock:u -F 99` on the exact full corpus:
24,857 leaf samples, zero lost. `evacuate1` 20.85%, `eval_thunk_selector`
15.63%, `scavenge_block1` 4.14%: about 40.62% in these three GC leaves.
17.07% of samples remain unresolved main-image addresses; there are no caller
stacks or allocation attribution. Do not interpret small named compiler
leaves as inclusive costs. Raw `perf.data`, complete leaf report, and method
are preserved in the review artifacts. Hardware PMU events are unavailable;
unprivileged software CPU-clock sampling worked without permission changes.

Optimized, non-profiled `+RTS -N1 -s -hT -i1 -l-au -RTS` produced a live-heap
profile and module markers with identical compiler products. Peak sampled
live heap: 1,134.21 MiB, of which 439.72 MiB is CST constructors. Near the
end, 198.59 MiB of CST remains in 930.80 MiB live heap. The early peak includes
117.34 MiB CST source positions, 78.65 MiB token annotations, 58.99 MiB ranges,
and 58.96 MiB source tokens. This is diagnostic: extra major collections made
the run take 381.6 s; that duration is not a baseline comparison.

This supports two independent source trials:

- **CST packing:** explicit `UNPACK` of already-strict nested products may
  remove headers/pointers, but can lose sharing or add reboxing; measure both.
- **Make parsing admission:** body parsing currently occurs before dependency
  waits and the semaphore, retaining full CSTs for modules that cannot run.
  Trial delaying parsing until admitted, while still checking syntax when a
  dependency failed and never holding the semaphore during dependency waits.
  Separately inspect final result-order bookkeeping retaining parsed modules.

Nursery screening is not yet a win: N1 A16 measured 251.46 s versus the
worker's same-machine default 248.68 s, despite copied bytes falling from
68.713 GB to 48.320 GB. A64 warm-up was 18.2% slower. These single-screen
observations are provisional; the worker is checking N4 before recommending
discard/retention. No compiler optimization has been accepted yet.
