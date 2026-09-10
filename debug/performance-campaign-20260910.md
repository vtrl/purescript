# Linux package-set performance campaign

Campaign branch: `perf/linux-package-set-20260910` in `vtrl/purescript`.
Original compiler baseline: `9160ce1518b5f11f9ebe32b445019f8dbb4f435a`, verified
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

Run on an otherwise idle Linux benchmark machine. Build the original baseline
in a clean checkout of the exact original commit, not the current campaign
HEAD. Use separate preserved binaries and fresh result paths for candidates:

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

For counterbalanced comparisons, add `--baseline /path/to/purs-baseline`
and supply the candidate with `--compiler`. `--samples 5` then measures five
pairs in B/C, C/B, B/C, C/B, B/C order, after one excluded clean warm-up of
each binary. Each compiler receives a separate summary; paired percentage
changes use the baseline in the same pair, not an earlier campaign mean.
All repetitions of both binaries must produce identical product hashes.

## Machine and ownership

Lead orb: 8 vCPUs (4 cores × 2 SMT), Intel Xeon @ 2.60 GHz, family 6/model
106; about 16 GiB physical RAM, 14 GiB workload cgroup limit, no swap;
Linux 6.1.158+, x86-64, KVM. No explicit CPU quota. Record machine metadata
with each run rather than assuming every large orb is identical.

Concurrency ceiling: eight Ultra orbs including the lead; workers are
`a1.large` or larger. Five orbs currently:

- Lead owns the benchmark harness, non-profiled baseline, integration, and
  combined correctness; `Make.hs` parsing/AST lifetime trial.
- GHC profiling worker now owns `perf/inliner-arity-20260910`:
  `CoreImp/Optimizer/Inliner.hs` and direct AST tests in `TestAst.hs`.
- Baseline correctness/native CPU worker now owns `perf/cst-unpack-20260910`:
  representation of already-strict products in `CST/Types.hs`; diagnostic
  heap profiling of the supplied Make candidate while packing repeats wait.
- Nursery worker completed the negative RTS trial and now independently
  measures supplied Make baseline/candidate binaries at N1.
- `perf/binding-visibility-20260910`: sparse name-visibility promotion in
  `TypeChecker/Monad.hs` and focused state tests in `TestCompiler.hs`.

Shared paired harness checkpoint: `e9261835`. The Make lifetime change below
is the first accepted compiler checkpoint. Other source trials started from
the original compiler; measure additive gains against the accepted checkpoint
before integration rather than assuming independent wins compose.

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

The separate GHC `-O2 -fprof-auto` allocation profile completed with all
8,985 products identical. Instrumentation consumed 65.44% of ticks: its
1,772.55 s duration is not a benchmark. Inclusive JSON-attributed allocation
identified `runFn'.go` (11.281 GB, 264 million entries),
`makeBindingGroupVisible` (7.787 GB), and read-only `varIfUnknown`
(2.531 GB) as bounded hypotheses. These instrumented allocation costs are
not predictions of uninstrumented savings. Full provenance/accounting is
in `ghc-profile-report.md` and the compressed raw profile in review artifacts.

### Rejected nursery settings

Retain existing defaults. N1 A16 measured 251.46 s versus the worker's
same-machine default 248.68 s (+1.12%), despite less copying. N1 A64 took
305.97 s (+23.04%); N4 A16 took 129.33 s versus 107.42 s (+20.40%). These
are single-sample screens, not precise effect estimates. Tiny-compilation
repeats found no stable latency win and substantial RSS increases. No
candidate merited expensive full repetitions. The evidence-only commit is
integrated; see [the nursery report](rts-nursery-trial-20260910.md).

### Accepted Make lifetime change

The Make change delays body parsing until dependencies finish and the
semaphore admits the module, while still reporting syntax errors after a
dependency fails. It also forces name-only order/dependency data, avoiding
references from the final ordering list and lazy graph vertices to parsed
modules. The final candidate passes **1,302 examples, zero failures**.

Five counterbalanced N4 pairs completed on the lead machine, after one
excluded clean warm-up per binary. Every pair reduced both wall time and
peak RSS. The modest wall gain and large memory gain justify retention.

| Metric | Baseline mean ± sample SD | Make mean ± sample SD | Mean paired change ± SD |
| --- | ---: | ---: | ---: |
| Wall seconds | 111.832 ± 3.495 | 107.228 ± 2.145 | −4.084% ± 1.668% |
| Peak RSS, KiB | 3,469,681 ± 34,566 | 2,013,866 ± 55,207 | −41.957% ± 1.560% |
| Maximum residency, bytes | 1,222,174,077 ± 19,006,537 | 692,782,141 ± 20,393,538 | −43.314% ± 1.512% |
| Allocation, GB | 397.516 ± 0.011 | 397.626 ± 0.009 | +0.028% ± 0.004% |

Individual B/C wall pairs: 110.78/105.02, 109.94/107.53, 110.13/105.90,
118.06/110.64, 110.25/107.05 s. Warm-ups: 112.14/107.73 s (excluded).
The improvement is object lifetime, not total allocation reduction. Timing
drift remains visible; do not compare against the older 109.893 s N4 mean.
Paired calculations were independently recomputed by sample number.

All 8,985 generated products match across all twelve runs. All runs also
have the same multiset of 745 warning contents. Warning order already varies
between original baseline runs; ignore ordinal headers, not warning content.
The new asymmetric Make test preserves a body syntax error when its dependency
has a type error, skips their downstream module, and builds an independent one.

Candidate SHA256:
`7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
The supplied source/test patch SHA256 is
`e2d2e899348f67e781353b3aa4209c492715d890404593d963c9b63455fe76db`.
The incremental GHC build retained older `6d636561 DIRTY` version metadata;
binary and source-patch identities, not that version string, identify the
measured candidate. Raw paired evidence and the full test log are in
`make-n4-paired.tar.gz` and `make-full-tests.log` under review artifacts.

A separate diagnostic heap profile supports the mechanism: total live-heap
peak 1,134.21→589.75 MiB, independent CST peak 446.11→80.02 MiB, near-end
CST 198.59→51.20 MiB. CST retention is reduced, not eliminated. These
instrumented profiles are not timing comparisons; remaining retaining roots
are unidentified. See `make-heap-summary.md` and its raw evidence bundle.

N1 repeated validation is still running on a separate machine. Its first two
complete pairs are 250.22/243.18 and 251.19/239.26 s; the first pair lowers
RSS 42.85%. This is supporting direction, not a completed five-pair claim.
Full N8 and tiny boundary checks are also pending; cold and incremental
candidate timings have not yet been measured.

### Other source trials

CST packing screens measured RSS reductions of 11.83% at N4 and 16.11% at
N1, but wall differences of −0.20% and +1.85% respectively are not evidence
of a speedup at one sample each. Its full 1,301-test suite and all products
pass. The source checkpoint is `perf/cst-unpack-20260910`; additive benefit
must be measured after the Make lifetime decision, not inferred by adding
the two standalone memory reductions.

Sparse binding visibility reduced allocation by 7.705 GB (1.94%) in its
N4 screen, with variable timings; N1 screened 253.39→239.67 s and 1.942%
less allocation. Its full suite passes 1,306 examples; repetitions are
underway. It is not yet integrated.

Both inliner changes are rejected. The positive-arity guard alone changed
N4 wall by −0.08% and allocation by −0.02%. Deferred argument-list creation
changed wall by +1.30%, RSS by +2.43%, and allocation by only −0.17%.
These are single-pair screens; neither justifies expensive repetitions.
Its 1,438-test suite and all products pass, but diagnostic auto-SCC allocation
attribution did not translate into material normal-O2 savings. Trial source
and tests remain on the worker branch; only negative evidence is for integration.
