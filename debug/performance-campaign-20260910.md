# Linux package-set performance campaign

Campaign branch: `perf/linux-package-set-20260910` in `vtrl/purescript`.
Original compiler baseline: `9160ce1518b5f11f9ebe32b445019f8dbb4f435a`, verified
against the fork's `master` and default branch on 2026-09-10. Do not push to
upstream or merge this campaign to master without separate authorization.

The only accepted compiler optimization is [Make lifetime release
38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
Later campaign commits preserve evidence, not the rejected binding-visibility,
CST packing, inliner, or traversal-INLINABLE source changes. Parser-warning
forcing and specialization flags passed their isolated confirmation blocks and
independent audit. Their experimental cumulative build passed correctness;
paired cumulative performance validation is next, not accepted source.
Equal-leaf guards remain HOLD after final validation; `varIfUnknown` is rejected
for integration after its final N1 block. External-fork trials
are described in [the provenance report](performance-fork-provenance-20260910.md).

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

The user raised the concurrency ceiling to **16 total orbs including the
lead**, then specified **xxlarge only**. New workers use `a1.xxlarge`, not
`a1.3xlarge`. Existing smaller-orb runs remain uninterrupted. At this checkpoint
12 unarchived campaign orbs are allocated: the lead, four original workers,
six validation workers, and one new cumulative build worker; completed workers
remain idle unless assigned a separately owned next task.

- Lead owns the harness, accepted branch, evidence imports, and eventual
  synchronized cumulative candidate and benchmark replication.
- Original specialization worker owns the isolated flags source/build/full
  tests and fresh five-pair N4 confirmation on its existing machine.
- Original boundary, N1, and heap workers completed separate warning-force
  N8/tiny, N1/dense, and diagnostic heap assignments; their evidence is retained.
- New binary-only workers own disjoint leaf N4-five/dense-ten, varIfUnknown
  N1-five, warning-force N4-five, specialization N1-three, and specialization
  N8-three/dense-ten/tiny-ten-per-capability blocks.
- The independent correctness worker completed the warning-force and isolated
  specialization audits. The latter was offline, without new timing.
- The cumulative builder completed the exact source union, optimized build,
  full tests, and preservation of the final tested binary. Its branch remains
  `perf/cumulative-warning-specialization-20260910`, separate from acceptance.
- Three existing xxlarge binary workers are reused for cumulative full N1-three,
  full N4-five, and full N8-three plus dense-ten/tiny-ten-per-capability blocks.
  No additional orb or source variant is needed.

New machines expose 16 logical Xeon CPUs and a 30 GiB workload limit. Fixed
N1/N4/N8 settings, exact baseline/candidate binary hashes, original corpus,
excluded warmups, and within-machine counterbalancing remain unchanged.
Do not pool measurements across different machines or treat capacity as
authorization for redundant variants. These are disjoint trial/report branches;
worker source and tests are not implicitly accepted into the campaign.

The lead's initial warning-force five-pair N4 block was interrupted after
three complete pairs; its partial fourth run and unavailable exit status are
preserved, not relabeled as a completed confirmation. A fresh xxlarge block
replaces it as a separate experiment. Long blocks use managed services with
an atomic run-once guard, recorded exit status, and an idle hold after completion
until collection and service stop. This prevents automatic restarts from
silently adding samples. The exact tested cumulative binary and final standalone
audit now pass review. Only the prescribed fresh matrix below is assigned;
there are no provisional or repeated selection screens.

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

Independent N1 and N8 verification is complete, using the same preserved
binary hashes and paired harness. These are within-machine paired comparisons,
not comparisons of timing means between orbs. All 12 N1 and eight N8 compiles,
including excluded warmups, match the original 8,985 products and complete
745-warning content multisets.

| RTS | Pairs | Baseline wall mean ± SD (s) | Make wall mean ± SD (s) | Paired wall change ± SD (pp) | Paired RSS change ± SD (pp) |
| --- | ---: | ---: | ---: | ---: | ---: |
| N1 | 5 | 249.978 ± 1.050 | 242.726 ± 5.530 | −2.893% ± 2.594 | −45.495% ± 1.562 |
| N8 | 3 | 78.690 ± 0.375 | 73.813 ± 1.227 | −6.199% ± 1.352 | −40.513% ± 2.276 |

SD is sample standard deviation, not a confidence interval. N1 wall pairs are
250.22/243.18, 251.19/239.26, 250.04/239.17, 250.15/239.85,
248.29/252.17 s. The final candidate is slower; it was retained, not rerun or
discarded. Its cause is unestablished. N1 supports consistent peak-memory
reduction, not a uniform-speedup claim. N8 wall pairs are 78.26/72.87,
78.95/73.37, 78.86/75.20 s; all three lower wall/RSS/residency. Allocation
slightly increases at both settings (+0.02885% N1, +0.02499% N8).

Tiny Sequence verification also completed: 57 modules/78 inputs, ten pairs
each at N1/N4/N8, plus excluded warmups. All 66 compiles match 135 original
products and three warning contents. RSS is lower in every pair, with mean
paired reductions 15.501%, 15.906%, and 9.621%. Mean paired wall changes are
−0.047% ± 10.462 pp, −6.448% ± 5.539 pp, and −6.940% ± 8.669 pp respectively.
Short-run variance is large; N1 has no established timing change. No outliers
were dropped. Cold and incremental candidate timings remain unmeasured.

Review artifacts contain `make-n1-paired.tar.gz`, `make-n8-evidence.tar.gz`,
`make-n8-summary.md`, `make-tiny-evidence.tar.gz`, and `make-tiny-summary.md`.

### Other source trials

CST packing is rejected after additive measurement. Standalone screens lowered
RSS 11.83% at N4 and 16.11% at N1, but the accepted Make change removes most
of that opportunity. Make versus Make + four UNPACK pragmas at N4 screened
113.90→115.54 s (+1.440%), RSS −2.944%, allocation −0.0731%. The full additive
suite passes 1,302 examples and all products/warning contents match. This small
memory signal did not justify repeats; one pair does not establish slowdown.
Only [the negative report](performance-cst-unpack-additive-20260910.md) is retained.

Both binding-visibility variants are rejected despite package-set gains. The
original sparse fold's five N4 pairs reduced standalone allocation 1.943%;
lead additive N4 five-pair wall was −5.727% ± 1.780 pp, allocation −1.940%,
with 1,307 full tests passing. But a valid compile-only cycle of 1,000 typed
functions exposed a dense-group regression: five N1 pairs gave wall +51.058%,
RSS +43.413%, allocation +35.269%. This counterexample invalidates acceptance.

The bounded replacement built a promoted map with `M.mapMaybe` and left-biased
`M.union`, removing most repeated-insertion allocation overhead. It still
failed the same five-pair dense gate: wall +20.126% ± 17.182 pp (all five
slower), RSS +61.449% ± 0.103 pp, allocation +2.067%, residency +55.406%.
Full 1,307/0 correctness and identical local products do not offset that
regression. No package-set repetition or threshold/cache workaround followed.
See [the batched rejection report](binding-visibility-batched-trial-20260910.md).
The separate original→Make+sparse N8 result (−8.590% wall, −40.838% RSS)
remains preserved in `make-binding-n8-evidence.tar.gz`, not an accepted result.

Read-only unknown generalization (`varIfUnknown`) is rejected for integration.
Four N4 confirmation pairs save 2.063 GB allocation (−0.51894% ± 0.00347 pp), but
wall −0.406% ± 2.695 pp and RSS −1.718% ± 3.014 pp establish neither a speed
nor peak-memory win. Full 1,305/0 and all 14 clean-run product/warning checks
pass. An independent dense-1000 N1 five-pair gate found allocation −0.41874%,
RSS +0.039% ± 0.063 pp, and no established timing change. Its subsequent full
N1 screen saved 0.52147% allocation but increased wall 2.92244% and RSS
2.08629%; all four package products/warning-content checks passed. This closed
as HOLD/unaccepted, not proof of slowdown. The user subsequently authorized
one final five-pair N1 block on xxlarge; no new source variant was included.
Retain [the original report](varifunknown-trial-20260910.md) and
[independent HOLD checkpoint](varifunknown-binary-verification-20260910.md).

That final N1 block completed once, with all five pairs and outliers retained.
Mean paired wall changed +1.6889% ± 1.1693 pp sample SD (four of five slower),
allocation −0.52047% ± 0.00109 pp, RSS +0.2295% ± 1.9598 pp, and maximum
residency +0.7818% ± 2.7588 pp. Mean allocation savings of 2.070 GB do not
justify the adverse timing and absent peak-memory benefit. This is a campaign
tradeoff rejection, not a correctness failure or universal slowdown claim.
No further repetitions, full N8, or cumulative inclusion follow.

The lead verified the final archive hash, reproduced its audit byte-for-byte,
and separately checked all 12 module-progress counters plus recorded binary,
harness, and manifest identities. Raw time/RTS statistics, sample order, original
745-warning/738-body multisets, and the original 8,985-product map all match.
Binary/corpus/output bytes are excluded from this archive; their worker rehash
is not represented as a lead rehash. See [the final N1 rejection report](varifunknown-n1-final-20260910.md).
Review archive `varifunknown-n1-final.tar.gz` has SHA256
`3677a5648836ba7836d2590ead5f52e410aebf05c1fb9312e3166c74a696100c`.

Both inliner changes are rejected. The positive-arity guard alone changed
N4 wall by −0.08% and allocation by −0.02%. Deferred argument-list creation
changed wall by +1.30%, RSS by +2.43%, and allocation by only −0.17%.
These are single-pair screens; neither justifies expensive repetitions.
Its 1,438-test suite and all products pass, but diagnostic auto-SCC allocation
attribution did not translate into material normal-O2 savings. Trial source
and tests remain on the worker branch; only negative evidence is for integration.

### Warning-force confirmation supports memory reduction

Forcing converted parser warnings before logging screened at N4 with RSS
−19.299%, residency −17.921%, and wall −0.741%; allocation was unchanged.
The interrupted confirmation contains only three complete pairs and cannot
satisfy its prespecified five-pair block. The fresh fixed-N4 five-pair block
completed on xxlarge with new excluded warmups and no historical pooling.
Mean paired RSS fell 24.7545% ± 3.6198 pp sample SD and sampled residency
23.7237% ± 3.8995 pp; all five pairs improved both memory metrics. Wall changed
−1.3265% ± 1.9924 pp, with two pairs slower; allocation was effectively unchanged
(+0.001672%). This is repeatable memory evidence, not consistent speedup.
See [the fresh confirmation report](performance-parser-warning-n4-xxlarge-20260910.md).

The lead verified the delivered archive and all 77 internal payload hashes,
both actual executable hashes, all 12 raw time/RTS/module-progress records,
original warning contents and saved product map, and all paired statistics.
Worker final output/corpus trees are excluded from that archive; their post-run
rehash remains worker evidence, not a claimed lead rehash of those trees.

Independent N1 one-pair RSS was −32.362%, residency −27.138%, wall −1.398%.
N8 three-pair RSS was −25.024% ± 1.159 pp and wall −2.625% ± 1.025 pp.
Dense five-pair N1 showed no material regression (+432 allocated bytes).
Tiny ten-pair N1/N4/N8 wall results were mixed; N1 mean +33 ms remains a
qualification, not proof of latency equivalence or universal speedup.

The supplied source passed the lead's 1,303/0 suite. An independent exact-source
optimized build also passed 1,303/0, separate Make 14/0, and focused warning/error
checks 5/0. The [independent correctness/evidence audit](parser-warning-force-correctness-audit-20260910.md)
is collected: all 104 archived file checksums pass, and the lead's offline
recomputation exactly matches its recorded JSON. Raw audits preserve all samples
and verify the package warning multiset. Those 745 warnings contain no `WarningParsingModule` codes:
nonempty parser warnings are covered by targeted tests, not this corpus.
Per-run product checks are supported by harness execution and saved maps;
archives do not contain every separate generated tree.

One type-heap diagnostic reduced total sampled peak 589.75→407.72 MiB and
near-end CST 51.20→0.36 MiB relative to saved Make evidence. Final-tenth CST
mean was 53.39→3.49 MiB, retaining excursions. This supports lifetime reduction,
not exact retaining roots or timing. See [the detailed trial report](parser-warning-force-trial-20260910.md)
and [independent N1 report](parse-warnings-force-binary-verification-20260910.md).

### External-fork trials remain isolated

Standalone traversal INLINABLE is rejected: one N4 pair had wall +3.737%,
RSS −1.096%, and unchanged allocation; excluded warmup was also slower.
Full 1,302/0 and product/warning equality passed. No repetitions or combination
with global flags followed. This does not refute the author's private-workload
result. See [the rejection report](performance-traversal-inlinable-20260910.md).

Equal-leaf guards initially closed without integration: N4 screen allocation
−1.651%, wall +2.672%; dense N1 five pairs allocation −1.672%, wall
+5.269% ± 16.162 pp, RSS +0.824%. Full 1,307/0 and output checks passed.
The [historical closure report](unify-leaf-fastpaths-trial-20260910.md) remains
unchanged. The user's later expansion reopened bounded final N4 and dense
validation on xxlarge. Both completed and remain **HOLD/unaccepted**: five-pair
N4 wall −2.2080% ± 3.1826 pp, RSS +0.5060% ± 2.5341 pp, allocation −1.65133%;
ten-pair dense N1 wall −3.1917% ± 7.7645 pp, RSS +0.8538%, allocation −1.67217%.
The dense 1.20→0.90 s pair stays in the data. Allocation repeats; a stable
speed or peak-memory win is not established. The lead verified all 165 archive
checksums, reproduced both analyses, and independently checked raw time/RTS
values and module progress for all 34 compiles. No full N1/N8 expansion follows.
See [the final leaf validation report](performance-leaf-validation-xxlarge-20260910.md).

The two global GHC flags produced a large allocation screen: −43.617% at N4,
wall 138.37→93.17 s, RSS +3.070%, residency +9.288%. Both measured runs were
slower than their excluded warmups, so −32.666% is not a precise speedup
estimate. Full 1,302/0 passed; stripped binary size rose 47.322%, and clean
compiler build took 768.70 s with 4,152,924 KiB maximum RSS. No matched baseline
compiler-build timing exists, so relative build cost is unquantified.

Completed standalone paired confirmation (SD is sample SD in percentage points):

| Block | Pairs | Wall change ± SD | Allocation change | Peak RSS change ± SD |
| --- | ---: | ---: | ---: | ---: |
| N4 | 5 | −29.5860% ± 2.7337 | −43.6296% | +4.1932% ± 4.2761 |
| N1 | 3 | −32.2495% ± 2.9184 | −43.6406% | +3.4177% ± 4.5339 |
| N8 | 3 | −29.7273% ± 1.1681 | −43.6308% | +10.8607% ± 4.3265 |

All wall pairs improved, but N4 baseline drift remains explicit. The N1 slower
final candidate and large first-pair memory effect remain included. The lead
reproduced N1/N4 analyses from saved raw records; the complete N1 archive also
allows independent rehashing of both binaries, all inputs, and its final products.
No cross-orb raw means are pooled. See [N4/build costs](performance-specialization-flags-20260910.md)
and [N1 verification](performance-specialization-n1-xxlarge-20260910.md).

The boundary worker completed 96 compiles: N8 above, dense N1 ten pairs, and
tiny N1/N4/N8 ten pairs each. Reported dense wall −20.6054%, allocation −43.7412%,
RSS +15.8146%; tiny wall −39.7901%/−43.5006%/−42.0528%, allocation about −49.6%,
RSS +8.5749%/+9.2463%/+6.1295%. All required product/diagnostic checks passed.
Their archive hashes are verified by the lead; the separate independent raw
audit is complete. The worker recorded a 30→29.5 GiB workload memory-limit
change between snapshots with unknown transition time; preserve that limitation.
Startup and cold paging are not isolated. See [the boundary report](specialization-boundaries-xxlarge-20260910.md).

The [independent specialization audit](specialization-independent-audit-20260910.md)
found no material contradiction across eight separate blocks: 120 raw compiles,
52 measured pairs and 16 excluded warmups. The lead verified all 548 audit
payload checksums, then replayed `audit.py` using all four original archives.
Its result JSON equals the independent worker's, including actual binary hashes,
all full/tiny/dense inputs, retained final product trees, exact warning contents,
and all raw statistics. No compiler or test was executed in either audit.
Per-run product comparisons remain harness assertions; nonexistent per-run
trees are not claimed as directly rehashed. Build/test validation is a supplied-log
audit, not a rebuild, and the full corpus has no `WarningParsingModule` warnings.
Review bundle `specialization-independent-audit.tar.gz` has SHA256
`69431d8a570db757443362d79e5a8f879177885e946bcf7035188cbdf1223611`.

### The cumulative experiment is exactly warning-force plus both flags

The selected experimental union is parser-warning source/test
[2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f)
and attributed flags config
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63),
on Make-equivalent report checkpoint
[c44600e8](https://github.com/vtrl/purescript/commit/c44600e8f7a390ad73a46f8932e8f90a21b40a8a).
Its separately owned xxlarge worker completed the exact two-patch union,
normal optimized build and full tests, preserving the final tested executable.
Leaf, varIfUnknown, and all rejected variants are excluded. This is a reversible
experimental branch, not acceptance into the campaign.

Tested source is
[d0d4b331](https://github.com/vtrl/purescript/commit/d0d4b3313f37ad9f79997eb6a852dc83aa2f0336).
The lead independently compared both patch hashes with their original candidates,
checked the supplied full 1,303/0 and Make 14/0 logs, and rehashed the delivered
stripped binary: 70,523,064 bytes, SHA256
`4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d`,
preserved as `.build/perf/purs-cumulative-tested`, mode 0555. The supplied Make
regression covers asymmetric skipped/independent parser-warning spans. The
1,268.90 s build includes uncached dependencies and test compilation, not a
matched baseline build-cost estimate. See [the cumulative correctness report](performance-cumulative-warning-specialization-20260910.md).
Build evidence `cumulative-warning-specialization-build.tar.gz` has SHA256
`459d46b9cefa5a07734d3dd1dbf1fe0132e86165424b12f5268f94802e8dc160`.

No standalone percentage is added or assumed to offset another: warning-force
memory savings may interact with the flags' increased residency. The prescribed
fresh matrix is full N1 three pairs, N4 five pairs, N8 three pairs; dense1000 N1
ten pairs; and tiny N1/N4/N8 ten pairs each. Each block excludes one clean warmup
per binary, then uses BC/CB alternating order beginning BC, explicit capabilities,
warm cache and JS-only code generation through the unchanged harness. All use
the tested cumulative binary above against preserved Make baseline SHA256
`7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
No outliers may be dropped, old samples pooled, or adaptive repeats added.
Full/tiny original products and warning contents, local dense products/diagnostics,
and independent raw-evidence audit are required before any integration claim.
No cumulative timing result is available at this checkpoint.
