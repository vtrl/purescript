# Cumulative candidate: independent N8 and small-input boundaries

All 96 prescribed compiles passed their required checks. Full N8's paired
wall mean changed **-28.1745% (sample SD 5.1006 percentage points)**, with
allocation -43.6307%, peak RSS -12.8477%, and sampled residency -11.4866%.
Dense/tiny mean wall times also fell, but their peak RSS increased; two of
ten dense pairs were slower. Every sample is retained.

This is binary-only measurement of the **warning-force plus specialization
flags** candidate, not acceptance or integration of its source. The standalone
specialization measurements and archives are preserved and are not pooled
with these samples. No additional thread, compiler build, test run, source
variant, or harness modification was made by this worker.

## Exact checkout and executable identities

Report-only branch `perf/cumulative-boundaries-xxlarge-20260910` starts at
[8bc296a5](https://github.com/vtrl/purescript/commit/8bc296a5d4218180c078d5ff481c4c4b6cb761fe),
the exact pushed campaign checkpoint supplied by the lead, not a moving
default branch. Its compiler/test/config files match accepted Make release
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The sole remote's fetch and push URLs are `https://github.com/vtrl/purescript`;
only this report branch is pushed, with no master/upstream integration. The
repository was already unshallowed before this task's history inspection.

| Binary | Bytes | SHA256 |
| --- | ---: | --- |
| B: accepted Make, local `.build/perf/purs-baseline` | 47,869,944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: lead's frozen `.build/perf/purs-cumulative-tested` | 70,523,064 | `4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d` |

C was downloaded directly from the lead and restored to mode 0555; B is also
0555. C's embedded version identifies source
[d0d4b331](https://github.com/vtrl/purescript/commit/d0d4b3313f37ad9f79997eb6a852dc83aa2f0336).
This combines the warning-force change and regression test from
[2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f) with the two
specialization flags from
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63).
B embeds the older dirty build identifier
`6d636561f72b7985962e2931e462aa017a38f7fc DIRTY`; its exact supplied hash,
rather than the version string alone, identifies the accepted executable.
The standalone candidate `4a918e8f...` was **not** used in cumulative timing.

These are the supplied normal optimized GHC 9.6.6 / Stack 3.3.1 /
lts-22.43 executables, not profiling or fast builds. The lead reported C's
optimized full suite at 1303/0 and Make suite at 14/0. The lead also reported
the standalone audit and full replay passing all 120 records. Those are
external correctness context, not runs or samples from this worker.

The flags are `-fspecialize-aggressively` and `-fexpose-all-unfoldings` in
Stack local GHC options and Cabal's `package purescript`. Their original
author is **seastian <seastian@users.noreply.github.com>**, from
[upstream PR 4584](https://github.com/purescript/purescript/pull/4584) /
[8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
[OxfordAbstracts PR 16](https://github.com/OxfordAbstracts/purescript/pull/16)
imported that work; no documented Claude attribution is claimed.

## Prespecified method and preserved preparation failure

The unchanged harness `ci/benchmark-compiler.py` has SHA256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
The existing pinned fixtures were reused only after every actual source/FFI
byte was rehashed, not merely their manifest files:

| Fixture | Sources / inputs | SHA256 identity |
| --- | ---: | --- |
| Full set 60.4.0 | 4084 / 4901 | `inputs.json`: `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| Tiny Sequence | 57 / 78 | `inputs.json`: `3736e1f30a4ab6ceb49ed90349358f81e3ef75b17a0fcbf8c3253e8efb92d4cb` |
| Dense1000 | 1 / 1 | `Dense.purs`: `408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7` |

Full inputs were originally prepared using Spago 0.93.43 and registry commit
`5d834cd364da1d49a1bd1b0219ab49fb15f20601`. Tiny retains the original Main and
prelude 6.0.1/effect 4.0.0/console 6.1.0 source/FFI bytes. Dense uses the same
local path for both executables, is compile-only, and its cyclic generated
functions are never executed. Old standalone raw/product archives were
checked before any clean output was reused.

Exactly five sequential blocks are prescribed: full N8, 3 pairs; Dense1000
N1, 10 pairs; tiny N1, N4, N8, 10 pairs each. Every block has one excluded
clean warmup per executable, then alternating BC/CB pairs beginning BC.
The total is 43 measured pairs / 86 measured compiles plus 10 excluded
warmups, or 96 compiles. No screen, adaptive repetition, outlier removal,
old-sample pooling, or bare `-N` is permitted.

Every compile uses clean output, warm filesystem cache, `--codegen js`, and
fixed `+RTS -N1 -s -RTS`, `+RTS -N4 -s -RTS`, or `+RTS -N8 -s -RTS`.
`--baseline` names exact Make; `--compiler` names exact cumulative C. The
harness clears `GHCRTS`, fixes C locale, and records the full direct argv.
Downloads, input hashing, and setup finished before timing, with no competing
build/test/audit workload during timing.

Managed services run the archived `run-once.sh`, with separate atomic
per-block started-directories, actual completion statuses, and an idle
`exec sleep infinity` hold. They are collected and stopped before the next
block. Before/after snapshots record CPU/memory and ancestor cgroup limits,
counters, and available pressure fields; no limits are modified.

**One setup failure occurred before any compiler invocation.** The initial
service's pre-timing snapshot tried to read `/proc/pressure/{cpu,memory,io}`,
which this orb lacks, and exited 1. The harness never started and created no
results directory, metadata, timing file, or sample. The original script,
guard, log, status, and snapshots remain intact under
`.build/perf/cumulative-boundaries/`. This was reported to the lead before
continuing. A snapshot-only check verified the optional-field handling; a
distinct guard/results root, `.build/perf/cumulative-boundaries-measured/`,
then began the first actual measurement. No failed guard was removed or
reused, and no measured run was retried. This preparation failure must not
be described as an extra screen or discarded timing sample.

## Results and verification

All five measured blocks completed with exit 0, as did their after-snapshot
captures. They were collected/stopped sequentially. There were no failed,
interrupted, restarted, added, or discarded compiler measurements. The
separate zero-compile preparation failure above is preserved, not omitted.

### Independent within-pair statistics

Each change is \(100(C_i-B_i)/B_i\), paired by numbered run, not a ratio of
aggregate means. SD is sample SD over paired percentages, with denominator
\(n-1\), expressed in **percentage points**, not a confidence interval.
Negative values mean less time/allocation/memory. Every raw `.time` and RTS
record was independently parsed and checked against the harness sample row;
these summaries agree with the harness within 1e-10.

| Block | Metric | Mean change | Sample SD (pp) | Median change | Range |
| --- | --- | ---: | ---: | ---: | --- |
| Full N8, 3 pairs | Wall | -28.1745% | 5.1006 | -28.6994% | -32.9925% … -22.8318% |
| Full N8 | Allocation | -43.6307% | 0.0018 | -43.6316% | -43.6319% … -43.6286% |
| Full N8 | Peak RSS | -12.8477% | 3.4204 | -10.9748% | -16.7955% … -10.7727% |
| Full N8 | Sampled residency | -11.4866% | 6.2710 | -10.7352% | -18.0994% … -5.6251% |
| Dense1000 N1, 10 pairs | Wall | -12.8782% | 12.4587 | -10.3745% | -33.7209% … +4.4776% |
| Dense1000 N1 | Allocation | -43.7443% | 0.0000 | -43.7443% | -43.7443% … -43.7443% |
| Dense1000 N1 | Peak RSS | +15.8500% | 0.0972 | +15.8448% | +15.6987% … +16.0182% |
| Dense1000 N1 | Sampled residency | +15.4928% | 0.0001 | +15.4928% | +15.4925% … +15.4928% |
| Tiny N1, 10 pairs | Wall | -39.2261% | 6.7584 | -38.7578% | -47.3684% … -28.3186% |
| Tiny N1 | Allocation | -49.6742% | 0.0001 | -49.6742% | -49.6743% … -49.6740% |
| Tiny N1 | Peak RSS | +8.0487% | 1.8836 | +7.8000% | +4.6140% … +11.6065% |
| Tiny N1 | Sampled residency | -4.4901% | 7.8737 | -1.0595% | -17.8911% … +6.8945% |
| Tiny N4, 10 pairs | Wall | -42.3102% | 4.6490 | -42.7146% | -48.2353% … -32.9268% |
| Tiny N4 | Allocation | -49.6291% | 0.0498 | -49.6407% | -49.6883% … -49.5529% |
| Tiny N4 | Peak RSS | +9.3452% | 2.9287 | +8.5533% | +5.6259% … +14.5167% |
| Tiny N4 | Sampled residency | +3.5193% | 17.6959 | +0.5329% | -14.8753% … +48.1484% |
| Tiny N8, 10 pairs | Wall | -44.6106% | 0.8297 | -44.8529% | -45.7831% … -43.0380% |
| Tiny N8 | Allocation | -49.6993% | 0.0918 | -49.6670% | -49.9142% … -49.6219% |
| Tiny N8 | Peak RSS | +6.3624% | 3.1066 | +6.5769% | +1.5453% … +10.1765% |
| Tiny N8 | Sampled residency | +0.8796% | 8.8198 | -0.7027% | -10.5199% … +16.4375% |

### Absolute wall times and excluded warmups

Seconds. Other absolute metric distributions are retained in each block's
`independent-analysis.json`; all within-pair metric values are retained too.

| Block | B mean / sample SD | C mean / sample SD | B median [range] | C median [range] | Excluded warmups B / C |
| --- | --- | --- | --- | --- | --- |
| Full N8 | 72.220 / 1.83714 | 51.81667 / 2.57706 | 71.43 [70.91, 74.32] | 50.93 [49.80, 54.72] | 71.43 / 49.69 |
| Dense1000 N1 | 0.763 / 0.06651 | 0.658 / 0.05181 | 0.770 [0.67, 0.86] | 0.685 [0.57, 0.71] | 0.84 / 0.55 |
| Tiny N1 | 1.271 / 0.08239 | 0.768 / 0.04686 | 1.270 [1.13, 1.38] | 0.765 [0.70, 0.83] | 1.33 / 0.78 |
| Tiny N4 | 0.819 / 0.04999 | 0.472 / 0.04211 | 0.805 [0.77, 0.92] | 0.455 [0.42, 0.55] | 0.79 / 0.48 |
| Tiny N8 | 0.818 / 0.01874 | 0.453 / 0.00823 | 0.820 [0.79, 0.85] | 0.450 [0.44, 0.47] | 0.80 / 0.47 |

### Every measured wall pair

B → C seconds. Odd pairs ran BC; even pairs ran CB. The slower dense
candidates in pairs 2 (+1.4286%) and 6 (+4.4776%) are included.

| Pair | Full N8 | Dense1000 N1 | Tiny N1 | Tiny N4 | Tiny N8 |
| ---: | --- | --- | --- | --- | --- |
| 1 | 74.32 → 49.80 | 0.68 → 0.64 | 1.33 → 0.70 | 0.85 → 0.44 | 0.84 → 0.46 |
| 2 | 70.91 → 54.72 | 0.70 → 0.71 | 1.21 → 0.77 | 0.77 → 0.42 | 0.85 → 0.47 |
| 3 | 71.43 → 50.93 | 0.80 → 0.64 | 1.24 → 0.76 | 0.77 → 0.46 | 0.81 → 0.45 |
| 4 | — | 0.73 → 0.68 | 1.20 → 0.80 | 0.78 → 0.49 | 0.83 → 0.45 |
| 5 | — | 0.81 → 0.57 | 1.30 → 0.70 | 0.82 → 0.55 | 0.80 → 0.45 |
| 6 | — | 0.67 → 0.70 | 1.38 → 0.75 | 0.92 → 0.53 | 0.82 → 0.46 |
| 7 | — | 0.84 → 0.69 | 1.34 → 0.82 | 0.85 → 0.49 | 0.82 → 0.45 |
| 8 | — | 0.86 → 0.57 | 1.36 → 0.74 | 0.78 → 0.44 | 0.82 → 0.45 |
| 9 | — | 0.76 → 0.69 | 1.22 → 0.83 | 0.79 → 0.45 | 0.79 → 0.45 |
| 10 | — | 0.78 → 0.69 | 1.13 → 0.81 | 0.86 → 0.45 | 0.80 → 0.44 |

`raw-samples.csv` contains all 96 records, with excluded warmups marked,
wall/user/system/RSS/allocation/residency, copied bytes, INIT/MUT/GC/EXIT/Total
CPU and elapsed times, and Gen0/1 collection counts, CPU/elapsed time, average
pause, and maximum pause. Full raw stdout/stderr/time files and the harness's
monotonic times remain intact. As descriptive N8 averages, MUT elapsed is
27.580 → 19.2663 s, GC elapsed 44.5657 → 32.481 s, and copied bytes
69,922,769,034.7 → 63,013,272,810.7. These are not separate timed experiments
or a causal decomposition of the two source changes.

### Correctness: per-run assertions and direct final checks

- **Full N8, 8/8 compiles:** the harness asserted exact product equality
  after each run; its 8,985-entry map matches the original reference SHA256
  `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
  Every stdout independently matches 745 warnings / 738 distinct bodies.
- **Dense1000 N1, 22/22 compiles:** both local JS/externs products match
  across B/C and repetitions; stdout is empty and compiler diagnostics before
  RTS statistics match exactly after outer whitespace only. Both binaries
  use the same source path; no cross-orb externs-byte assumption is made.
- **Tiny N1/N4/N8, 66/66 compiles:** each block's asserted product map equals
  the original 135 products; all three warning bodies match in every stdout.

Warning comparison removes only ordinal headers and outer whitespace,
retains duplicates and all internal whitespace/content, and ignores order.
The original reference files and hashes are archived. All checks include
excluded warmups. In addition to the harness's per-run assertions, this
worker **directly rehashed every actual final product after each block**,
before tiny output was reused. Those maps are separately saved as
`direct-final-products.json`. The final three retained trees contain 8,985
full + 2 dense + 135 tiny products. Intermediate per-run trees are deleted
by the unchanged clean-output harness, not independently retained snapshots.

After all timing, `verify-identities.py postflight` rehashed every actual
4,901 full / 78 tiny / 1 dense input, B/C executables, unchanged harness,
original references, and both preserved standalone archives: **PASS**.
The independent analyzer reparsed all 96 raw time/RTS records: **PASS**.

### Observed machine limits and counters

Existing `a1.xxlarge` orb: 16 logical CPUs, 8 cores × 2 SMT, Intel Xeon
2.60 GHz family 6/model 106/stepping 6, Linux 6.1.158+, x86-64 KVM; physical
memory 33,669,939,200 bytes, no swap. All ten measured-block boundary
snapshots show workload `cpu.max = max 100000`, `memory.max = 31675383808`
(29.5 GiB), and CPU affinity 0–15. **No limit transition was observed across
these snapshots; constancy between snapshots is not asserted.** All workload
throttled-period/time and memory-event deltas (including high/max/OOM/kills)
are zero. `/proc/pressure` is unavailable, explicitly recorded as such.

| Block | Before → after (UTC, 2026-09-10) | Workload CPU usage delta (µs) |
| --- | --- | ---: |
| Full N8 | 14:48:18 → 14:56:38 | 2,029,184,059 |
| Dense1000 N1 | 14:57:25 → 14:57:42 | 16,084,753 |
| Tiny N1 | 14:57:52 → 14:58:15 | 23,170,051 |
| Tiny N4 | 14:58:41 → 14:58:56 | 28,340,908 |
| Tiny N8 | 14:59:02 → 14:59:17 | 32,134,199 |

These cgroup deltas cover warmups, measured compiles, harness hashing, and
other workload-cgroup overhead; they are not compiler-only CPU timings.
Raw per-run GNU-time and RTS CPU data are separate. Complete before/after
ancestor-cgroup snapshots and machine/process records are retained, with
counter differences in `environment/block-counter-summary.json`.

## Interpretation limits

This matrix compares the cumulative candidate directly with exact Make.
It does not estimate the incremental effect over standalone flags and cannot
establish additive speedups. Separate standalone means are not pooled or
subtracted. A single orb and three full N8 pairs do not support a universal
percentage; VM drift, workloads, and capability settings can change results.

The candidate is 22,653,120 bytes larger (+47.3222%). Warm-cache clean tiny
compilation includes startup and fixed compilation overhead, but does not
isolate pure startup or cold paging. No startup-equivalence claim is made.
Maximum sampled residency is GC-schedule-sensitive and is not peak RSS.
No source acceptance follows from this report: the complete matrix and the
independent audit are required before the lead's acceptance decision.

## Transferable archives and independent replay

The files are available from this worker's
[Amp thread](https://ampcode.com/threads/T-01a08b7b-01a8-7007-b2c0-01d41c78f59d),
under `.amp/in/artifacts/cumulative-boundaries-20260910/`:

| File | Bytes | SHA256 |
| --- | ---: | --- |
| `raw-evidence.tar.gz` | 3,281,463 | `c9874a504036ac6a12de8fcf64d07b93b61a7359dc879bd2220a2f2c8c5c4de7` |
| `verified-products.tar.gz` | 19,111,380 | `2b40177c0cda4d1d02f0332cf2bb694cae724b53900538a6db8ca6e75ef438d9` |
| `raw-samples.csv` | 19,841 | `5331aa32a44fd151dec3580fec2902575c740ab66e30e5b0fe6e4a6ce40219ee` |

The adjacent `SHA256SUMS` verifies all three. Both archives also have internal
per-file manifests. The raw archive contains 488 data files: every run's
stdout/stderr/time, samples, summaries, independent analysis, direct final
product maps, metadata/argv, original references, fixture manifests and exact
dense/tiny inputs, unchanged harness/preparation script, per-block machine
snapshots/counters, pre/postflight actual-byte identity records, and both the
preserved zero-compile preparation attempt and the successful run-once
scripts/guards/completions. The products archive contains all 9,122 final
product files. Compiler executables and build/dependency caches are excluded.

All download hashes, all 488 raw-file hashes, and all 9,122 product-file
hashes were verified. Extracting both archives into a fresh directory and
running `python3 .build/perf/cumulative-boundaries-measured/analyze.py` passed
all five blocks / 43 pairs / 96 records and directly rehashed all retained
trees, without executing any compiler. Each archive has a root `SHA256SUMS`;
check its internal manifest before extracting the second over it. Do not
rerun the managed measurement scripts or remove their atomic guards.

The old standalone report branch and archives are unchanged. Both old archive
hashes were verified before and after cumulative timing; their final products
were already archived before clean outputs were reused. All measured services
are stopped, and both supplied cumulative comparison binaries remain 0555.
