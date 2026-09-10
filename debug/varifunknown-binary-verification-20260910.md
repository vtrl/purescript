# varIfUnknown independent binary verification — 2026-09-10

Decision: **HOLD / unaccepted**, confirmed by the lead. Dense-1000 passed and
the allocation saving is independently reproduced, but the one N1 package-set
pair was 2.922% slower with 2.086% higher peak RSS. The screen does not establish
a repeatable regression or improvement; it does not justify integrating the
allocation-only 0.521% saving. No extra timed repetitions were run.

## Binary-only scope and provenance

No compiler rebuild, source edit, or new test was performed. The evidence branch
`perf/varifunknown-verification-20260910` starts at exact campaign report-only
HEAD `4861c1e1e418122a172ad4b707f4ba5f8a2c5361`; compiler source remains accepted
Make `38080a40fc3a53de813a0ac46187e8c709dcbcc7`. The rejected visibility source,
reports, and archives remain untouched on their separate branches.

Candidate source and correctness provenance were supplied by the lead:

- Source `7b69e68aadada69371190b735dcd22ac205d42e3`, described as a pure traversal
  which snapshots `substNames` once in `varIfUnknown`.
- Original worker report `b9f5e3bfb0bde910f2eedb3636bc5f2de44d00d5`.
- Original worker reported full-suite **1,305 examples, 0 failures**; this was
  not rerun here, since this assignment is strictly binary-only.
- Original worker's N4 confirmation (four pairs): allocation -0.51894% ±
  0.00347 percentage points sample SD, about 2.063 GB; wall -0.406% ± 2.695
  points and RSS -1.718% ± 3.014 points. Those results did not establish speedup.

The candidate was downloaded from worker thread
`T-01a08b0b-2da5-711e-b078-b5fb012bdc11`, file
`.build/perf/purs-varifunknown-tested-release`, without rebuilding it.
Both binary SHA-256 hashes were verified before timing:

| Binary | Exact SHA-256 |
| --- | --- |
| Accepted Make baseline | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| varIfUnknown candidate | `604b26f6e2532804378a6717140150b470bccea148c47ccad23f8dd6b23ce8e2` |

The baseline remains at `/home/user/perf-binding-batched/purs-make-baseline`.
The downloaded candidate is retained read-only at
`/home/user/perf-varifunknown-verification/purs-varifunknown-tested-release`.
Their embedded version strings refer to older dirty source snapshots; the
verified binary hashes, not those strings, identify this experiment.

## Common protocol

Both stages use the exact e926 paired harness, SHA-256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
The harness invokes each binary directly, uses clean JS output for every run,
clears `GHCRTS`, and passes `+RTS -N1 -s -RTS`. It holds the corpus lock and
checks generated-product hashes after every successful compile, outside timing.
One full clean warm-up per binary is excluded from each stage's measurements.
No builds, tests, dependency fetching, or other compiler timings ran concurrently.
Both stages ran in this same 8-vCPU/16-GiB Linux x86_64 orb.

## Dense-1000 gate: no material regression observed

The input is the exact valid typed 1,000-function mutual-recursion cycle used
to reject the visibility variants. It is compiled only; its recursive
functions are never executed. Source SHA-256:
`408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7`.
Input manifest SHA-256:
`6b017a4d18606d78ffc298ad87528ffe1ae7e8848f763c4fd8ed345e7d910e6d`.

```sh
work=/home/user/perf-varifunknown-verification
python3 ci/benchmark-compiler.py \
  --corpus /home/user/perf-binding-batched/lead-dense/dense-recursive-1000 \
  --compiler "$work/purs-varifunknown-tested-release" \
  --baseline /home/user/perf-binding-batched/purs-make-baseline \
  --results "$work/dense-1000-n1-paired-five" \
  --label exact-Make38080-vs-varIfUnknown7b69e68-binary-only \
  --capabilities 1 --samples 5
```

Five pairs alternate B/C on odd pairs and C/B on even pairs. All uncertainties
are sample SD, not confidence intervals. Percentage changes are calculated per
pair before summarizing.

| Pair | Order | Baseline wall (s) | Candidate wall (s) |
| --- | --- | ---: | ---: |
| 1 | B/C | 0.93 | 0.86 |
| 2 | C/B | 0.93 | 0.95 |
| 3 | B/C | 0.85 | 0.84 |
| 4 | C/B | 0.86 | 0.84 |
| 5 | B/C | 0.84 | 0.90 |

| Metric | Baseline mean ± SD | Candidate mean ± SD | Paired change %, mean ± SD |
| --- | ---: | ---: | ---: |
| Wall (s) | 0.882 ± 0.0444 | 0.878 ± 0.0471 | -0.347 ± 5.443 |
| Peak RSS (KiB) | 144,913.6 ± 60.7 | 144,970.4 ± 35.6 | +0.03922 ± 0.06285 |
| Allocation (bytes) | 1,186,420,136 ± 0 | 1,181,452,136 ± 0 | -0.418739 ± 0 |
| Maximum residency (bytes) | 59,255,504 ± 0 | 59,255,475.2 ± 39.4 | -0.000049 ± 0.000067 |

Allocation falls by exactly 4,968,000 bytes in every pair. Wall differences are
mixed and small relative to their spread; RSS and residency are effectively
unchanged. No material memory/time regression was observed, so the gate passed.
Warm-ups B0.90/C0.92 s are excluded. All 12 runs have identical JS and externs
products and empty diagnostic stdout; the products also match the prior local
exact-Make-baseline result on this same corpus path.

## Pinned-corpus N1 screen

The full package set is 60.4.0, all 506 packages, 4,084 PureScript sources and
4,901 total inputs. The required manifest matched:
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
Only one measured B/C pair follows the two excluded warm-ups; there is no
sample-SD estimate for this screen.

```sh
work=/home/user/perf-varifunknown-verification
python3 ci/benchmark-compiler.py \
  --corpus /home/user/perf-binding/corpus \
  --compiler "$work/purs-varifunknown-tested-release" \
  --baseline /home/user/perf-binding-batched/purs-make-baseline \
  --results "$work/package-n1-paired-screen" \
  --label exact-Make38080-vs-varIfUnknown7b69e68-binary-only \
  --capabilities 1 --samples 1
```

| Metric | Baseline | Candidate | Change |
| --- | ---: | ---: | ---: |
| Wall (s) | 243.29 | 250.40 | +2.922438% |
| Peak RSS (KiB) | 1,766,776 | 1,803,636 | +2.086286% |
| Allocation (bytes) | 397,398,359,424 | 395,326,039,312 | -0.521472% |
| Maximum residency (bytes) | 609,940,064 | 608,664,384 | -0.209148% |

The measured allocation saving is **2,072,320,112 bytes (2.072 GB)**. Excluded
warm-ups were B247.99/C250.26 s, allocating 397,395,042,856/395,322,816,384 bytes
with RSS 1,798,328/1,868,032 KiB respectively. The candidate was slower in the
one measured pair and in the warm-up comparison; there is no measured-pair SD
and no claim of a repeatable slowdown or speedup. No sample was dropped.

## Equality audit and evidence

All **16** compile-only runs succeeded. All four package runs match the original
**8,985-product** manifest and **745 warning contents**, compared as a multiset
with duplicate counts preserved. Only numbered `Warning i of n` headers,
surrounding whitespace, and ordering are ignored. All 12 dense runs match two
products and have empty diagnostic stdout. `verification.json` records the audit.

Package product-manifest SHA-256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
Canonical warning-content SHA-256:
`d30f6e29be13b1e6a7954fc1f38f1fc7c8d9adeebf67270095cb5ff19a30b707`.
Binary hashes were verified again after all timings and remain unchanged.

Review archive:
`.amp/in/artifacts/varifunknown-verification-20260910/evidence.tar.gz`, SHA-256
`b02f9928ff067757dc5c52c20f9df89d1024539e5ccb5054a1cef3549f616bd1`.
It includes all run logs, timing/RTS output, metadata, summaries, products, exact
harness, binary identities, verification audit, dense source and manifests, and
the pinned package-set manifests. It does not include copied compiler binaries
or claim a local build/test run. Raw data remains under
`/home/user/perf-varifunknown-verification`.

This report is the only tracked change. The accepted Make compiler source and
tests remain unchanged, and no candidate source was integrated. Only the
evidence branch is pushed to verified `origin` at
`https://github.com/vtrl/purescript`. Prior visibility evidence remains intact.
The bounded checkpoint is complete: retain these positive allocation and
negative timing/RSS observations, leave the candidate unaccepted, and do not
repeat without a new explicit assignment.
