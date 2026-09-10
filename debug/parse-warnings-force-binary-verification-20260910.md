# Parser warning forcing: independent binary verification — 2026-09-10

Status: verification complete. Dense-1000 passed without a material regression;
the single package-set N1 pair reduced peak RSS by 32.362% and maximum residency
by 27.138%, with essentially unchanged allocation. Retain for the lead's
confirmation and acceptance decision as a memory-lifetime improvement. This
single package pair does not establish a speedup or accept the candidate.
This is a bounded binary-only verification. No rebuild, compiler source edit,
new test, or additional timing repetition is part of this assignment.

## Scope and identity

The candidate was downloaded from the lead thread
`T-01a08a95-ee2d-757c-9930-0bba3e678f5c`, file
`.build/perf/parse-warnings-force/purs-candidate`, and its exact supplied hash
was verified. Both binaries are 47,869,944 bytes:

| Binary | SHA-256 |
| --- | --- |
| Accepted Make baseline | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| Parser-warning-force candidate | `92643eb2d1100711e7e8da39168202d76246ae0a6e335edf640bcac7c0d52e1d` |

The baseline remains at `/home/user/perf-binding-batched/purs-make-baseline`.
The candidate is retained read-only at
`/home/user/perf-parse-warnings-force-verification/purs-candidate`.
The supplied source identity is `2d5cc3bb3b766551e0b6c341dc84fc760915298f` on
`perf/parse-warnings-force-20260910`, with report-only/Make-equivalent parent
`3f570156`. As described by the lead, the one functional line forces
`CST.toMultipleWarnings` before `tell`, inside the existing semaphore. The
candidate contains neither visibility trial nor varIfUnknown change.

This orb did not rebuild or independently edit that source. Its evidence-only
branch `perf/parse-warnings-force-verification-20260910` starts at exact campaign
report-only HEAD `4861c1e1e418122a172ad4b707f4ba5f8a2c5361`; the local compiler
and test source remain accepted Make `38080a40fc3a53de813a0ac46187e8c709dcbcc7`.

Lead-reported evidence, not rerun here:

- Normal O2 full suite: **1,303 examples, 0 failures**. The exact skipped and
  independent warning-location test also passes the baseline.
- N4 screen: B110.72/C109.90 s (-0.741%), RSS 1,967,444/1,587,744 KiB
  (-19.299%), maximum residency 683,120,616/560,696,616 bytes (-17.921%);
  allocation essentially unchanged.
- Excluded N4 warm-ups: B111.00/C108.89 s, RSS 1,987,832/1,458,720 KiB.
  All four runs match 8,985 products and 745 warning contents.
- The lead owns a separate five-pair N4 confirmation; these independent N1
  measurements do not duplicate or replace it.

## Common protocol

The exact e926 paired harness is unchanged, SHA-256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
It invokes the preserved binaries directly, clears `GHCRTS`, holds the corpus
lock, deletes output before every JS compile, and passes `+RTS -N1 -s -RTS`.
Hashing is outside the timed process. One complete clean warm-up per binary is
excluded from each stage's measurements. There was no competing build, test,
dependency-fetch, or compiler-timing workload in this 8-vCPU/16-GiB orb.

## Dense-1000 gate passed without a material regression

The exact typed 1,000-function mutual-recursion cycle is compiled only, never
executed. Source SHA-256:
`408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7`.
Input manifest SHA-256:
`6b017a4d18606d78ffc298ad87528ffe1ae7e8848f763c4fd8ed345e7d910e6d`.

```sh
work=/home/user/perf-parse-warnings-force-verification
python3 ci/benchmark-compiler.py \
  --corpus /home/user/perf-binding-batched/lead-dense/dense-recursive-1000 \
  --compiler "$work/purs-candidate" \
  --baseline /home/user/perf-binding-batched/purs-make-baseline \
  --results "$work/dense-1000-n1-paired-five" \
  --label exact-Make38080-vs-parser-warning-force2d5cc3b-binary-only \
  --capabilities 1 --samples 5
```

| Pair | Order | Baseline wall (s) | Candidate wall (s) |
| --- | --- | ---: | ---: |
| 1 | B/C | 0.94 | 0.86 |
| 2 | C/B | 0.90 | 0.93 |
| 3 | B/C | 1.00 | 0.97 |
| 4 | C/B | 0.96 | 0.99 |
| 5 | B/C | 0.87 | 0.88 |

All uncertainties are sample SD, not confidence intervals. Percentage changes
are calculated per pair before summarizing.

| Metric | Baseline mean ± SD | Candidate mean ± SD | Paired change %, mean ± SD |
| --- | ---: | ---: | ---: |
| Wall (s) | 0.934 ± 0.0508 | 0.926 ± 0.0559 | -0.781 ± 5.015 |
| Peak RSS (KiB) | 144,904.8 ± 72.1 | 144,925.6 ± 117.4 | +0.01436 ± 0.07547 |
| Allocation (bytes) | 1,186,420,136 ± 0 | 1,186,420,568 ± 0 | +0.000036 ± 0 |
| Maximum residency (bytes) | 59,255,633.6 ± 306.2 | 59,255,475.2 ± 39.4 | -0.000267 ± 0.000560 |

The allocation difference is a constant 432 bytes. Wall changes are mixed and
small relative to their spread; RSS and residency are effectively unchanged.
No material regression was observed, so the package-set screen was permitted.
Warm-ups B1.00/C0.95 s are excluded. All 12 runs produce identical JS and
externs products, matching the prior local exact-Make-baseline products, with
empty diagnostic stdout.

## One pinned-corpus N1 screen

The corpus is the unchanged full package set 60.4.0: 506 packages, 4,084
PureScript sources and 4,901 total inputs. Required manifest SHA-256:
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
One measured B/C pair follows the two excluded warm-ups. There is no
sample-SD estimate for this screen and no extra repetition.

```sh
work=/home/user/perf-parse-warnings-force-verification
python3 ci/benchmark-compiler.py \
  --corpus /home/user/perf-binding/corpus \
  --compiler "$work/purs-candidate" \
  --baseline /home/user/perf-binding-batched/purs-make-baseline \
  --results "$work/package-n1-paired-screen" \
  --label exact-Make38080-vs-parser-warning-force2d5cc3b-binary-only \
  --capabilities 1 --samples 1
```

| Metric | Baseline | Candidate | Change |
| --- | ---: | ---: | ---: |
| Wall (s) | 246.83 | 243.38 | -1.397723% |
| Peak RSS (KiB) | 1,809,712 | 1,224,052 | -32.362055% |
| Allocation (bytes) | 397,400,888,752 | 397,396,305,680 | -0.001153% |
| Maximum residency (bytes) | 622,672,976 | 453,692,968 | -27.137842% |

Excluded warm-ups:

| Metric | Baseline | Candidate |
| --- | ---: | ---: |
| Wall (s) | 245.83 | 246.40 |
| Peak RSS (KiB) | 1,817,720 | 1,216,952 |
| Allocation (bytes) | 397,393,726,048 | 397,397,694,032 |
| Maximum residency (bytes) | 613,357,192 | 462,219,152 |

Both candidate runs show substantially lower peak RSS and maximum residency,
whereas warm-up and measured wall changes have opposite signs. These results
support the memory-lifetime hypothesis, not a speedup claim. The allocation
difference is only 4,583,072 bytes of approximately 397.4 GB.

## Product and warning audit passed

Both harness commands exited successfully. All 16 compile-only runs passed
the harness's per-run product comparison: 12 dense runs produce the same two
products and four package runs produce the same 8,985 products. The package
product dictionary also exactly matches the original source-baseline manifest,
whose SHA-256 is
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
The input manifest still has the required `2d7ae344...` identity above.

An independent diagnostic audit compared every package run, including both
warm-ups, against the original baseline's 745-warning content multiset.
It strips only the numbered `Warning i of n` headers and surrounding block
whitespace, preserving contents and duplicate counts while ignoring ordering.
Every run has exactly 745 headers, each reporting a total of 745, and every
content multiset matches. The canonical multiset SHA-256 is
`d30f6e29be13b1e6a7954fc1f38f1fc7c8d9adeebf67270095cb5ff19a30b707`.
All 12 dense runs have empty diagnostic stdout. The audit assertions completed
successfully and are summarized in the archive's `verification.json`.

## Evidence and decision

Raw results remain under
`/home/user/perf-parse-warnings-force-verification/{dense-1000-n1-paired-five,package-n1-paired-screen}`.
The archive at
`.amp/in/artifacts/parse-warnings-force-verification-20260910/evidence.tar.gz`
contains all timing and RTS logs, diagnostic stdout, summaries, paired changes,
product manifests, run metadata, the exact harness, binary identities, dense
source and manifests, package input manifests, and the audit summary. SHA-256:
`c322360672137ddff2b4c57438f889c7eba614a0e7569b636b4ebbba00bec578`.
`gzip -t` passes. Compiler binaries and the complete package-source cache are
retained separately, not embedded in the archive.

Retain this candidate for the lead's ongoing N4 confirmation and acceptance
decision: the independent dense gate passes and both N1 package runs support
lower live memory with equivalent observable products and warnings. Do not
claim an established speedup from one measured pair. No extra timings were
run, no accepted source was changed, and no rebuild or new test was performed.
The full-suite result above remains explicitly lead-reported.

The preceding varIfUnknown verification remains HOLD/unaccepted on its separate
evidence branch: its N1 screen saved 0.521472% allocation but increased wall
time by 2.922438% and RSS by 2.086286%. Both rejected visibility trials and all
prior positive and negative measurements remain intact on their own branches
and in separate archives.
