# varIfUnknown final repeated N1 validation — 2026-09-10

Recommendation: **REJECT for integration in this campaign; retain the evidence.**
The candidate saves 2,069,962,740.8 allocated bytes per compile on average
(2.070 GB, -0.520472%), but the five-pair N1 block is slower: paired wall change
**+1.688892% ± 1.169347 percentage points sample SD**, with four of five pairs
slower. Neither peak RSS nor maximum residency demonstrates a memory benefit.
This is a performance-tradeoff rejection, not a correctness failure or a claim
that every workload will regress. The candidate remains unaccepted; this report
does not integrate source. No further repetitions or capability expansion ran.

## Scope and exact identities

The report-only branch `perf/varifunknown-n1-final-20260910` starts at exact
campaign commit [3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d).
Compiler source remains accepted Make
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The checkout was unshallowed before history inspection. Origin fetch and push
both resolve exclusively to `https://github.com/vtrl/purescript`. Only this
report branch is published; no master, upstream, source, tests, configuration,
or harness changes are included.

Candidate source is
[7b69e68a](https://github.com/vtrl/purescript/commit/7b69e68aadada69371190b735dcd22ac205d42e3):
snapshot `substNames` once in `varIfUnknown`, then use the existing pure
traversal. The source worker supplied three focused tests (3/0) and the full
suite (1,305/0). Those tests were not rerun here. No external-fork provenance is
asserted. Pending parser-warning, specialization, leaf, and visibility changes
were not imported.

Both exact normal O2 GHC 9.6.6 / Stack 3.3.1 / lts-22.43 binaries were downloaded
without rebuilding, kept mode 0555, and rehashed after the block. Stale embedded
version strings do not identify the experiment; the following hashes do.

| Identity | SHA-256 |
| --- | --- |
| Make baseline B | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| varIfUnknown candidate C | `604b26f6e2532804378a6717140150b470bccea148c47ccad23f8dd6b23ce8e2` |
| Unchanged e926183 harness | `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399` |
| Corpus `inputs.json` | `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| Original 8,985-product manifest | `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed` |

Baseline came from lead thread
[T-01a08a95](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c),
`.build/perf/purs-parse-release`; candidate came from source worker
[T-01a08b0b](https://ampcode.com/threads/T-01a08b0b-2da5-711e-b078-b5fb012bdc11),
`.build/perf/purs-varifunknown-tested-release`. Local copies remain at those
same workspace-relative paths.

## This orb and the prespecified block

Requested new worker size: a1.xxlarge. Observed Linux 6.1.158+ x86_64 orb:
16 logical CPUs, Intel Xeon @ 2.60 GHz, family 6/model 106/stepping 6, 8 cores,
2 threads/core, one socket, KVM. `/proc/meminfo` reports MemTotal 32,880,800 KiB
and zero swap. Effective cpuset is 0–15; workload `cpu.max` is `max 100000`.
The workload cgroup has `memory.max=32212254720` bytes (30 GiB) and
`memory.high=31675383808` bytes (29.5 GiB); its parent and service have no tighter
memory limits. The service runs under
`/amp.slice/amp-workload.slice/amp-svc-varifunknown-n1-final-20260910.service`.
Full CPU, memory, cgroup, and pre-timing process snapshots are archived. Final
counters show zero CPU throttling, OOM kills, or memory high/max events.

Every compile used exactly **one GHC capability**, independently confirmed in
all 12 RTS logs. No direct absolute-time comparison with older smaller orbs is
made. No builds, tests, downloads, dependency work, or other compiler workloads
ran concurrently. Only brief status/sample-file inspections occurred during
timing; the full audit and archive construction followed completion.

`ci/prepare-package-set-benchmark.sh` prepared all 506 packages using Spago
0.93.43, package set 60.4.0, and registry revision
`5d834cd364da1d49a1bd1b0219ab49fb15f20601`. The expected 4,084 sources / 4,901
inputs and input hash matched before timing; every input content was rehashed
and matched after timing. Spago ran under Node 22.23.2. No compiler was rebuilt.

```sh
repo=/home/user/workspace/repo
block="$repo/.build/perf/varifunknown-n1-final-20260910"
python3 "$repo/ci/benchmark-compiler.py" \
  --corpus "$block/corpus" \
  --baseline "$repo/.build/perf/purs-parse-release" \
  --compiler "$repo/.build/perf/purs-varifunknown-tested-release" \
  --results "$block/results" \
  --label Make38080a40-vs-varIfUnknown7b69e68-binary-only-final-N1-five-pairs \
  --capabilities 1 --samples 5 --cache warm --codegen js
```

The exact command ran once in the unchanged harness through an ignored
`run-once.sh` wrapper and `amp orb service start`, without a portal. The wrapper
atomically created `started/`, guards against any second invocation, recorded
exit status, and executed `sleep infinity` pending collection. The service began at
13:20:07 UTC and completed at 14:09:54 UTC with **exit status 0 / zero restarts**.
It was stopped only after completed results and service evidence were collected.

One full clean warmup per binary was excluded. Measured order was exactly
BC/CB/BC/CB/BC. Warm page cache was retained, JS-only code generation selected,
`GHCRTS` cleared, and `+RTS -N1 -s -RTS` passed explicitly. The harness deleted
the output directory before every compile, timed the compiler alone with
`/usr/bin/time`, and hashed products outside timing. All samples, including the
slowest candidate run at 257.52 s, are retained. No adaptive reruns occurred.

## Every individual run is retained

Warmups are shown for completeness but excluded from all measured statistics.

| Run | Wall s | Peak RSS KiB | Allocated bytes | Maximum residency bytes |
| --- | ---: | ---: | ---: | ---: |
| Warmup B | 242.29 | 1,861,812 | 397,707,379,920 | 642,164,024 |
| Warmup C | 251.17 | 1,830,224 | 395,639,863,416 | 631,203,000 |
| B1 | 250.29 | 1,820,860 | 397,711,057,544 | 641,997,280 |
| C1 | 250.07 | 1,844,588 | 395,636,806,840 | 620,964,288 |
| C2 | 246.29 | 1,820,020 | 395,637,098,464 | 638,565,456 |
| B2 | 240.33 | 1,824,116 | 397,710,576,960 | 624,669,000 |
| B3 | 241.34 | 1,845,680 | 397,708,215,416 | 627,555,960 |
| C3 | 244.06 | 1,844,344 | 395,641,690,592 | 629,087,776 |
| C4 | 253.05 | 1,798,324 | 395,643,385,640 | 623,913,392 |
| B4 | 247.68 | 1,845,612 | 397,707,783,344 | 620,584,416 |
| B5 | 250.61 | 1,815,984 | 397,705,752,672 | 614,104,936 |
| C5 | 257.52 | 1,865,076 | 395,634,590,696 | 639,773,392 |

| Metric | Baseline mean | Baseline median | Baseline min–max | Candidate mean | Candidate median | Candidate min–max |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Wall s | 246.050 | 247.68 | 240.33–250.61 | 250.198 | 250.07 | 244.06–257.52 |
| RSS KiB | 1,830,450.4 | 1,824,116 | 1,815,984–1,845,680 | 1,834,470.4 | 1,844,344 | 1,798,324–1,865,076 |
| Allocation bytes | 397,708,677,187.2 | 397,708,215,416 | 397,705,752,672–397,711,057,544 | 395,638,714,446.4 | 395,637,098,464 | 395,634,590,696–395,643,385,640 |
| Residency bytes | 625,782,318.4 | 624,669,000 | 614,104,936–641,997,280 | 630,460,860.8 | 629,087,776 | 620,964,288–639,773,392 |

Changes are computed within each numbered pair as \(100(C - B)/B\) before
summarizing. SD below is **sample SD in percentage points**, not a confidence
interval. Positive means the candidate consumes more of the measured resource.

| Pair | Order | Wall % | RSS % | Allocation % | Residency % |
| --- | --- | ---: | ---: | ---: | ---: |
| 1 | BC | -0.087898 | +1.303121 | -0.521547 | -3.276181 |
| 2 | CB | +2.479923 | -0.224547 | -0.521354 | +2.224611 |
| 3 | BC | +1.127041 | -0.072385 | -0.519608 | +0.244092 |
| 4 | CB | +2.168120 | -2.562185 | -0.519074 | +0.536426 |
| 5 | BC | +2.757272 | +2.703328 | -0.520777 | +4.179816 |
| Mean | | +1.688892 | +0.229466 | -0.520472 | +0.781753 |
| Sample SD (pp) | | 1.169347 | 1.959801 | 0.001087 | 2.758839 |
| Median | | +2.168120 | -0.072385 | -0.520777 | +0.536426 |
| Min–max | | -0.087898–+2.757272 | -2.562185–+2.703328 | -0.521547–-0.519074 | -3.276181–+4.179816 |

Mean wall difference is +4.148 s. Allocation savings are consistent, but
allocation volume is not peak memory and does not establish a speedup. This
bounded, single-orb confirmation provides adverse N1 timing evidence. It is
not a universal regression claim, and memory differences remain noisy.

## Original-product and warning-content equality passed

The original manifest and original warning transcript were downloaded directly
from the lead at `.build/perf/baseline-n1-warm/products.json` and `1.stdout`.
All 12 runs passed the unchanged harness's per-run product comparison. Its
common `products.json` is byte-identical to the original 8,985-product manifest;
the final product files were independently rehashed against it after timing.
Thus the equality claim is against the original, not merely within this block.

Every stdout contains exactly 745 warning bodies / 738 distinct. The audit
removes only `Warning N of M` header lines and outer body whitespace, then
compares byte-string Counters against the original transcript, preserving all
duplicate multiplicities and internal whitespace. Every run matches exactly.
Original transcript SHA-256:
`6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280`.
This archive's canonical multiset SHA-256:
`89db272d1c56d27e006fb036ff348e0522e1b598115dcd5faf5af2fcb7281f16`.
Its serialization is sorted UTF-8 body/count pairs, JSON `ensure_ascii=False`,
`indent=2`, plus a final newline; this serialization-specific hash is not used
as a substitute for direct body/multiplicity comparison.

`verify.py` also checks run order, count, excluded warmups, all raw time/RTS
statistics, fixed N1, and independently recomputes means, medians, ranges and
sample SD. Its decisive output is:

```text
PASS: 12/12 runs at -N1; original 8985 products; original 745 warnings / 738 distinct; exact body/multiplicity equality.
PASS: all raw timings/RTS statistics and independently recomputed means, medians, ranges and sample SD agree.
```

## Earlier selection evidence remains separate

The earlier independent report
[c09d89cb](https://github.com/vtrl/purescript/commit/c09d89cbbcc5e7719d7bcdcd32a19ef94e2c7a4e)
recorded a dense-1000 five-pair gate with -0.418739% allocation and effectively
unchanged memory, and a single full-corpus N1 screen B243.29/C250.40 s
(+2.922%), RSS +2.086%, allocation -0.521472%. That selection screen established
neither a repeatable regression nor a gain and left the candidate on HOLD.
The source worker's four-pair N4 confirmation saved about 2.063 GB allocation
(-0.51894%), but wall -0.406% ± 2.695 pp and RSS -1.718% ± 3.014 pp did not
establish a speed/memory win. None of those samples is pooled into this report's
new block, and older absolute times are not compared across orb sizes.

The new final N1 repetition supports rejecting the integration tradeoff while
retaining all evidence. Final campaign disposition remains with the lead.

## Complete raw evidence and transfer

Worker thread:
[final N1 validation](https://ampcode.com/threads/T-01a08b70-40ba-7372-a59e-f96deed4fdca).
Raw workspace directory: `.build/perf/varifunknown-n1-final-20260910`.
Transfer path: `.amp/in/artifacts/varifunknown-n1-final-20260910/raw.tar.gz`.
Archive SHA-256:
`3677a5648836ba7836d2590ead5f52e410aebf05c1fb9312e3166c74a696100c`.

The archive contains every warmup/measured stdout, stderr and time file;
metadata, full command, samples, paired changes, summaries, products; original
reference files; warning Counter serialization, audit script and outputs;
before/after identities and machine metadata; preparation logs, package-set
and input manifests, Spago configuration/lock/source lists; unchanged harness
and preparation script; run-once wrapper, guard timestamp, completion status,
and collected/stopped service evidence. Copied binaries, fetched package source
trees and generated output trees are not duplicated inside the archive; their
identities/manifests are included and local copies remain available.

First-pair and final checkpoints were sent to the lead. Only this Markdown
report is a tracked change; accepted compiler/source/test/configuration files
are unchanged. No new thread, source variant, N4/N8 block, or integration was
created by this worker.
