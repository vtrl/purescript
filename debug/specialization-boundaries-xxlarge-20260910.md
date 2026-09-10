# Specialization flags: independent N8 and small-input boundaries

The exact final-tested specialization binary was faster in every measured
pair here, with large allocation reductions, but **higher peak RSS in every
pair** and a 47.322% larger executable. Full N8 wall time improved 29.727%
on average; Dense1000 and tiny checks found no wall-time regression in these
fixtures. All 96 compiles passed their required product/diagnostic checks.

Report-only branch: `perf/specialization-boundaries-xxlarge-20260910` in
`vtrl/purescript`. This worker performed binary-only validation; it made no
compiler, test, configuration, or harness changes and built no compiler.

## Identity and provenance

The checkout was pinned to accepted campaign
[3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d).
Its `src`, `tests`, `stack.yaml`, `cabal.project`, and `purescript.cabal` are
identical to accepted Make lifetime release
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The baseline was not advanced. The checkout was unshallowed before inspecting
history. The sole remote's fetch and push URLs were both
`https://github.com/vtrl/purescript`; no master/upstream integration was done.

The candidate source/config commit is
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63),
verified by fetching `perf/specialization-flags-20260910` at report checkpoint
[5cffa728](https://github.com/vtrl/purescript/commit/5cffa728b8cd7dfc3c0e4e87091c66f486dce733).
Relative to this worker's pinned checkout, only two build configuration files
differ: Stack's local GHC options add `-fspecialize-aggressively` and
`-fexpose-all-unfoldings`, and `cabal.project` sets the same options for package
`purescript`. The trial includes no parser-warning force, traversal
`INLINABLE`, binding-visibility, CST, or unifier changes.

The original author is **seastian <seastian@users.noreply.github.com>**.
Authoritative provenance is
[purescript/purescript PR 4584](https://github.com/purescript/purescript/pull/4584),
upstream commit
[8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
[OxfordAbstracts PR 16](https://github.com/OxfordAbstracts/purescript/pull/16)
imported that work; it was not the original implementation. There is no
documented Claude attribution.

The two downloaded executables were preserved read-only (mode 0555):

| Binary | Bytes | SHA256 |
| --- | ---: | --- |
| B: lead `.build/perf/purs-parse-release`, local `purs-baseline` | 47,869,944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: specialization worker's final tested `.build/perf/purs-specialization` | 70,523,064 | `4a918e8fe3b147cf1bd24adf26cc8286d3d72dbc0f5dbe928aa563166fd773c1` |

The initial-build `c790` executable was never used. B's embedded version is
`0.15.15 [development build; commit: 6d636561f72b7985962e2931e462aa017a38f7fc DIRTY]`;
its assigned hash identifies the accepted binary, not that embedded version
string alone. C's version embeds the complete candidate source commit above.
Both are the supplied normal optimized GHC 9.6.6 / Stack 3.3.1 / lts-22.43
executables, not profiling or `--fast` builds.

## Machine, fixtures, and method

This independent `a1.xxlarge` orb reports 16 logical CPUs (8 cores × 2 SMT),
Intel Xeon @ 2.60 GHz, family 6/model 106/stepping 6, x86-64 KVM, Linux
6.1.158+. Physical memory is 33,669,939,200 bytes. The workload cgroup limit
was 32,212,254,720 bytes (30 GiB) in the preflight snapshot and
31,675,383,808 bytes (29.5 GiB) in the final snapshot; the transition time
was not sampled, so a constant per-run limit is not claimed. Both snapshots
are retained. Swap is zero and `cpu.max` is `max 100000` (no explicit CPU
quota); the final CPU snapshot records zero throttled periods/usec. CPU
affinity is 0–15. Full CPU, memory, cgroup, process, and kernel snapshots are
in the raw evidence. No other benchmark or build ran concurrently. This
worker created no threads.

All downloads, tool installation, and fixture preparation finished before
timing. The unchanged `ci/prepare-package-set-benchmark.sh` used Spago
0.93.43, set 60.4.0, registry commit
`5d834cd364da1d49a1bd1b0219ab49fb15f20601`:

| Fixture | Sources / inputs | Identity |
| --- | ---: | --- |
| Full pinned set | 4084 / 4901 | `inputs.json`: `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| Dense1000 | 1 / 1 | `Dense.purs`: `408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7` |
| Tiny Sequence | 57 / 78 | `inputs.json`: `3736e1f30a4ab6ceb49ed90349358f81e3ef75b17a0fcbf8c3253e8efb92d4cb` |

Dense1000 was extracted from the lead's `binding-dense-regression.tar.gz`
into a new ignored local directory. Both binaries compiled the same source
path. Its cyclic functions were **never executed**. Tiny uses the lead's
`make-tiny-evidence.tar.gz` manifests and Main verbatim, with prelude 6.0.1,
effect 4.0.0, and console 6.1.0 copied from the full pinned corpus; every input
hash was checked. No fixture was changed.

The unchanged e926183 harness `ci/benchmark-compiler.py` has SHA256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
The prescribed sequential blocks were full N8 (3 pairs), Dense1000 N1
(10 pairs), then tiny N1, N4, and N8 (10 pairs each). Each block has one
excluded clean warmup per binary, then BC/CB alternating order beginning BC.
There are no extra repetitions, source variants, adaptive sample counts, or
discarded measured samples.

Every measured command directly invokes the relevant binary with `compile`,
the full `purs-files.json` source list, the same fixture-local `--output`
path, `--codegen js`, and `+RTS -N1 -s -RTS`, `+RTS -N4 -s -RTS`, or
`+RTS -N8 -s -RTS`. Never bare `-N`. Output is deleted before every compile;
warm cache means OS filesystem cache, not incremental compilation. The
harness clears `GHCRTS` and fixes `LC_ALL=C` / `LANG=C`. Metadata records
the entire argv, not a shell glob. GNU time wall/user/system seconds and
peak RSS, RTS allocation and maximum sampled residency, monotonic elapsed
time, stdout, and stderr are preserved for every run.

Each long block ran through `amp orb service start ... --command
'bash /absolute/ignored/run-once.sh' --cwd /home/user/workspace/repo` without
a portal. An atomic `mkdir` started-directory prevents reexecution; actual
exit status goes to a completion file, followed by `exec sleep infinity` on
completion or duplicate detection. Services were stopped only after results
were collected. The full N8 checkpoint was sent before small-input timing.

## Results

All three managed blocks exited 0 with no interruption or retry. UTC windows:
full N8 13:32:27–13:40:46; Dense1000 N1 13:42:24–13:42:39; tiny N1/N4/N8
13:43:02–13:43:52, all on 2026-09-10. All services were stopped after result
collection. There were 43 measured pairs (86 compiles) and 10 excluded
warmups, totaling 96 compiles.

### Paired changes: independent calculation from raw time/RTS files

For each metric and numbered pair, change is
\(100(C_i-B_i)/B_i\). Means, medians, ranges, and sample standard deviations
are calculated over those paired percentages, not from a ratio of aggregate
means. SD below is in **percentage points**, with denominator \(n-1\).
Negative values mean less time/memory/allocation. These calculations also
match the unchanged harness summaries within 1e-10.

| Block | Metric | Mean change | Sample SD (pp) | Median change | Range |
| --- | --- | ---: | ---: | ---: | --- |
| Full N8, 3 pairs | Wall | -29.7273% | 1.1681 | -29.3404% | -31.0397% … -28.8017% |
| Full N8 | Allocation | -43.6308% | 0.0018 | -43.6314% | -43.6323% … -43.6288% |
| Full N8 | Peak RSS | +10.8607% | 4.3265 | +9.9362% | +7.0711% … +15.5747% |
| Full N8 | Sampled residency | +15.8207% | 8.1627 | +17.3964% | +6.9851% … +23.0807% |
| Dense1000 N1, 10 pairs | Wall | -20.6054% | 6.5642 | -20.5247% | -30.7692% … -9.2308% |
| Dense1000 N1 | Allocation | -43.7412% | 0.0010 | -43.7415% | -43.7415% … -43.7384% |
| Dense1000 N1 | Peak RSS | +15.8146% | 0.0495 | +15.8123% | +15.7408% … +15.9008% |
| Dense1000 N1 | Sampled residency | +15.4928% | 0.0000 | +15.4928% | +15.4928% … +15.4928% |
| Tiny N1, 10 pairs | Wall | -39.7901% | 5.1920 | -38.5282% | -49.2647% … -32.7273% |
| Tiny N1 | Allocation | -49.6741% | 0.0001 | -49.6741% | -49.6743% … -49.6739% |
| Tiny N1 | Peak RSS | +8.5749% | 2.1832 | +7.8169% | +6.0437% … +13.7320% |
| Tiny N1 | Sampled residency | -1.8738% | 11.8738 | -2.1535% | -16.1590% … +24.5622% |
| Tiny N4, 10 pairs | Wall | -43.5006% | 2.6267 | -43.9529% | -47.5000% … -39.1892% |
| Tiny N4 | Allocation | -49.5782% | 0.0276 | -49.5766% | -49.6272% … -49.5427% |
| Tiny N4 | Peak RSS | +9.2463% | 2.4267 | +10.4687% | +3.9532% … +11.5480% |
| Tiny N4 | Sampled residency | +3.9849% | 8.6920 | +1.9019% | -6.7579% … +18.5536% |
| Tiny N8, 10 pairs | Wall | -42.0528% | 1.8028 | -41.1729% | -45.2055% … -40.5405% |
| Tiny N8 | Allocation | -49.6178% | 0.0425 | -49.6304% | -49.6739% … -49.5603% |
| Tiny N8 | Peak RSS | +6.1295% | 1.8225 | +5.8373% | +3.8146% … +9.8051% |
| Tiny N8 | Sampled residency | +0.4521% | 9.0110 | -1.1748% | -10.5383% … +14.4920% |

### Absolute wall times and excluded warmups

Seconds; distribution summaries use measured samples only. The raw archive
also contains absolute per-binary mean/SD/median/range for every other metric.

| Block | B mean / sample SD | C mean / sample SD | B median [range] | C median [range] | Excluded warmups B / C |
| --- | --- | --- | --- | --- | --- |
| Full N8 | 71.860 / 0.59925 | 50.49333 / 0.42360 | 71.71 [71.35, 72.52] | 50.67 [50.01, 50.80] | 74.76 / 50.60 |
| Dense1000 N1 | 0.732 / 0.05432 | 0.579 / 0.03755 | 0.740 [0.65, 0.82] | 0.575 [0.53, 0.66] | 0.86 / 0.60 |
| Tiny N1 | 1.215 / 0.09992 | 0.728 / 0.04517 | 1.240 [1.08, 1.36] | 0.735 [0.66, 0.79] | 1.35 / 0.69 |
| Tiny N4 | 0.783 / 0.02359 | 0.442 / 0.01476 | 0.795 [0.74, 0.81] | 0.445 [0.42, 0.47] | 0.75 / 0.45 |
| Tiny N8 | 0.754 / 0.04300 | 0.437 / 0.02983 | 0.745 [0.69, 0.83] | 0.435 [0.40, 0.49] | 0.76 / 0.44 |

### Every measured wall sample

Pairs with odd numbers ran BC; even numbers ran CB. Each cell is B → C
seconds; no sample is omitted. Exact wall/user/system/RSS/allocation/residency
rows, with warmups explicitly marked, are also in `boundaries/raw-samples.csv`
in the archive. The raw harness files retain monotonic time as well.

| Pair | Full N8 | Dense1000 N1 | Tiny N1 | Tiny N4 | Tiny N8 |
| ---: | --- | --- | --- | --- | --- |
| 1 | 71.35 → 50.80 | 0.82 → 0.66 | 1.31 → 0.68 | 0.80 → 0.44 | 0.80 → 0.47 |
| 2 | 71.71 → 50.67 | 0.74 → 0.53 | 1.26 → 0.76 | 0.80 → 0.42 | 0.83 → 0.49 |
| 3 | 72.52 → 50.01 | 0.71 → 0.55 | 1.17 → 0.73 | 0.80 → 0.45 | 0.78 → 0.43 |
| 4 | — | 0.78 → 0.54 | 1.09 → 0.66 | 0.80 → 0.43 | 0.74 → 0.44 |
| 5 | — | 0.77 → 0.61 | 1.30 → 0.77 | 0.76 → 0.43 | 0.73 → 0.43 |
| 6 | — | 0.65 → 0.59 | 1.23 → 0.77 | 0.81 → 0.45 | 0.69 → 0.41 |
| 7 | — | 0.69 → 0.58 | 1.08 → 0.69 | 0.74 → 0.45 | 0.73 → 0.40 |
| 8 | — | 0.74 → 0.59 | 1.25 → 0.79 | 0.76 → 0.45 | 0.71 → 0.40 |
| 9 | — | 0.66 → 0.57 | 1.36 → 0.69 | 0.77 → 0.43 | 0.75 → 0.44 |
| 10 | — | 0.76 → 0.57 | 1.10 → 0.74 | 0.79 → 0.47 | 0.78 → 0.46 |

### Exact correctness comparisons

- **Full N8: 8/8 compiles** matched all 8,985 original product path/SHA256
  entries, including JS, FFI, and externs. Every stdout matched the original
  745-warning multiset, with 738 distinct bodies.
- **Dense1000 N1: 22/22 compiles** matched both local products and compiler
  diagnostics (no warnings). Cross-orb dense externs embed absolute paths,
  so this deliberately compares local B/C at the same source path, not the
  lead's externs bytes. Local `Dense/index.js` SHA256 is
  `079232515f42ec7e346d13a33a75736252964eccdebc46e8f18af72d01db0103`;
  `Dense/externs.cbor` is
  `0540fdde4c08e732018cc7faa5fdb29b551fe836930fe3c172a57c67c3b710f2`.
- **Tiny N1/N4/N8: 66/66 compiles** matched all 135 original product entries
  and the exact three warning bodies, including every excluded warmup.

The unchanged harness hashes products after each compile, outside timing,
and aborts on any mismatch. Completion plus exact run counts verifies all
runs, not merely the final B/C outputs. The independent analysis reparses
every raw `.time`/RTS record and checks it against `samples.json` by run name;
it does not derive expected values from harness summaries. Warning comparison
removes only ordinal headers and outer whitespace, retains internal content
and multiplicity, and ignores order. Mutation checks confirmed that warning
reordering is accepted while duplicate loss and internal whitespace/content
changes are rejected. Dense stderr diagnostics before RTS statistics were
also compared exactly, after outer whitespace only.

Original reference SHA256 identities:

| Reference | SHA256 |
| --- | --- |
| Lead full `products.json` | `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed` |
| Lead full `1.stdout` | `6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280` |
| Tiny `original-products.json` | `6e3f9b11fad45ffa3655f5826ce9752c3f08dda6b0dd9526bf1937511a385da4` |
| Tiny reference `n1/baseline-1.stdout` | `cfa7c6f6dd1dcc259e9bbc94a9a115d07633dfb7e7b93ebfbc079f4216260491` |

## Interpretation limits and prior evidence

This report keeps the earlier N4 one-pair screen separate: 138.37 → 93.17 s
(-32.666%), allocation -43.617%, RSS +3.0695%, sampled residency +9.2879%.
Its excluded warmups were 119.35 / 85.69 s and showed drift. Those are not
samples from this worker and are not pooled with its measurements; no wall
comparison across different orb sizes is valid. Other workers own N4
five-pair confirmation and full-corpus N1 three-pair confirmation.

The original specialization worker reported the seed-9160 normal optimized
suite at 1302 examples / 0 failures, with no golden updates. This worker did
not rerun that suite or rebuild. That worker also reported a 768.70 s clean
build, peak build RSS 4,152,924 KiB, and a separate test-enabled rebuild.
These are build-cost context, not this worker's benchmark measurements.

C adds 22,653,120 executable bytes (+47.322%); size is not resident-memory
usage. Clean warm-cache tiny compilations include process startup and fixed
compilation overhead, but do not isolate startup or cold executable paging.
Tiny wall times are short and quantized by GNU time to hundredths of a
second. Lack of a statistically persuasive change does not establish
equivalence. Sampled residency is GC-schedule-sensitive and is not peak RSS.

These are single-orb point estimates, not a universal speedup percentage.
The full N8 confirmation has only three pairs; shared-host drift, different
workloads, and different capability settings can change the measured effect.
The 43.6% full-corpus allocation reduction is substantially steadier than
wall time or sampled residency, but does not imply lower live-memory demand.

The independent evidence applies only to these two exact binaries. No
flags-plus-warning-force or other combined candidate was built or tested.
Final cumulative validation remains pending a separately supplied accepted
synchronized combined binary from the lead.

## Transferable evidence

Archives are in this worker's
[Amp thread](https://ampcode.com/threads/T-01a08b7b-01a8-7007-b2c0-01d41c78f59d),
under `.amp/in/artifacts/specialization-boundaries-20260910/`:

| Archive | Bytes | SHA256 |
| --- | ---: | --- |
| `raw-evidence.tar.gz` | 2,020,530 | `dc949a495ae689ff37fd6c2c2d0603603a103133882a42ce7db0e2d74af48d00` |
| `verified-products.tar.gz` | 19,111,996 | `51d6c43c261fe7e6c249e8c5863cec4c0405ff1e438a1eae33883203dda089a6` |

`SHA256SUMS` beside the archives checks both downloads. Each archive also
contains its own per-file `SHA256SUMS`. The raw archive has 444 data files:
all 96 sets of stdout/stderr/time records, summaries and product maps,
independent analysis, original references, exact argv and machine/cgroup/binary/source
identities, run-once scripts/guards/completion files, the unchanged harness and
preparation script, full-corpus manifests, and exact Dense1000/tiny inputs.
It excludes compiler executables, dependency/build caches, and unrelated
prior measurements. The products archive contains 9,122 final verified
products (8,985 full + 2 dense + 135 tiny); cache metadata is excluded.

Per-run product trees are not retained by the unchanged clean-compile
harness: they are hashed after each run, compared exactly, and deleted before
the next. The retained final products match all earlier product hashes.
Original raw diagnostics and timing files remain available for every run.

To inspect without executing a compiler, extract the raw archive into a new
directory and run `python3 .build/perf/boundaries/analyze.py` there. It verifies
all raw records, original product maps, and warning multisets. Fresh-extraction
reanalysis passed for all five blocks / 43 pairs / 96 compiles, and all 444
raw-file plus 9,122 product-file archive hashes were independently checked.
The archived run-once scripts preserve the actual commands and must not be
reused to add campaign repetitions. Compiler binaries remain read-only in
the worker orb for any explicitly supplied later cumulative validation.
