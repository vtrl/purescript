# Cumulative warning-force + specialization: independent N1 block

This binary-only worker measures the correctness-tested experimental union,
not a source-accepted compiler. It runs one fresh full-corpus N1 block and
does not rebuild, run tests, import source, change configuration, or pool old
samples. The standalone specialization branch and complete archive remain
preserved and separate.

All three fresh pairs favor C on the four requested metrics. Independent mean
within-pair changes are **−35.1418% wall time**, **−43.6402% allocation**,
**−20.6802% peak RSS**, and **−14.1143% maximum sampled residency**. All eight
runs preserve original products and warning content. Maximum minor-GC pauses
increase despite lower total GC time; this is not an all-pause improvement.
These are this block's results, not a universal, additive, or acceptance claim.

## Pinned identities and supplied correctness evidence

Report branch: `perf/cumulative-n1-xxlarge-20260910`, based exactly on pushed
campaign [8bc296a5](https://github.com/vtrl/purescript/commit/8bc296a5d4218180c078d5ff481c4c4b6cb761fe).
The tree outside `debug/` matches accepted Make
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7),
verified with `git diff --exit-code`. No moving main/master ref substitutes
for that checkpoint. This checkout is unshallowed. Origin fetch and push are
both exclusively `https://github.com/vtrl/purescript`.

Experimental source:
[d0d4b331](https://github.com/vtrl/purescript/commit/d0d4b3313f37ad9f79997eb6a852dc83aa2f0336),
tree `8fe67bd6750d99c383210f2e9a702156adda2cea`. Its diff from accepted Make is
limited to `src/Language/PureScript/Make.hs`, `tests/TestMake.hs`, `stack.yaml`,
and `cabal.project`. It is the exact union of:

- Warning forcing and asymmetric parser-warning regression, original
  [2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f),
  by Justin Garcia; cumulative cherry-pick
  [e6b308ca](https://github.com/vtrl/purescript/commit/e6b308ca2af071637104ae7583043446c021a319).
- `-fspecialize-aggressively -fexpose-all-unfoldings`, original
  [c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63),
  by seastian <seastian@users.noreply.github.com>; authoritative upstream
  [PR 4584](https://github.com/purescript/purescript/pull/4584) /
  [8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
  OxfordAbstracts PR 16 imported that work. No Claude authorship is claimed.

| Binary | Bytes | SHA256 |
| --- | ---: | --- |
| B: preserved accepted Make | 47869944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: frozen tested cumulative union | 70523064 | `4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d` |

C was downloaded from the lead's `.build/perf/purs-cumulative-tested`; both
binaries were verified and mode 0555 before timing. C embeds clean source
`d0d4b3313f37ad9f79997eb6a852dc83aa2f0336`. B has an older dirty development
version string; its verified frozen hash, not that string, is the measurement
identity. The standalone `4a918...` candidate was not used in this block.

The supplied build worker records GHC 9.6.6, Stack 3.3.1, lts-22.43, Node
22.23.2, and normal optimized flags including `-O2 -Werror
-fspecialize-aggressively -fexpose-all-unfoldings -fhide-source-paths`. It reports:

| Supplied command | Result | Wall s | Max RSS KiB |
| --- | --- | ---: | ---: |
| Optimized dependencies/library/executable/tests build, jobs=8 | exit 0 | 1268.90 | 4552768 |
| Full optimized suite, seed 9160 | 1303 examples, 0 failures | 104.92 | 692416 |
| Make subset, seed 9160, default RTS | 14 examples, 0 failures | 15.89 | 501292 |

The frozen binary was installed only after both suites; independent stripping
and post-test hashes agreed. No subsequent build ran in that worker. This
worker preserves those supplied results; neither suite ran here. The source
worker's report and full 217,437-byte provenance archive are included in this
worker's evidence; source archive SHA256:
`459d46b9cefa5a07734d3dd1dbf1fe0132e86165424b12f5268f94802e8dc160`.
The archive was retrieved from originating worker
[thread](https://ampcode.com/threads/T-01a08b9e-a9df-77b8-b32b-4f5a6d1e93cf),
not rebuilt when the same archive path was absent on the lead.

Prior independent standalone audit
[90ffc537](https://github.com/vtrl/purescript/commit/90ffc537528f88a4398b7b8a704d8d6993b00734)
passed; the lead reports replay matching all 120 standalone records. Those
records motivate this block but are not included in its sample counts or means.

## Fresh fixed-N1 protocol and observed hardware

The existing a1.xxlarge orb exposes 16 logical CPUs, Intel Xeon @ 2.60 GHz,
family 6/model 106/stepping 6, eight cores with two threads each, one socket,
KVM, and CPUs 0–15. Linux 6.1.158+ x86-64; physical RAM 32,880,800 KiB; no swap.
The actual before/after workload `memory.max` is **31,675,383,808 bytes
(29.5 GiB)**; the service's own limit is `max`. Workload/parent `cpu.max` is
`max 100000`. This observed memory limit differs from the standalone block's
30 GiB; this worker made no system-limit changes. Full before/after CPU,
cgroup hierarchy, memory/events, process list, load, and argv captures are
retained. These are the current block's observations, not reused old metadata.
The snapshots report no CPU throttling, OOM, or memory-limit events. The
service-wide cgroup peak is 2,155,503,616 bytes, including cache and other
charges; this is not GNU-time process RSS and not a per-variant peak.

The same prepared corpus was verified byte-for-byte before timing: set 60.4.0,
registry `5d834cd364da1d49a1bd1b0219ab49fb15f20601`, originally prepared with
Spago 0.93.43 using unchanged `ci/prepare-package-set-benchmark.sh`. Exactly
4084 unique PureScript sources and all 4901 input byte streams match manifest
SHA256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

Unchanged harness:
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b),
`ci/benchmark-compiler.py`, SHA256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.

```sh
python3 ci/benchmark-compiler.py \
  --corpus .build/package-set-benchmark \
  --baseline .build/perf/purs-parse-release \
  --compiler .build/perf/purs-cumulative-tested \
  --results .build/perf/cumulative-n1/runs \
  --label '<cumulative label with full pinned campaign/source/binary identities>' \
  --capabilities 1 --samples 3 --cache warm --codegen js
```

The exact label and all 4084 source arguments are in `runs/metadata.json`.
Actual B/C warmup `/proc` argv was captured and matches the expanded harness
metadata exactly, including `+RTS -N1 -s -RTS` and all source arguments.
All runs use fixed N1, cleared `GHCRTS`, `LC_ALL=C`, and `LANG=C`, with
no affinity/nursery override. Every compile starts with deleted JS output;
hashing occurs after the timed process. Two excluded clean B/C warmups precede
exactly three measured pairs in **BC / CB / BC** order. No extra screen,
warmup, capability, adaptive retry, excluded outlier, or old-sample pooling is
allowed. Downloads and all preparation finished before timing; no competing
build/test/benchmark workload ran on this orb.

The service command is `amp orb service start cumulative-n1 --command 'bash
/home/user/workspace/repo/.build/perf/cumulative-n1/run-once.sh' --cwd
/home/user/workspace/repo`, without a portal. The wrapper atomically creates
`block.started`, refuses duplicate execution, records actual exit/completion,
and holds with `exec sleep infinity` after completion or duplicate detection.
The guard and evidence are retained; it is not a retry mechanism.

## Every run, paired changes, and retained GC behavior

The block ran **2026-09-10 14:47:28–15:09:33 UTC**, harness exit **0**, with
exactly eight compiles and no interruption or restart. The preparation/start
checkpoint and the first complete-pair checkpoint were sent to the lead;
candidate-2 was already running during the latter. Completion status/logs and
post-run metadata were collected while the wrapper held in `sleep infinity`,
then the service was stopped. No other service remains running.

All GNU-time wall/user/system values below are seconds, RSS is KiB, and
allocation/residency are bytes. Rows follow execution order.

| Run | Wall s | User s | System s | Peak RSS KiB | Allocated B | Sampled residency B |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Warmup B, excluded | 196.85 | 194.20 | 3.12 | 1787060 | 397545198920 | 610206632 |
| Warmup C, excluded | 131.52 | 129.31 | 2.64 | 1518032 | 224057347360 | 581254904 |
| B1 | 196.21 | 193.82 | 2.94 | 1816692 | 397550259264 | 622251848 |
| C1 | 129.87 | 127.65 | 2.62 | 1492224 | 224055287672 | 568341160 |
| C2 | 129.35 | 127.24 | 2.45 | 1401248 | 224056199472 | 515797440 |
| B2 | 200.69 | 197.81 | 3.43 | 1775892 | 397552347144 | 612221832 |
| B3 | 203.73 | 201.48 | 2.83 | 1768568 | 397547299648 | 611977712 |
| C3 | 130.25 | 128.27 | 2.35 | 1360308 | 224063964856 | 502255320 |

Independent analysis computes \(100(C-B)/B\) within each numbered pair,
including candidate-first pair 2. It reports the mean of these three changes,
not a ratio of pooled means. Sample SD uses denominator \(n-1\), in percentage
points (pp); it is not a confidence interval. No sample or outlier is removed.

| Pair / order | Wall change % | Allocation change % | RSS change % | Sampled residency change % |
| --- | ---: | ---: | ---: | ---: |
| 1 / BC | −33.810713 | −43.641016 | −17.860375 | −8.663805 |
| 2 / CB | −35.547362 | −43.641082 | −21.096103 | −15.749911 |
| 3 / BC | −36.067344 | −43.638414 | −23.084213 | −17.929148 |

| Metric | Mean change % | Sample SD pp | Median change % | Range of changes % |
| --- | ---: | ---: | ---: | ---: |
| Wall time | −35.141806 | 1.181716 | −35.547362 | [−36.067344, −33.810713] |
| Allocation | −43.640171 | 0.001522 | −43.641016 | [−43.641082, −43.638414] |
| Peak RSS | −20.680230 | 2.636633 | −21.096103 | [−23.084213, −17.860375] |
| Maximum sampled residency | −14.114288 | 4.844388 | −15.749911 | [−17.929148, −8.663805] |

B wall time rises from 196.21 to 203.73 s; C spans 129.35–130.25 s. Memory
effects vary across the three pairs. These observations are retained rather
than corrected with additional runs. Residency is a GC-sampled maximum, not
total allocation or RSS.

The following RTS table retains copy and GC behavior in execution order.
Times and pauses are seconds. CPU-phase times, average pauses, INIT/EXIT/Total,
parallel collection counts, and residency sample counts are also preserved in
the independently parsed `raw_rts_details` JSON; full RTS output remains in
every `.stderr` file.

| Run | Copied B | MUT elapsed s | GC elapsed s | Gen0 collections | Gen1 collections | Gen0 max pause s | Gen1 max pause s |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Warmup B, excluded | 63214614304 | 117.628 | 79.169 | 94341 | 61 | 0.0086 | 0.5145 |
| Warmup C, excluded | 55744925472 | 75.980 | 55.499 | 52687 | 69 | 0.0522 | 0.4357 |
| B1 | 62468961768 | 117.491 | 78.664 | 94343 | 60 | 0.0106 | 0.5394 |
| C1 | 56024562720 | 75.251 | 54.572 | 52688 | 70 | 0.0518 | 0.3690 |
| C2 | 54886233648 | 75.279 | 54.027 | 52689 | 68 | 0.0542 | 0.3263 |
| B2 | 63349162208 | 120.206 | 80.435 | 94346 | 60 | 0.0085 | 0.5312 |
| B3 | 62540485696 | 122.437 | 81.239 | 94343 | 60 | 0.0092 | 0.5198 |
| C3 | 55160203624 | 75.579 | 54.627 | 52689 | 68 | 0.0542 | 0.3483 |

Measured maximum Gen0 pauses rise from B **8.5–10.6 ms** to C **51.8–54.2 ms**,
while maximum Gen1 pauses fall from B **519.8–539.4 ms** to C **326.3–369.0 ms**.
Lower total wall/GC time does not imply improvement in every pause metric.

## Correctness checks and complete transferable evidence

`python3 .build/perf/cumulative-n1/analyze.py` independently validates exactly
eight completed rows, matching each raw `.time` and parsed RTS allocation and
residency value. It checks observed B/C argv against metadata, explicit N1,
both frozen identities, the prescribed order/count, and computes pair effects
independently. Its paired changes agree with the unchanged harness within
1e-10 percentage points. The original products and warnings match for all eight
runs, including both excluded warmups.

The original map contains **8985 products**: 4084 `index.js`, 817 `foreign.js`,
and 4084 `externs.cbor`. Shared product-manifest SHA256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
The unchanged harness hashes each run's products outside timing and asserts
equality with the first successful map before emitting that run's row. It keeps
one shared manifest, not eight independent copies. This worker independently
compares that shared map to the original and directly rehashes the retained
final output tree. Only the final actual tree was rehashed independently;
earlier per-run equality is supported by the harness assertions and raw rows.

Each run matches the complete original **745-warning Counter / 738 distinct
bodies**, retaining duplicate counts and internal whitespace. Only numbered
`Warning N of M:` headers and outer body whitespace are removed. Original raw
stdout SHA256 is `6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280`;
canonical sorted body-to-count JSON (UTF-8, `ensure_ascii=False`, indent 2,
final newline) SHA256 is
`21c39f0ba7c0a8370f1027327ad0c2559cd688dde940896e02c18b8f7626f436`.
The raw warning text for every run and original reference are retained.

All 4901 input byte streams, both binaries, their 0555 modes/sizes, the harness,
and the standalone archive were rechecked after timing. The cumulative source
diff SHA256 `e1975195ba218be2fd423a9f32d0099950581cb11a95e9d5941146f81a4bd677`
also matches the supplied union provenance. No compiler source/test/configuration
or harness change is made by this report-only branch.

Complete archive: `.amp/in/artifacts/cumulative-n1-xxlarge-20260910.tar.gz`, with
adjacent `.tar.gz.sha256`. Delivery includes its exact bytes and SHA256. It
contains both measured binaries, all raw run outputs/timings/RTS/metadata,
original product/warning references, shared and independently rehashed product
maps, per-run raw checksums, independent statistics and detailed RTS parse,
observed argv, before/after hardware and identity checks, guarded wrapper and
service records, supplied build/test provenance, exact harness, full prepared
corpus, retained final output tree, report, and report commit patch. A separate
per-file checksum manifest covers all cumulative evidence files.

Audit offline after extracting into an empty directory with
`python3 .build/perf/cumulative-n1/analyze.py`; no compilation, test suite, or
fetch is needed. The archive preserves `.build/` paths and the started guard.
Do not remove the guard or invoke the measurement wrapper to audit results.

## Interpretation limits and preserved standalone work

The cumulative executable is 22,653,120 bytes / **47.3222% larger** than B.
The supplied build includes uncached dependency work and test compilation;
there is no matched baseline build-cost comparison. Do not interpret 1268.90 s
as a measured relative build penalty or claim this worker rebuilt either binary.

This fixed-N1 full-corpus block does not establish universal or additive
speed/memory benefits. The complete synchronized capability/workload matrix
and independent cumulative audit remain separate lead-owned requirements.
No claim about cold-cache, incremental, IDE, startup, tiny/dense workloads,
other capabilities, or source acceptance follows from this block alone.

The preserved standalone branch remains at
[1467ecd5](https://github.com/vtrl/purescript/commit/1467ecd5e669743a7b76daa9262ec29210a4a024).
Its 65,425,517-byte archive remains unchanged at
`.amp/in/artifacts/specialization-n1-xxlarge-20260910.tar.gz`, SHA256
`71f428a414ed9756d5c00b56070ffaddc6d78fb2b3a98d63e29f8cf3386894f4`.
