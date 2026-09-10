# Parser-warning forcing: fresh five-pair N4 confirmation

**Recommendation: retain the isolated candidate as a memory optimization.**
All five pairs reduce peak RSS and maximum sampled residency. Mean paired RSS
change is **−24.754545%** (sample SD **3.619798 percentage points**); residency is
**−23.723650%** (SD **3.899471 pp**). Wall time averages **−1.326541%**
(SD **1.992444 pp**), but the candidate is slower in pairs 4 and 5. This is not
evidence of a consistent or universal speedup. Allocation is effectively
unchanged at this workload's scale, not literally identical: **+0.001672%**
(SD **0.004920 pp**).

This is one fresh, prespecified experiment, not a completion of the lead's
interrupted block. Neither the earlier N4 screen nor its three surviving full
pairs and partial run contributes any sample here. Those remain preserved with
the lead. Earlier N1/N8/dense/tiny findings are separate evidence; the reported
mixed tiny-workload timings still preclude equivalence or universal-speed claims.

## Pinned scope and execution

- Repository fetch and push URL verified exclusively as
  `https://github.com/vtrl/purescript`; the checkout was unshallowed before history
  inspection, then fetched.
- Report-only branch: `perf/parser-warning-n4-xxlarge-20260910`, created at exact
  [3f57015676d590bc55758cac58b65a16ee15b59d](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d).
  Compiler source/build-configuration comparison against accepted Make-only
  [38080a40fc3a53de813a0ac46187e8c709dcbcc7](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7)
  was empty. Campaign HEAD equaled the report base at the initial fetch.
- Candidate source:
  [2d5cc3bb3b766551e0b6c341dc84fc760915298f](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f).
  Its only functional compiler change forces `CST.toMultipleWarnings fp pwarnings`
  with `evaluate . force` before `tell`, inside the existing dependency/semaphore
  admission in `Make.hs`. The source/test companion patch was downloaded but
  never applied. No other optimization is in this comparison.
- Worker: [fresh confirmation thread](https://ampcode.com/threads/T-01a08b75-7528-73ec-a04d-cb025d6b814e),
  assigned `a1.xxlarge`. No further threads or orbs were created.
- Actual machine: Linux 6.1.158+, x86-64, KVM, Intel Xeon @ 2.60 GHz,
  family 6/model 106; 16 logical CPUs (8 cores × 2 SMT), CPUs 0–15.
  `MemTotal=32880800 KiB`; workload `memory.max=32212254720` bytes (30 GiB),
  `memory.high=31675383808` bytes (29.5 GiB), no machine swap. Ancestor CPU quota
  is `max 100000`. Post-run counters show no CPU throttling, memory-high/max
  events, or OOM events. Full before/after cgroup ancestry and machine snapshots
  are archived. Raw wall means are compared only within this worker, never
  against another orb type.
- Single managed invocation from **2026-09-10T13:25:04Z to 13:47:10Z**.
  Its atomic `mkdir started` guard allows at most one benchmark execution;
  duplicate starts sleep indefinitely instead of rerunning. After the benchmark,
  the wrapper atomically records its actual exit status and `exec sleep infinity`.
  Completion status was **0**, service `NRestarts=0`, and the final main process
  was `sleep infinity`. Status, journal, raw samples, and completion files were
  collected before stopping the service. There were no interrupted or extra runs.
- Two full clean warmups were excluded, then exactly **BC/CB/BC/CB/BC**, all with
  fixed **`-N4`**, warm OS cache, JS codegen, and no extra RTS arguments. Every
  compiler invocation started from deleted output. All outliers were retained;
  no sample was retried, substituted, or dropped. Setup and downloads finished
  before timing; no builds, tests, setup, downloads, or other benchmark workload
  ran concurrently. Only brief status reads occurred during timing.
- A post-timing fetch observed campaign HEAD
  [c44600e8f7a390ad73a46f8932e8f90a21b40a8a](https://github.com/vtrl/purescript/commit/c44600e8f7a390ad73a46f8932e8f90a21b40a8a).
  This report branch and its synchronized binary baseline were **not** advanced.

## Exact build, harness, and corpus identities

Both executables were transferred directly from the
[lead](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c),
each **47,869,944 bytes**, retained mode `0555`, with all identities rechecked
after timing. Supplied provenance is normal optimized GHC 9.6.6 / Stack 3.3.1 /
lts-22.43, not fast or profiling builds. Both binaries independently report GHC
9.6.6, RTS way `rts_thr`. No worker rebuild was performed. Their identical
development version text is not used as their source identity:

```text
0.15.15 [development build; commit: 6d636561f72b7985962e2931e462aa017a38f7fc DIRTY]
```

| Object | SHA256 |
| --- | --- |
| B: lead `.build/perf/purs-parse-release` → worker `bin/purs-baseline` | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: lead `.build/perf/parse-warnings-force/purs-candidate` → worker `bin/purs-candidate` | `92643eb2d1100711e7e8da39168202d76246ae0a6e335edf640bcac7c0d52e1d` |
| Unapplied lead `source.patch` | `8fe311c640a75f031d67152c0e302226d8453c6b6d7abcaaf8c7532ea81375d8` |
| Unchanged `ci/benchmark-compiler.py`, revision [e926183](https://github.com/vtrl/purescript/commit/e926183) | `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399` |
| Unchanged `ci/prepare-package-set-benchmark.sh` | `2c58732de6df1b38ab77b18e8083225d50dd9ee8a11f9b9c1f8d203f6e3e8c95` |
| Package set 60.4.0 JSON | `8cab74af472f4f4e142d2ff04e630a2aaf604b7af40987aacab0ae704fdadbb0` |
| `inputs.json` (4,901 source/FFI inputs) | `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| `purs-files.json` (4,084 PureScript sources) | `f157d71fde6dd11db4946391b7e6fe3a719d85ad5aaf7a552c2720dcdc549936` |
| Original and fresh `products.json` (8,985 products) | `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed` |
| Original lead `.build/perf/baseline-n1-warm/1.stdout` | `6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280` |
| Single-use `run-once.sh` | `3d3c05557c5cadd279e0fbcb368e2149b3e8298d809980de8f853bc601582140` |

Registry pin:
[5d834cd364da1d49a1bd1b0219ab49fb15f20601](https://github.com/purescript/registry/commit/5d834cd364da1d49a1bd1b0219ab49fb15f20601).
Preparation used Spago **0.93.43**, Node **22.23.2**, and the unchanged script:

```sh
export PATH="$PWD/.build/perf/n4-confirmation/tools/node_modules/.bin:$PWD/.build/perf/n4-confirmation/bin:$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
# bin/purs is a symlink to the supplied purs-baseline for Spago's version check.
bash ci/prepare-package-set-benchmark.sh .build/package-set-benchmark
```

Exact benchmark argument vector, with shell line wrapping for readability:

```sh
python3 /home/user/workspace/repo/ci/benchmark-compiler.py \
  --corpus /home/user/workspace/repo/.build/package-set-benchmark \
  --baseline /home/user/workspace/repo/.build/perf/n4-confirmation/bin/purs-baseline \
  --compiler /home/user/workspace/repo/.build/perf/n4-confirmation/bin/purs-candidate \
  --capabilities 4 --samples 5 --cache warm --codegen js \
  --results /home/user/workspace/repo/.build/perf/n4-confirmation/results \
  --label 'fresh-independent-N4-five-pairs; baseline-source=38080a40fc3a53de813a0ac46187e8c709dcbcc7; report-base=3f57015676d590bc55758cac58b65a16ee15b59d; candidate-source=2d5cc3bb3b766551e0b6c341dc84fc760915298f; GHC9.6.6-Stack3.3.1-lts22.43-normal-optimized; no-rebuild'
```

The invocation was launched only through:

```sh
amp orb service start parser-warning-n4-confirmation \
  --command 'bash /home/user/workspace/repo/.build/perf/n4-confirmation/run-once.sh' \
  --cwd /home/user/workspace/repo
```

No portal was requested. `argv.sh` preserves the shell-escaped harness argv.
`results/metadata.json` preserves the complete 4,084-source compiler argv:
`purs compile <sources> --output <corpus>/benchmark-output --codegen js +RTS -N4 -s -RTS`.
GNU time used `-f '%e %U %S %M'`; the compiler environment fixed `LC_ALL=C`,
`LANG=C`, and empty `GHCRTS`.

## Every measured pair is retained

B/C columns always show baseline then candidate, irrespective of execution order.
RSS is KiB; residency and allocation are bytes.

| Pair | Order | Wall B / C (s) | Paired wall change | RSS B / C (KiB) | Paired RSS change |
| --- | --- | --- | --- | --- | --- |
| 1 | BC | 110.35 / 107.89 | −2.229271% | 2,010,696 / 1,497,612 | −25.517731% |
| 2 | CB | 113.14 / 109.20 | −3.482411% | 2,031,096 / 1,444,104 | −28.900259% |
| 3 | BC | 108.97 / 106.27 | −2.477746% | 1,993,980 / 1,597,960 | −19.860781% |
| 4 | CB | 111.58 / 112.07 | +0.439147% | 2,022,956 / 1,569,112 | −22.434695% |
| 5 | BC | 108.27 / 109.48 | +1.117576% | 1,992,900 / 1,453,636 | −27.059260% |

| Pair | Residency B / C (bytes) | Paired residency change | Allocation B / C (bytes) | Paired allocation change |
| --- | --- | --- | --- | --- |
| 1 | 707,426,296 / 516,338,616 | −27.011673% | 397,631,555,376 / 397,644,615,320 | +0.003284% |
| 2 | 699,016,536 / 513,821,816 | −26.493611% | 397,630,821,584 / 397,627,697,024 | −0.000786% |
| 3 | 687,170,352 / 559,821,832 | −18.532307% | 397,620,166,448 / 397,658,345,288 | +0.009602% |
| 4 | 696,244,448 / 553,163,248 | −20.550426% | 397,635,525,824 / 397,626,871,816 | −0.002176% |
| 5 | 673,771,328 / 498,387,072 | −26.030234% | 397,638,289,360 / 397,632,064,328 | −0.001566% |

Excluded warmups B/C: wall **110.10 / 109.61 s**; peak RSS
**2,048,528 / 1,459,472 KiB**; residency **697,966,128 / 502,828,272 bytes**;
allocation **397,635,526,712 / 397,634,228,592 bytes**. Their raw records and
correctness checks are retained, but they enter no summary below.

## Independent statistics match the harness

`analyze.py` independently parses raw `.time` and RTS `.stderr`, without importing
the harness. Each paired change is computed as `100 * (C - B) / B`; means are
arithmetic means of the five paired percentages, **not** percentages of group
means. Sample SD uses denominator **4**. The reconstructed raw rows, execution
order, means, medians, SDs, and ranges match the harness's derived files.

| Metric | Mean paired change | Sample SD (pp) | Median paired change | Paired range |
| --- | --- | --- | --- | --- |
| Wall | −1.326541% | 1.992444 | −2.229271% | −3.482411% to +1.117576% |
| Peak RSS | −24.754545% | 3.619798 | −25.517731% | −28.900259% to −19.860781% |
| Maximum residency | −23.723650% | 3.899471 | −26.030234% | −27.011673% to −18.532307% |
| Allocation | +0.001672% | 0.004920 | −0.000786% | −0.002176% to +0.009602% |

| Variant | Metric/unit | Mean | Sample SD | Median | Range |
| --- | --- | --- | --- | --- | --- |
| B | Wall/s | 110.462 | 1.966 | 110.35 | 108.27–113.14 |
| C | Wall/s | 108.982 | 2.144 | 109.20 | 106.27–112.07 |
| B | RSS/KiB | 2,010,325.600 | 17,043.356 | 2,010,696 | 1,992,900–2,031,096 |
| C | RSS/KiB | 1,512,484.800 | 68,689.455 | 1,497,612 | 1,444,104–1,597,960 |
| B | Residency/bytes | 692,725,792.000 | 12,827,644.338 | 696,244,448 | 673,771,328–707,426,296 |
| C | Residency/bytes | 528,306,516.800 | 26,736,129.872 | 516,338,616 | 498,387,072–559,821,832 |
| B | Allocation/bytes | 397,631,271,718.400 | 6,909,320.179 | 397,631,555,376 | 397,620,166,448–397,638,289,360 |
| C | Allocation/bytes | 397,637,918,755.200 | 13,443,215.711 | 397,632,064,328 | 397,626,871,816–397,658,345,288 |

## Original-reference correctness checks pass

`python3 .build/perf/n4-confirmation/analyze.py` exited **0** and reported:

```text
PASS: 12 clean runs; 5 complete measured pairs; exact prescribed order; -N4 throughout.
PASS: all 12 transcripts match original 745-warning / 738-distinct multiset, preserving duplicates and internal bytes.
PASS: original 8985 products match; final products and all 4901 inputs rehashed unchanged.
PASS: independent raw-row reconstruction and mean/sample-SD statistics match harness output.
```

For each of the 12 stdout files, the checker removes only `Warning N of 745:`
headers and outer body whitespace, then compares **byte-content multisets**
against the original lead transcript. It checks contiguous ordinal coverage,
745 bodies, 738 distinct bodies, and duplicate multiplicity. Internal whitespace
and warning locations are not normalized. Canonical multiset SHA256 (sorted
UTF-8 body/count pairs, compact JSON) is
`01d75c574b46febb1d30ffe31372e75854a1d21e40bc1b8e643ec118716672d3`.

The unchanged harness verifies the inputs at block entry and compares generated
products after **every** clean run. Its common 8,985-product manifest exactly
matches the downloaded original reference, so all 12 runs match that original
by transitivity. The independent audit also rehashes all final output products
and all 4,901 inputs after timing. Every stderr contains all 4,084 distinct
compilation progress ordinals and RTS confirmation of `-N4`.

No compiler test suite was rerun here. The lead's optimized seed-9160 suite
1,303/0, unchanged goldens, baseline-first warning-location regression, and the
separately owned correctness audit are external evidence, not this worker's
executed checks. Their source patch remains unapplied on this report branch.

## Transferable raw evidence and remaining uncertainty

Worker-relative transfer path:
`.amp/in/artifacts/parser-warning-n4-xxlarge-20260910-raw.tar.gz`

Size: **23,634,016 bytes**. SHA256:
`9243d1e91606bc35e5dd2653bd679f607a59e6ad3c71846151d1543d903a1279`.
Download from the worker thread linked above. The archive includes both exact
binaries, all warmup/measured raw streams and timings, complete compiler argv,
metadata, original references, unapplied patch, unchanged harness/setup script,
corpus manifests, service guard/completion evidence, and independent analysis.
All **77** regular payload files passed an independent hash check by reading
the compressed archive against its internal `MANIFEST.sha256`; `gzip -t` passed.
Installed npm tools, fetched source trees, and generated output trees are not
included; their identities/manifests and the completed input/output audit are.
Those trees and exact executables remain available on the worker.

This block supports retaining the isolated warning-forcing change for its
repeatable memory reduction. It does not resolve wall-time noise, performance on
other workloads or concurrency settings, or interactions with later accepted
optimizations. No source integration, rebuild, master/upstream push, or cumulative
comparison was performed. A final cumulative replication remains pending the
lead supplying an accepted synchronized cumulative binary and comparison scope.
