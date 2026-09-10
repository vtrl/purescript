# Specialization flags: separate N4 screen and confirmation

The fresh five-pair N4 confirmation supports an isolated workload improvement:
mean paired wall time falls **29.586%**, allocation falls **43.630%**, RSS rises
**4.193%**, and sampled residency rises **6.134%**. All five pairs are faster.
Full optimized correctness passes; the stripped executable grows **47.322%**.
Baseline wall time drifts downward, so the paired results and variance matter.

The first screen below remains separate and is not pooled with confirmation.
Its original report is preserved at
[5cffa728](https://github.com/vtrl/purescript/commit/5cffa728b8cd7dfc3c0e4e87091c66f486dce733).
The lead owns independent N1/N8 verification. This worker made no traversal,
warning-force, source, dependency, or RTS-default combination.

## Upstream provenance and exact scope

The authoritative change is
[purescript/purescript PR 4584](https://github.com/purescript/purescript/pull/4584),
commit [8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
Its original author is **seastian <seastian@users.noreply.github.com>**;
GitHub is the upstream committer. The patch is dated 2025-10-18.
[OxfordAbstracts/purescript PR 16](https://github.com/OxfordAbstracts/purescript/pull/16)
imported that upstream commit; it did not originate the work. No evidence
supports attributing the upstream patch to Claude.

The upstream PR reports an approximately 30% private ACME gain, binary growth
from 110 MB to 130 MB, and longer compiler builds. Those observations motivate
this trial but are not local evidence: the private workload and repeated
measurement controls are unavailable here.

This branch, `perf/specialization-flags-20260910`, starts at exact campaign
[4861c1e1](https://github.com/vtrl/purescript/commit/4861c1e1e418122a172ad4b707f4ba5f8a2c5361).
Compiler source matches accepted Make lifetime commit
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The local functional adaptation is
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63),
retaining seastian as author and recording the upstream provenance.

Only two functional configuration edits are made:

```diff
-  "$locals": -O2 -Werror
+  "$locals": -O2 -Werror -fspecialize-aggressively -fexpose-all-unfoldings
```

```cabal
package purescript
  ghc-options: -fspecialize-aggressively -fexpose-all-unfoldings
```

The unrelated upstream `cabal.project` indentation cleanup is omitted. The
existing cheapskate revision, resolver, dependency versions, compiler source,
tests, and RTS defaults are unchanged. Both flags are tested together; this
is not a flags matrix and does not include the traversal-INLINABLE worker.
Raw upstream patch SHA256:
`9982045a76b085f54799c60ee2d87bc57c055f0636403e197f76692ba2720b15`.

## Optimized build and full-suite validation

Toolchain: Stack 3.3.1, GHC 9.6.6, lts-22.43, Node 22.23.2. This existing orb
has eight vCPUs (four cores with SMT), Intel Xeon @ 2.60 GHz family 6/model 106,
about 16 GiB physical RAM and a 14 GiB workload cgroup limit, no swap or CPU
quota. No new worker or machine resize was used.

Test fixtures were installed, and normal optimized Haskell dependencies were
prepared with `stack build --test --only-dependencies`, before measurement.
After `stack clean purescript`, the compiler-only build was timed separately:

```sh
/usr/bin/time -v -o compiler-build.time \
  env -u GHCRTS stack --no-terminal --jobs=6 build \
  purescript:exe:purs --lock-file=error-on-write
```

- Compiler build wall: **768.70 s**; user 762.44 s; system 10.17 s.
- GNU-time maximum RSS: **4,152,924 KiB**. This is GNU time's RSS measurement,
  not summed concurrent-process memory.
- Build-scope cgroup peak: **4,601,724,928 bytes**, including non-RSS charges;
  do not label this value RSS.
- No same-machine baseline full-build timing was collected, so this trial
  does not quantify relative compiler-build time or build-RSS growth.

The captured GHC command has `-O` followed by `-O2 -Werror
-fspecialize-aggressively -fexpose-all-unfoldings`, with no `-O0` or `-prof`.
The full suite used normal optimization, no golden acceptance, and seed 9160:

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=6 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

**1,302 examples, zero failures.** Hspec elapsed 95.5453 s. The entire Stack
test command took 1,026.93 s with maximum RSS 4,125,572 KiB, including a
test-enabled rebuild. These are validation/build timings, not compiler
performance samples. No fixture, golden, source, or dependency file changed.

Enabling tests changed Cabal's library unit ID and triggered recompilation.
The captured library GHC arguments differ only in that unit ID, not flags or
dependency arguments. Relinking changed the executable hash, but not its size.
The initial-build binary is preserved separately and was **not** benchmarked.

| Binary | Stripped bytes | SHA256 |
| --- | ---: | --- |
| Supplied accepted Make baseline | 47869944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| Initial specialization build, not benchmarked | 70523064 | `c790c75a1b6eb2960881371c32ce07a75efee7d549bad43b54289331b0046fd9` |
| Final tested specialization build | 70523064 | `4a918e8fe3b147cf1bd24adf26cc8286d3d72dbc0f5dbe928aa563166fd773c1` |

The tested binary is **22,653,120 bytes / 47.3222% larger** than the supplied
baseline. Both are stripped, dynamically linked x86-64 ELF executables. The
candidate embeds clean flags commit `c7df45fb`; GHC RTS is non-profiled
`rts_thr`, with unchanged embedded `-N` default. The preserved accepted Make
binary's older dirty version string is not its source identity; its hash is.

## One clean N4 pair: favorable allocation signal, visible drift

The exact paired harness from
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b)
has SHA256 `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
The fixed set 60.4.0 corpus has 506 packages, 4,084 PureScript sources and
4,901 inputs; manifest SHA256
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

```sh
python3 ci/benchmark-compiler.py --corpus .build/perf/corpus \
  --compiler .build/perf/purs-specialization \
  --baseline .build/perf/purs-parse-release \
  --capabilities 4 --samples 1 \
  --results .build/perf/specialization-trial/n4-screen \
  --label accepted-Make-38080a40-vs-specialization-c7df45fb
```

All four runs are clean JS compilations with explicit `+RTS -N4 -s -RTS`
and cleared `GHCRTS`; output is deleted before every run. One baseline and
one candidate warm-up are excluded, followed by one measured baseline/candidate
pair. Setup, builds, tests, and downloads completed beforehand. No concurrent
compiler/test/build load, nursery overrides, or profiling were used.

| Measured metric | Accepted Make | Specialization flags | Within-pair change |
| --- | ---: | ---: | ---: |
| Wall s | 138.37 | 93.17 | −32.6660% |
| Peak RSS KiB | 1966692 | 2027060 | +3.0695% |
| Allocated bytes | 397627058416 | 224193976312 | −43.6170% |
| Maximum sampled residency bytes | 673668656 | 736238392 | +9.2879% |
| Copied bytes | 68056914688 | 62606526304 | — |
| MUT elapsed s | 63.382 | 40.509 | — |
| GC elapsed s | 74.885 | 52.571 | — |
| GC CPU s | 152.360 | 105.654 | — |
| Minor / major collections | 34401 / 61 | 20687 / 54 | — |

| Excluded warm-up | Wall s | RSS KiB | Allocated bytes | Sampled residency bytes |
| --- | ---: | ---: | ---: | ---: |
| Accepted Make | 119.35 | 2007620 | 397636881048 | 714456568 |
| Specialization flags | 85.69 | 2106976 | 224148612224 | 750968944 |

There is **one measured sample per binary and no variance estimate**. The
measured baseline is substantially slower than its warm-up; the candidate
also slows. Do not treat 32.67% as a precise population effect, count warm-ups
as samples, or compare with old timings from other orbs. Both warm-up and
measured allocation values support a large allocation reduction, while RSS
and sampled residency increase in both comparisons. Repeated counterbalanced
N4 pairs and lead-owned independent N1/N8 verification are warranted before
acceptance. This screen's timing point is not accepted.

## Screen correctness and checkpoint decision

The harness exited 0 without interruption. All **8,985 products** match across
all four runs and the accepted-Make reference: 4,084 JS outputs, 817 foreign
JS files, and 4,084 externs. Product-manifest SHA256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.

All four **745-warning content multisets** match the accepted reference,
including multiplicities. Only numbered `Warning N of M:` headers and outer
body whitespace were removed; internal text and whitespace were preserved.
The four warning sequences differ, consistent with preexisting ordering
nondeterminism, not changed content. Canonical warning-multiset SHA256:
`21c39f0ba7c0a8370f1027327ad0c2559cd688dde940896e02c18b8f7626f436`.

Binary and all input hashes were rechecked after timing. GNU-time and RTS
values, paired calculations, and build-summary numbers were independently
checked against raw records. Full logs, GHC command arguments, original
upstream patch, fixture versions, metadata, product/warning manifests, and
`n4-screen/analysis.json` are retained under
`.build/perf/specialization-trial/`; an evidence archive will accompany the
confirmation report.

Recommendation at this checkpoint: retain the trial for repeated N4
confirmation, not integration as a proven speedup. Weigh its larger executable,
roughly 4 GiB compiler-build RSS, and higher workload RSS/residency against
the allocation and wall-time signal. Do not combine it with traversal-INLINABLE
or infer additive gains. No flags matrix, N1/N8, cold, incremental, startup,
IDE, or small-workload conclusions are supported yet.

The first screen completed using the existing shell invocation. Subsequent
long blocks must use a managed orb service and an ignored run-once wrapper:
an atomic started-directory guard prevents reexecution, command exit status
is recorded, and the wrapper then sleeps until evidence collection and
service shutdown. Missing completion status after interruption is reported
as partial evidence; it must never cause a blind rerun. No portal is needed.

## Fresh N4 confirmation: five complete counterbalanced pairs

The approved confirmation ran once from 2026-09-10 13:26:15 UTC through
13:47:25 UTC on the same existing orb. Managed service
`specialization-n4-confirmation` used the ignored guarded wrapper
`.build/perf/specialization-trial/run-n4-confirmation.sh`. It recorded exit 0,
then slept; the service was stopped after evidence collection. No interruption,
restart, additional compile, concurrent build/test, or sample exclusion occurred.

```sh
python3 ci/benchmark-compiler.py --corpus .build/perf/corpus \
  --compiler .build/perf/purs-specialization \
  --baseline .build/perf/purs-parse-release \
  --capabilities 4 --samples 5 \
  --results .build/perf/specialization-trial/n4-confirmation \
  --label accepted-Make-38080a40-vs-specialization-c7df45fb-confirmation
```

Both immutable binaries, the exact e926 harness, and pinned corpus are the
same as in the screen. Every run is a clean compile, with explicit N4 and no
extra RTS flags. Two full warm-ups are excluded; five pairs follow in
BC/CB/BC/CB/BC order (B = accepted Make, C = specialization flags).

| Excluded confirmation warm-up | Wall s | RSS KiB | Allocated bytes | Sampled residency bytes |
| --- | ---: | ---: | ---: | ---: |
| B | 119.28 | 1982940 | 397648426776 | 691899336 |
| C | 90.22 | 1959448 | 224161299456 | 678676672 |

All measured pairs, including high-memory and slower observations:

| Pair | Order | B wall s | C wall s | Wall change | B RSS KiB | C RSS KiB | B residency bytes | C residency bytes |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | BC | 129.14 | 85.73 | −33.614682% | 1954300 | 2121312 | 711570432 | 774015144 |
| 2 | CB | 124.19 | 86.63 | −30.243981% | 2009328 | 2018668 | 723093728 | 680790288 |
| 3 | BC | 123.51 | 86.45 | −30.005668% | 2026700 | 2003412 | 718375856 | 742598408 |
| 4 | CB | 120.12 | 87.54 | −27.122877% | 2059804 | 2206288 | 716023816 | 792485976 |
| 5 | BC | 118.51 | 86.58 | −26.942874% | 1980744 | 2099444 | 671263968 | 763178744 |

Means ± sample SD, using only the five measured observations per binary.
The final column is the mean ± SD of the five within-pair percentage changes,
not the percentage change between group means; its SD is in percentage points.

| Metric | B mean ± SD | C mean ± SD | Paired change % ± SD |
| --- | ---: | ---: | ---: |
| Wall s | 123.094 ± 4.116616 | 86.586 ± 0.644849 | −29.586016 ± 2.733698 |
| Peak RSS KiB | 2006175.2 ± 40758.551436 | 2089824.8 ± 82428.926835 | +4.193179 ± 4.276131 |
| Allocated bytes | 397634323360 ± 12948680.320863 | 224148015707.2 ± 7148853.299261 | −43.629611 ± 0.002135 |
| Sampled residency bytes | 708065560 ± 20988825.461875 | 750613712 ± 43006828.465354 | +6.133728 ± 7.681501 |
| Copied bytes | 68010227300.8 ± 346584965.439354 | 62343292113.6 ± 698172828.788398 | −8.326984 ± 1.448066 |
| MUT elapsed s | 55.878 ± 1.381765 | 37.1506 ± 0.438051 | −33.479980 ± 1.925344 |
| MUT CPU s | 192.5698 ± 5.657868 | 126.4112 ± 1.860581 | −34.304557 ± 2.372355 |
| GC elapsed s | 67.1318 ± 2.791189 | 49.343 ± 0.411580 | −26.380044 ± 3.564609 |
| GC CPU s | 137.1858 ± 5.792298 | 99.3422 ± 0.741836 | −27.470446 ± 3.429551 |
| Minor collections | 33918.8 ± 57.155052 | 20532.6 ± 61.654684 | −39.465377 ± 0.174457 |
| Major collections | 61.2 ± 0.447214 | 54.4 ± 0.894427 | −11.105235 ± 1.739491 |

The mean paired wall difference is −36.508 s (SD 4.621587 s). Baseline wall
times decline from 129.14 to 118.51 s; candidate times range 85.73–87.54 s.
Every pair favors the candidate, but effects shrink from 33.615% to 26.943%
across the block. This drift limits precision and generalization; neither the
first confirmation pair nor the separate 32.666% screen is the final estimate.
No outlier was discarded. Allocation is much more stable than time or memory.

### Confirmation correctness and evidence audit

The saved-evidence audit passed: **12 completed runs, five measured pairs,
8,985 equal products, and 745 equal warning-content multisets**. The harness
checked products after every run outside timing; the independent audit also
compared the final products and the screen/reference manifest. Product and
warning-content manifest hashes remain the exact values above.

All 12 warning sequences differ, including baseline repetitions, but content
and multiplicity match after removing only numbered warning headers and outer
body whitespace. Internal text/whitespace are preserved. This is preexisting
ordering nondeterminism, not a whole-stdout hash failure or a content regression.

All 4,901 input hashes, both binary hashes, and the harness hash were rechecked.
The audit reparsed GNU-time and RTS statistics, matched saved sample rows, and
independently recomputed all summary and explicitly matched pair statistics.
Full-precision values, individual allocations, pause/collection statistics,
and raw stdout/stderr/time files are in `n4-confirmation/analysis.json` and
the accompanying raw run files. `n4-screen/` remains a separate dataset.

Evidence archive: `.amp/in/artifacts/specialization-flags-20260910-evidence.tar.gz`
(2,491,252 bytes), SHA256
`267b98d0d5bbcdde20387e02962f329fe877fd77a23549d71f41e786812dd51d`.
It contains both datasets, audit code/output, guarded wrapper and exit status,
build/test logs, GHC arguments, binary identities, exact harness and corpus
manifests, upstream patch, and applied flags patch. `build-summary.json`
identifies the initial compiler-only build; `tested-binary.json` identifies
the different final test-enabled binary actually used for every benchmark.

The tested executable stays read-only (mode 0555) at
`.build/perf/purs-specialization`, **70,523,064 bytes**, SHA256
`4a918e8fe3b147cf1bd24adf26cc8286d3d72dbc0f5dbe928aa563166fd773c1`.
No initial-build binary substitution occurred.

### Recommendation: retain for gated integration, not discard

N4 confirmation and unchanged correctness justify retaining the isolated
upstream flags candidate for the lead's integration decision. The evidence
supports a substantial clean-package-set N4 improvement on this machine, not
universal compiler or startup performance. Condition broader acceptance on
the lead-owned independent N1/N8 results and acceptance of the costs:
47.322% executable growth, roughly 4 GiB compiler-build RSS, and higher mean
workload RSS/residency despite lower allocation. No relative build-time gain
or regression can be quantified without a matched baseline build.

This worker's scope is complete: one screen and one separate five-pair N4
confirmation, normal optimized full suite 1302/0, and attributed configuration
only. No additional N1/N8, tiny, cold, incremental, IDE, or interaction trials
were run here. Do not assume additive gains with traversal-INLINABLE or
warning-force changes; those require a separately coordinated trial.
