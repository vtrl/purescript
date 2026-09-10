# Specialization flags: first N4 screen and build costs

The combined flags produce a credible screening signal, not an accepted
performance result: one measured N4 pair reduces wall time by 32.67% and
allocation by 43.62%, while RSS rises 3.07% and sampled residency rises 9.29%.
The stripped executable grows 47.32%. Full optimized correctness passes.
One fresh five-pair N4 confirmation is approved on this existing orb; its
results will remain separate from this screen. The lead owns independent
N1/N8 verification. No repeated N4, N1, N8, or traversal-INLINABLE interaction
measurements are included at this checkpoint.

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

## Correctness, evidence, and next decision

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
