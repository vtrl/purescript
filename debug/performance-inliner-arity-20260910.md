# Inliner arity matching: negative optimized screens

**Discard both optimization trials.** The positive-arity guard was neutral;
deferred argument-list construction saved little allocation and screened slower.
Compiler source was restored exactly to the shared baseline. No N1 or five-pair
confirmation run was justified, and no broader dispatch rewrite was attempted.

This report is a standalone evidence-only commit. Cherry-pick it without the
rejected compiler changes or the trial branch's additional AST tests.

## Scope and reproducibility

- Repository: `https://github.com/vtrl/purescript`; branch
  `perf/inliner-arity-20260910`, based on source-equivalent
  [6d636561](https://github.com/vtrl/purescript/commit/6d636561f72b7985962e2931e462aa017a38f7fc).
- Original compiler baseline:
  [9160ce15](https://github.com/vtrl/purescript/commit/9160ce1518b5f11f9ebe32b445019f8dbb4f435a).
- GHC 9.6.6, Stack 3.3.1, `lts-22.43`, normal `-O2` from `stack.yaml`.
  No `--profile`, `--fast`, or RTS tuning; binary RTS way `rts_thr`.
- a1.large orb: 8 logical vCPUs, Intel Xeon @2.60GHz (family 6/model 106),
  4 cores × 2 SMT, Linux 6.1.158+, KVM, 16,785,420,288 bytes guest RAM,
  14 GiB workload cgroup memory limit, no swap or CPU quota.
- Shared package set 60.4.0: 4,084 PureScript files / 4,901 inputs;
  manifest SHA256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
- Exact sorted `purs-files.json`, cwd corpus, clean `benchmark-output`,
  `--codegen js +RTS -N4 -s -RTS`, `LC_ALL=C LANG=C GHCRTS=''`, warm OS caches.
  Setup, hashing, and builds are outside the timed process. Nothing else was
  building or testing in this orb during measurements.

## One-sample screens reject the candidates, not all possible optimizations

Each binary had one full clean warmup and one measured sample. GB is decimal;
RSS is KiB. These are screening results, not statistically established effects.

| Trial | Baseline wall s | Candidate wall s | Wall change | Baseline allocated B | Candidate allocated B | Allocation change | RSS B / C |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Positive arity guard | 116.87 | 116.78 | −0.077% | 397,526,749,344 | 397,458,704,808 | −0.0171% | 3,483,212 / 3,437,124 |
| Deferred argument list | 116.47 | 117.98 | +1.296% | 397,501,965,232 | 396,843,971,944 | −0.1655% | 3,426,536 / 3,509,776 |

Warmups were 118.45 / 116.99 s (guard) and 115.19 / 117.96 s (deferred).
The deferred candidate's measured RSS changed +2.429%.
All **8,985 deterministic products** matched the original baseline exactly,
including JavaScript and externs, across all measured and warmup runs.

The guard stops `runFn'.go` at exhausted arity instead of continuing into
negative arity, which cannot match. The deferred variant retains the existing
44 rules and their ordering, but validates the terminal root before consing
arguments while returning through the bounded recursion. It reverses the
successful argument list to preserve order. Neither justified integration.

## Trial history and checks

- Guard and direct AST checks:
  [84c0b4cf](https://github.com/vtrl/purescript/commit/84c0b4cfda45408669f2f7064a0dbce960cfa972).
- Deferred-list variant:
  [0214bad5](https://github.com/vtrl/purescript/commit/0214bad5e909e94e589292a973a854fa07b2e8c9).
- Restore original matcher:
  [512c3fa1](https://github.com/vtrl/purescript/commit/512c3fa1e24d6f9526d0bd6a9b4a4c6bd287ba0b).
- Compiler scope: `src/Language/PureScript/CoreImp/Optimizer/Inliner.hs`,
  `runFn'` only. Tests: `tests/TestAst.hs`; no other worker's files changed.
- Both candidates: 137 direct AST examples, zero failures. Cases cover all four
  runFn families, arities 0..10, distinct argument order and source spans,
  effect/ST wrappers, under/overapplication, arity 11, wrong module, and an
  unrelated long application spine.
- Deferred candidate full suite: **1,438 examples, zero failures**, 123.47 s.
- Restored baseline plus trial tests: **1,438 examples, zero failures**, 109.93 s.
  Compiler source and build configuration diff against the shared base is empty.

Build/test commands: `stack build purescript:exe:purs -j4`,
`stack test --test-arguments='--match inlineCommonOperators' -j4`, and
`stack test -j4`. Guard screens used `ci/benchmark-compiler.py` at the shared
base, separately B then C, `--capabilities 4 --samples 1`. Deferred used the
exact paired harness from
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b),
with `--baseline .build/perf/purs-baseline --compiler .build/perf/inliner/purs-deferred
--corpus .build/perf/corpus --capabilities 4 --samples 1` and a fresh results directory.

| Binary | SHA256 |
| --- | --- |
| `.build/perf/purs-baseline` | `d105283e23997dc02c531fcfce7036554c6d68599ee528536bfafd35375e3d00` |
| `.build/perf/inliner/purs-guard` | `4cbe46c138be589a9da15f668931d1a6a6e0acd921a5bef58ba3f9c862bcf9df` |
| `.build/perf/inliner/purs-deferred` | `36459d2382313bee3b9a5de09b1c1f7245100d7132327c77b5533ff90c7c1713` |

## Profiling evidence is diagnostic, not a speedup estimate

The preceding exact-baseline `-O2 --profile -fprof-auto` run successfully compiled
the same corpus and matched all products. Its JSON inclusive costs were:
type checking 192.914 GB (34.679%), JS emission 64.662 GB (11.624%), CoreImp
optimization 52.703 GB (9.474%), `inlineCommonOperators` 22.466 GB (4.039%),
and `runFn'.go` 11.281 GB (2.028%; 11.185 GB exclusive; 264,150,719 entries).
Inclusive rows overlap and must not be added.

Automatic profiling itself consumed 65.44% of ticks. JSON attributed allocation
was 556.288 GB, versus physical RTS allocation 860.440 GB. GHC JSON includes
the explicit profiling-overhead bucket but omits each closure's profiling header.
Normal `-O2` measurements, not those instrumented percentages, decide acceptance.
These two implementations did not realize the allocation magnitude suggested
by the profile; this does not establish that every arity optimization is futile.

Full commands, hardware, raw profiles, logs, summaries, hashes, patches, and
caller-stack evidence are preserved in the
[profiling/arity worker thread](https://ampcode.com/threads/T-01a08a9c-4b3c-702a-9422-8dcd013fbe1a):
`.amp/in/artifacts/profile-baseline-20260910/` and
`.amp/in/artifacts/inliner-arity-20260910/`.
The 224 MiB raw profile is also preserved as a tested ~14 MiB gzip file and
transferred to the lead with the remaining profiling artifacts before archiving.
