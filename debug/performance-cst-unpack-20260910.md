# CST unpacking trial — initial screens, 2026-09-10

## Status and interpretation

**Full correctness passes and all product hashes match. Memory use is lower in both initial screens; a speedup is not established.** There is only one measured sample per binary/capability, so sample variance is unavailable. The five-sample finalist phase is deliberately deferred at the lead's request until the interaction with the separate Make AST-lifetime trial is coordinated.

This is an **unaccepted source-and-report checkpoint** on `perf/cst-unpack-20260910`, based on shared harness checkpoint [b89f5b4998c0fe0a85d699826b79c2d0fe27eaf9](https://github.com/vtrl/purescript/commit/b89f5b4998c0fe0a85d699826b79c2d0fe27eaf9). The compiler baseline remains source-equivalent to [9160ce1518b5f11f9ebe32b445019f8dbb4f435a](https://github.com/vtrl/purescript/commit/9160ce1518b5f11f9ebe32b445019f8dbb4f435a). No source rebase or combination with the lead's Make changes has occurred; no measurements are running. Further repetitions await an exact Make baseline for an additive CST trial. Origin fetch/push is only `https://github.com/vtrl/purescript`.

## Hypothesis and bounded change

The lead's heap profile identified substantial live CST position/range/token annotation objects. The trial adds `UNPACK` only to already-strict fields in `src/Language/PureScript/CST/Types.hs`: `SourceRange.srcStart`, `SourceRange.srcEnd`, `TokenAnn.tokRange`, and `SourceToken.tokAnn`. `SourcePos` already unpacks its two strict `Int` fields.

Flattening may remove object headers/pointer indirections but can lose sharing or introduce reboxing. Measurements, rather than the representation hypothesis, determine whether to retain it. GHC's generated interface confirms all four fields are unpacked. No custom serialization, FFI, `Storable`, or unsafe layout consumers were found for these CST records. Existing parser/token round trips, diagnostics/goldens, compiler, IDE, and remaining tests cover externally observable behavior.

Only that source file changed: four inserted pragmas. No Make, typechecker, parser algorithm, Cabal, global RTS defaults, shared harness, golden files, or test fixtures were edited. There is no file overlap with the lead or nursery worker.

## Build and correctness

Toolchain: Stack 3.3.1, GHC 9.6.6, lts-22.43, normal `-O2 -Werror` locals; Node 22.23.2 for the JavaScript tests. No `--fast` or GHC profiling flags.

```bash
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal --jobs=8 build --test --no-run-tests --lock-file=error-on-write
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

Both commands exited 0. Rebuild: 138.46 s wall. Full suite: **1301 examples, 0 failures, 0 pending**, 108.9055 s Hspec / 111.16 s total wall. Failing test names: none. Test-suite duration is not used as a performance comparison.

The incremental rebuild did not recompile `Version.hs`'s Template Haskell metadata, so the candidate's version string still names the original baseline. Use source patch/hash and binary hash below for identity; do not infer source equivalence from that stale string.

## Measurement procedure

The unchanged `ci/benchmark-compiler.py` performed each screen with `--samples 1`, hence one excluded clean warm-up and one measured clean build. Sequence: baseline N4 → candidate N4 → baseline N1 → candidate N1. Every compile used the same sorted 4084-source full-JS corpus and stable `benchmark-output` path, deleting output before every run. No incremental/no-op build was timed. OS caches were not dropped; these are warm-cache screens. No other build, profiler, or benchmark ran concurrently in this orb.

Each invocation used:

```text
python3 ci/benchmark-compiler.py --corpus .build/perf/corpus \
  --compiler <preserved baseline or candidate> --results <new screen directory> \
  --label <binary/source identity> --capabilities <1 or 4> --samples 1
```

Corpus: package set 60.4.0, 4084 PureScript sources / 4901 total inputs. Input manifest SHA-256: `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

Machine: Debian 12/Linux 6.1.158+, x86_64 E2B orb, 8 logical Xeon CPUs @ 2.60 GHz (4 cores × 2 SMT), approximately 15 GiB visible RAM / 14 GiB workload limit, no swap. Full command, machine, input, and binary metadata are saved per screen.

## Initial measured samples

| Metric | N1 baseline | N1 candidate | Change | N4 baseline | N4 candidate | Change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Wall seconds | 239.54 | 243.96 | +1.845% | 117.41 | 117.17 | −0.204% |
| Peak RSS, KiB | 3,409,228 | 2,860,072 | −16.108% | 3,454,472 | 3,045,656 | −11.834% |
| Allocated bytes | 397,425,045,360 | 397,155,565,992 | −0.0678% | 397,521,687,976 | 397,223,373,408 | −0.0750% |
| Maximum sampled residency, bytes | 1,200,800,800 | 985,098,920 | −17.963% | 1,210,474,424 | 1,061,558,528 | −12.302% |

Excluded warm-up wall seconds: N1 baseline 245.19 / candidate 243.77; N4 baseline 114.49 / candidate 115.14. These are not additional measured samples and do not supply a variance estimate. Per-screen `summary.json` correctly reports `stdev: null`.

**All 8985 generated-product hashes are identical** across both binaries, both capability settings, and all warm-up/measured repetitions. These cover 4084 `index.js`, 817 `foreign.js`, and 4084 `externs.cbor` files.

The N1 sample is slower and N4 is essentially unchanged. Together with the warm-up variation, this does not establish either a speedup or a reliable wall-time regression. The lower RSS/residency is worth investigating, but may overlap the Make lifetime fix; repeated paired measurements must follow the next baseline decision.

## Preserved binaries and artifacts

- Baseline: `/home/user/workspace/repo/tmp/baseline-correctness/purs-baseline-O2`; SHA-256 `c50e97532fb5beffb610469ae7f4a6d8e5f99015dff57e7f58305609cd0ad5b2`.
- Candidate: `/home/user/workspace/repo/.build/perf/cst-unpack/purs-candidate`; SHA-256 `be7e2e0fa09380767878a01e08d5dedc9f71a25ef8784cf50a1f43057356e396`.
- Candidate `CST/Types.hs` SHA-256: `15be7c71660e09532eb01b37e9238aacdcc0fe470da24cc32612a9fdc351a203`.
- Both binaries are preserved read-only. The optimized Haskell build workspace remains ready.

Raw artifacts remain in the worker orb at `.amp/in/artifacts/cst-unpack-20260910/`: `source.patch`, `build.log`, `full-suite.log`, `candidate-identity.txt`, `candidate-representation.txt`, and `screens.tar.gz` (approximately 2.3 MiB). The archive contains all four screen directories with raw stdout/stderr/time logs, exact metadata, samples, summaries, and matching product manifests. These ignored artifacts and compiler binaries are not part of the source checkpoint.

Earlier native CPU-profile artifacts are retained separately at `.amp/in/artifacts/native-perf-20260910/`: 24,857 samples, zero lost, top GC leaves `evacuate1` (5182 / 20.85%), `eval_thunk_selector` (3885 / 15.63%), `scavenge_block1` (1030 / 4.14%). Its 17.07% unresolved main-image samples and lack of call stacks prevent allocation/caller attribution. It is profiling evidence, not a performance win.
