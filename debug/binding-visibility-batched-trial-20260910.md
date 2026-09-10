# Batched binding visibility trial — 2026-09-10

**Rejected: the required dense-group gate still fails.** The exact requested
public-map approach, `mapMaybe` promotions left-unioned over the original names,
preserves tested semantics but regresses dense-1000 peak RSS by 61.449% and
maximum residency by 55.406%. All five measured N1 pairs are slower. The full
optimized suite passes 1,307 examples, but correctness does not make this an
acceptable performance change. No package-set screen or expensive repetition
was started after this gate failed.

## Why the original fold was rejected despite package-set wins

The prior sparse fold performed one `M.insert` per undefined name. Its original
package-set result remains valid: five alternating N4 pairs gave wall
-6.162% ± 3.591 percentage points sample SD and allocation -1.943%, with no
demonstrated RSS improvement. The lead subsequently reported additive Make +
fold N4 results of 108.152 ± 0.932 s to 101.950 ± 1.530 s, mean paired
-5.727% ± 1.780 points, allocation -1.940%, and full-suite 1,307/0.

Those workloads did not cover a dense undefined binding group. The lead's
compile-only counterexample is a valid typed cycle of 1,000 functions:

```purescript
f0 :: Int -> Int
f0 x = f1 x
-- ...
f999 :: Int -> Int
f999 x = f0 x
```

The original fold versus the exact Make baseline on that input had five-pair
mean wall +51.058% ± 12.002 points, RSS +43.413% ± 0.122 points, allocation
+35.269%, and maximum residency +52.094%. A point insertion rebuilds a search
path for every key when most names are undefined. This counterexample invalidates
the original acceptance recommendation, not its positive package-set measurements.

Original source/report branch `perf/binding-visibility-20260910`, source commit
`2722e8b67168abc122234353981e0cd8a52e4768`, report commit
`fe101d3cd6f92df654f0e0420d8f9de27eed922c`, and its raw evidence archive are
preserved unchanged. This report separately records the counterexample and the
failed batched replacement.

## Batched source and build identity

Branch `perf/binding-visibility-batched-20260910` starts at exactly accepted Make
commit `38080a40fc3a53de813a0ac46187e8c709dcbcc7`, not the lead's later report-only
commit. Compiler changes are confined to `TypeChecker/Monad.hs`. The original
five `TestCompiler.hs` visibility tests were carried forward unchanged, with
original author Justin Garcia preserved. No Make, Cabal, CST, Inliner, or harness
source was changed. No threshold, cache, internal Map API, or checker-state
invariant was introduced.

The implementation is:

```haskell
makeBindingGroupVisible = modifyEnv $ \e -> e { names = M.mapMaybe define (names e) `M.union` names e }
  where
  define (ty, nk, Undefined) = Just (ty, nk, Defined)
  define (_, _, Defined) = Nothing
```

The promoted map contains exactly the currently undefined keys. Left-biased
union replaces their values and retains the other keys, types, kinds, and
qualifications. Existing generic setters and scope restoration are unchanged.
Although this removes the explicit repeated-insertion algorithm, its dense
retention/performance behavior is still unacceptable. The exact retention
mechanism has not been diagnosed; the measurements below do not assume one.

Identities:

- Accepted base `src` tree: `c126b25499974017903f05d78ff6e0fe50e4201c`.
- Candidate source: `65f627aa0cd511b999004fee91428bebebd647ef`.
- Candidate `src` tree: `82de28c28755fe058aa77e07969eb0a8a19bf934`.
- Exact downloaded Make binary SHA-256:
  `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
- Candidate binary SHA-256:
  `a7331b22feedcaf9a8510003ccba703fb4a44a2eea4477566b71e4a051a8a2ff`.
- Candidate `Monad.hs` SHA-256:
  `3d73e10e82e1e458d09eab09bf0e96a13af46b3fe199ba7f95d30ac373db8be9`.
- Unchanged focused-test file SHA-256:
  `6b982061c07aa3c2a2eaf54b8fb07153b14c9577d925e926765260f8a62db7af`.

Stack 3.3.1, GHC 9.6.6, lts-22.43, normal locals `-O2 -Werror`. The build log
confirms recompilation of both the accepted Make source and the changed Monad.
Embedded version strings retain the old harness commit, so use source and binary
hashes above rather than that string to identify the experiment. Binaries are
preserved at `/home/user/perf-binding-batched/{purs-make-baseline,purs-batched}`.

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal build --test --no-run-tests --lock-file=error-on-write
```

Only `origin` at `https://github.com/vtrl/purescript` was fetched/pushed. Only
the named batched trial branch was pushed; master, upstream, and the accepted
campaign branch were not changed.

## Dense-1000 protocol and measurements

The lead-provided archive contains complete 300/1,000-function sources,
manifests, and original regression results. Downloaded archive SHA-256 matches
`f669d9970eea4e687b56be8b9dc222a56afe8bb47a6e4301c07024a218e922d6`.
Dense-1000 source SHA-256 matches
`408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7`;
its input manifest SHA-256 is
`6b017a4d18606d78ffc298ad87528ffe1ae7e8848f763c4fd8ed345e7d910e6d`.
The functions were **never executed**, only compiled.

Same 8-vCPU/16-GiB orb for baseline and candidate. The unchanged paired harness
has SHA-256 `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
It deletes output before every compile, holds the corpus lock, clears `GHCRTS`,
and invokes direct `purs compile Dense.purs --codegen js +RTS -N1 -s -RTS`.
Timing excludes hashing, dependency setup, builds, tests, and the two warm-ups.
No competing build or test workload ran during these measurements.

```sh
work=/home/user/perf-binding-batched
python3 ci/benchmark-compiler.py \
  --corpus "$work/lead-dense/dense-recursive-1000" \
  --compiler "$work/purs-batched" --baseline "$work/purs-make-baseline" \
  --results "$work/dense-1000-n1-paired-five" \
  --label Make38080-vs-batched-mapMaybe-union-O2 \
  --capabilities 1 --samples 5
```

| Pair | Order | Baseline wall (s) | Batched wall (s) |
| --- | --- | ---: | ---: |
| 1 | B/C | 0.85 | 1.08 |
| 2 | C/B | 0.98 | 1.00 |
| 3 | B/C | 0.88 | 1.03 |
| 4 | C/B | 0.85 | 1.24 |
| 5 | B/C | 0.93 | 1.01 |

All uncertainties below are sample SD, not confidence intervals. Percentage
changes are computed per pair before summarizing.

| Metric | Baseline mean ± SD | Batched mean ± SD | Paired change %, mean ± SD |
| --- | ---: | ---: | ---: |
| Wall (s) | 0.898 ± 0.0563 | 1.072 ± 0.0988 | +20.126 ± 17.182 |
| Peak RSS (KiB) | 144,949.6 ± 40.9 | 234,019.2 ± 90.3 | +61.449 ± 0.103 |
| Allocation (bytes) | 1,186,420,136 ± 0 | 1,210,946,627.2 ± 472.3 | +2.067269 ± 0.000040 |
| Maximum residency (bytes) | 59,255,504 ± 0 | 92,086,432 ± 0 | +55.405702 ± 0 |

Warm-ups B0.91/C1.06 s were excluded. No samples were dropped. Short-run wall
noise is large, but RSS and residency regressions are stable and substantial;
the dense gate clearly fails. The package-set N4 screen was therefore skipped,
rather than treating another package-set win as sufficient for acceptance.

## Correctness and evidence

The unchanged five focused scope/visibility tests pass. The normal optimized
full suite also passes **1,307 examples, 0 failures**, 123.4998 s, with no golden
updates or pending cases:

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

All 12 local compile-only runs (including warm-ups) have identical JS and
externs products, and empty diagnostic stdout. The JS also matches the lead's
archived output exactly. Externs differ across orbs only in 6,001 occurrences
of the absolute source path: replacing that path and its CBOR text-length
prefix makes the complete byte streams identical. No normalization is needed
for equality between the local baseline and candidate.

Raw review archive:
`.amp/in/artifacts/binding-visibility-batched-20260910/evidence.tar.gz`, SHA-256
`78d1ae576d8ca9ff5a45b0a206deb527cd26da50e9e230bad6c24562f9241073`.
It includes the original lead dense-regression archive unchanged, all new
measurements/products/metadata, build and test logs, source patch, exact harness,
build identities, and verification details. Raw working data remains under
`/home/user/perf-binding-batched`. The original sparse-fold positive evidence
remains separately under `.amp/in/artifacts/binding-visibility-20260910`.

Decision: **do not integrate either visibility trial**. This branch is retained
as negative experimental evidence. The prescribed batched approach did not
satisfy its prerequisite, so no package-set claim is made for it and no cache,
threshold, or internal-map workaround was substituted.
