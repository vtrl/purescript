# Binding visibility allocation trial — 2026-09-10

Decision: retain the trial for lead-owned additive validation, not discard.
Five alternating N4 pairs all favored the candidate: paired wall change
**-6.162% ± 3.591 percentage points sample SD**, with allocation down 7.721 GB
(-1.943%) and no demonstrated RSS improvement. The full optimized suite passed
1,306 examples. These results are against the original compiler only; they do
not establish the gain on the lead's newly accepted Make baseline.

## Scope and rationale

The original `makeBindingGroupVisible` mapped over every entry in `names`,
allocating replacement map nodes and triples even for already-`Defined`
imports. The campaign's native O2 CPU sample identified this function among
compiler leaves (136/24,857 samples). The auto-SCC diagnostic attributed
roughly 7.7 GB to its lambda; that instrumented run is not a benchmark.

The trial scans the original map and inserts a replacement only for an
`Undefined` entry. Defined entries remain shared. It deliberately retains the
full scan rather than introducing a second visibility representation into
`CheckState`. The cost is O(n + u log n) for n names and u undefined entries,
instead of rebuilding all n entries; unusually large all-undefined environments
may do more tree work. No keys, types, kinds, or visibility results change.

The complete environment-write audit found:

- `bindNames` left-unions shadowing entries and restores the entire original
  names map; `preservingNames` and `withBindingGroupVisible` restore names too.
- `emptyCheckState` accepts an arbitrary supplied environment. Public `putEnv`
  and `modifyEnv` can also replace arbitrary names, including deleting entries
  or changing a defined name back to undefined.
- `TypeChecker.hs` adds values and foreign declarations as `Defined` through
  `putEnv`; other setters there change types, constructors, synonyms, or classes.
- Monad scope helpers and the direct `TypeChecker.hs` dictionary update only
  alter non-name fields. Externs loading introduces `Defined` imported names
  before the environment enters the checker.
- `TypeSearch.checkInEnvironment` directly substitutes an environment into a
  saved `CheckState`. A cached pending-key design would have to cover that path.
- Undefined recursive/local entries originate in `TypeChecker/Types.hs` through
  `bindNames`. Its `getVisibility`/`checkVisibility` semantic gate is unchanged.

The selected implementation reads the current map at every call, so all these
existing operations work without new lifetime invariants or API changes.
No changes touch Make, CST, Inliner, Cabal, or default RTS settings.

## Source, build, and machine identity

Only the verified remote `origin`, fetch and push URL
`https://github.com/vtrl/purescript`, was used. Only
`perf/binding-visibility-20260910` was pushed; neither master nor upstream was
changed.

- Original compiler: `9160ce1518b5f11f9ebe32b445019f8dbb4f435a`.
- Source-equivalent harness baseline: `6d636561f72b7985962e2931e462aa017a38f7fc`.
  Its only differences from the original compiler are benchmark/docs files.
- Baseline `src` tree: `cdf9e074d76b70f7144829a9452fa6f98b014105`.
- Trial implementation/tests: `2722e8b67168abc122234353981e0cd8a52e4768`.
- Trial `src` tree: `226b0805f0f198c45a58468535eb733f00d0919a`.
- Baseline binary SHA-256:
  `208a8d236f29dc6aea7ce520932b2b4c4b7513122fbc7a6c1c654922636c0aa6`.
- Candidate binary SHA-256:
  `251090816949e1553e1262c73ab87b19e3d0e87ab1c3a92eccd9a5f42bda9a6b`.
- Compiler-change file SHA-256 (`TypeChecker/Monad.hs`):
  `ce6a402bb2e0b12b0e29fff19a90311eee920d56e0cce122499ff6b864d8a1bd`.

Stack 3.3.1, GHC 9.6.6, lts-22.43, normal locals `-O2 -Werror`; no `--fast`,
profiling build, or benchmark RTS override beyond explicit `-N1`/`-N4 -s`.
Both binaries were built using:

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal build --test --no-run-tests --lock-file=error-on-write
```

The baseline binary was copied to `/home/user/perf-binding/purs-baseline`
before edits. The candidate is `/home/user/perf-binding/purs-candidate`.
Both executables retain the embedded version string for the harness baseline;
source-tree and binary hashes above identify the actual candidate independently.
Machine: same orb, 8 vCPUs, 16 GiB RAM, Intel Xeon @ 2.60 GHz, Linux x86_64.
Full CPU/memory/kernel metadata is in each benchmark's `metadata.json`.
No builds, tests, or dependency setup ran concurrently with timed compilations.

## Corpus and benchmark protocol

Package set 60.4.0, all 506 packages, 4,084 PureScript modules and 4,901 total
inputs. Required manifest SHA-256 matched:
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
Spago 0.93.43 and its compiler-version-check dependency were installed privately
under `/home/user/perf-binding/tools`, outside timing. Preparation used the
shared `ci/prepare-package-set-benchmark.sh`; the corpus lives at
`/home/user/perf-binding/corpus`.

The harness invokes the preserved compiler directly, removes the JS output
before each run, clears `GHCRTS`, holds the corpus lock, and times only the
compiler subprocess with `/usr/bin/time`. Hashing occurs after timing and checks
all 8,985 emitted JS/foreign/externs products after every run. Warm-ups are clean
compiles and are excluded from samples.

N4 screening used the baseline harness, one excluded warm-up and one measured
sample for B, then the same for C. N1 uses the new paired harness extracted
unchanged from `e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b`; its SHA-256 is
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
It is retained as `/home/user/perf-binding/benchmark-compiler-paired.py`.
The repository's harness file was not modified by this trial.

## Initial screen

GB means decimal bytes / 1,000,000,000. With one measured sample per variant,
sample SD cannot be estimated.

| RTS | Variant | Wall (s) | Peak RSS (KiB) | Allocation (bytes) | Maximum residency (bytes) |
| --- | --- | ---: | ---: | ---: | ---: |
| N4 | Baseline | 127.79 | 3,424,860 | 397,386,896,280 | 1,202,033,640 |
| N4 | Candidate | 122.56 | 3,451,464 | 389,681,712,760 | 1,200,912,736 |
| N1 | Baseline | 253.39 | 3,292,544 | 397,282,512,368 | 1,147,740,264 |
| N1 | Candidate | 239.67 | 3,248,436 | 389,567,226,744 | 1,126,099,880 |

N4 changes: wall -4.09%, RSS +0.78%, allocation -7.705 GB (-1.94%), maximum
residency -0.09%. All product hashes match. Warm-up B was 117.39 s and C was
128.69 s, showing that timing noise is material. The allocation saving matches
the profiled site; the single timing pair is not repeatability evidence.

N1 changes: wall -5.41%, RSS -1.34%, allocation -7.715 GB (-1.94%), maximum
residency -1.89%. Excluded warm-ups were B253.05/C241.35 s. All product hashes
match the N4 products too. The measured RTS logs report MUT elapsed times of
159.384/159.349 s and GC times of 93.876/80.191 s for B/C respectively. GC
copied 66.920/50.116 GB. Nearly all the measured wall difference is accounted
for by the reported GC-time difference; repetition remains necessary.

Raw screen directories: `/home/user/perf-binding/n4-baseline-screen`,
`/home/user/perf-binding/n4-candidate-screen`, and
`/home/user/perf-binding/n1-paired-screen`.

## Alternating repetition

The N4 repetition uses five pairs with B/C on odd pairs and C/B on even pairs,
following one excluded clean warm-up per binary. These samples are separate
from the initial screens. N1 currently has a screen only, not a five-sample
timing claim.

```sh
work=/home/user/perf-binding
python3 "$work/benchmark-compiler-paired.py" \
  --corpus "$work/corpus" \
  --compiler "$work/purs-candidate" --baseline "$work/purs-baseline" \
  --results "$work/n4-paired-five" \
  --label 6d636561-O2-baseline-vs-2722e8b-sparse-visibility \
  --capabilities 4 --samples 5
```

| Pair | Order | Baseline wall (s) | Candidate wall (s) |
| --- | --- | ---: | ---: |
| 1 | B/C | 113.73 | 107.83 |
| 2 | C/B | 121.57 | 107.56 |
| 3 | B/C | 114.77 | 108.96 |
| 4 | C/B | 114.43 | 112.42 |
| 5 | B/C | 116.90 | 108.39 |

The following uncertainties are sample SD, not confidence intervals. Percentage
changes are calculated per pair and then summarized, not from the two means.

| Metric | Baseline mean ± SD | Candidate mean ± SD | Paired change %, mean ± SD |
| --- | ---: | ---: | ---: |
| Wall (s) | 116.280 ± 3.185 | 109.032 ± 1.969 | -6.162 ± 3.591 |
| Peak RSS (KiB) | 3,439,512.0 ± 16,475.9 | 3,450,348.8 ± 35,696.9 | +0.320 ± 1.487 |
| Allocation (GB) | 397.385968 ± 0.014949 | 389.665334 ± 0.018019 | -1.942855 ± 0.004026 |
| Maximum residency (GB) | 1.201764 ± 0.011002 | 1.203241 ± 0.014924 | +0.136 ± 2.035 |

All five pairs reduced both elapsed time and allocation. Average allocation
saved is 7,720,634,048 bytes. The RSS change is small and mixed; do not describe
this as a peak-memory optimization. The slow second baseline contributes to
the timing spread and is retained. No samples were dropped. Excluded warm-ups
were B115.02/C110.81 s. N1 remains a one-pair screen, with no repeated-N1 claim.

## Correctness

Five focused state tests passed against both the original implementation and
the candidate. They assert independently constructed full names maps, not only
success: supplied undefined names, unchanged types/kinds/imports, opposite
shadowing directions, qualified-name identity, nested visible scopes with new
inner bindings, scope restoration, arbitrary `putEnv`/`modifyEnv`, preservation
of other state, and restored `CycleInDeclaration`/`NameIsUndefined` errors.

Existing recursive-cycle failures and accepted lambda-recursion fixtures are
also exercised by the full optimized suite:

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

Result: **1,306 examples, 0 failures**, 124.4727 s. No golden updates, pending
tests, or unrelated worktree changes. Logs are retained under
`/home/user/perf-binding/{baseline-focused-tests,candidate-focused-tests,full-tests}.log`.

All **20** completed benchmark runs (including excluded warm-ups) emitted the
same 8,985 products and **745 identical warning contents**. Warning comparison
removes only the numbered `Warning i of n` headers and surrounding whitespace,
then compares the complete bodies as a multiset, preserving duplicate counts
while ignoring parallel output order. `verification.json` records the audit.
Product manifest SHA-256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.

## Evidence and integration boundary

Review archive: `.amp/in/artifacts/binding-visibility-20260910/evidence.tar.gz`
(3.2 MiB), SHA-256
`df377e526b045adc001fa34bcc16903080b0876707a14fc22e0ddaa8fdc20dc1`.
It contains every run's stdout/stderr/time, metadata, products and summaries;
build logs and identities; both focused-test logs and the full suite log;
the exact paired harness, source patch, corpus-preparation log, and diagnostic
equality audit. The preserved binaries and corpus remain under
`/home/user/perf-binding`; they are not duplicated in the archive.

The source remained at `2722e8b67168abc122234353981e0cd8a52e4768` throughout
repetition. The lead accepted Make lifetime commit
`38080a40fc3a53de813a0ac46187e8c709dcbcc7` while this experiment was running
and explicitly requested finishing the original-source pairs unchanged. The
lead owns the combined Make + visibility build, full tests, and paired N4
measurement against its exact Make binary. This worker did not rebase, change
source mid-run, duplicate that work, or claim additive performance results.

Recommendation: keep this small, scope-contract-preserving allocation change
for integration **only if the lead reproduces its additive benefit**. The
original-source timing and allocation evidence supports retention, but the new
baseline changes GC behavior, so the standalone percentage cannot be added to
the Make improvement. No cache/keyset complexity is justified by this trial.
