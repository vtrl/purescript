# Read-only unknown generalization: normal-O2 trial

Campaign: `perf/linux-package-set-20260910`, exclusively in
<https://github.com/vtrl/purescript>. Trial branch:
[`perf/varifunknown-readonly-20260910`](https://github.com/vtrl/purescript/tree/perf/varifunknown-readonly-20260910).
**Recommendation: retain for provisional integration as a small allocation
reduction, subject to lead integration checks.** Four counterbalanced N4 pairs
confirm 0.519% less allocation (2.063 GB per clean package-set compile).
They do **not** establish a wall-time, peak-RSS, or sampled-residency improvement.
No further local timing or N1 expansion was run. Do not advertise the initial
one-pair 4.32% wall improvement as the result of this trial.

## Source and executable identity

- Exact accepted source baseline:
  [38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7),
  the accepted Make parsed-AST lifetime fix, not the original campaign baseline.
- Trial source and three focused tests:
  [7b69e68a](https://github.com/vtrl/purescript/commit/7b69e68aadada69371190b735dcd22ac205d42e3).
  Only `varIfUnknown` and its now-unused import in `TypeChecker/Unify.hs`,
  plus `tests/TestAst.hs`, changed. No state representation, shared traversal,
  benchmark harness, compiler configuration, or other worker's module changed.
- The accepted baseline executable was downloaded from the
  [lead thread](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c),
  path `.build/perf/purs-parse-release`, and preserved with mode 0555.
  SHA-256: `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
  Its old `6d636561… DIRTY` TH version metadata is expected; identity comes
  from this supplied hash and the accepted source checkpoint.
- Measured candidate: `.build/perf/purs-varifunknown-tested-release`, mode 0555.
  SHA-256: `604b26f6e2532804378a6717140150b470bccea148c47ccad23f8dd6b23ce8e2`.
  It embeds `38080a40… DIRTY`: the optimized build preceded the trial commit.
  Exact patch `.build/perf/varifunknown-trial.patch` SHA-256:
  `03d9a8b5021a05b646aa09cf6a6a4507cef0ec7f3907e26f5827208da2db9003`.
- A first optimized executable was preserved before tests with SHA-256
  `2730c045bda1ed488679fb1d642e93cdd66cace0f9bc629d3bde6003c93a818e`.
  Enabling tests refreshed Hspec and rebuilt the compiler. **No timing used
  that earlier executable**; all measurements use the actually tested build
  identified above. Both builds used normal local `-O2`, not `--fast` or profiling.

The change reads `substNames . checkSubstitution` once, closes over that map,
uses the existing pure `everywhereOnTypes` for kinds and body, and ordinary
`map` for bindings. Existing `mkForAll` preserves supplied binding order.
Name rendering remains stored prefix (default `t`) followed by the decimal
unknown number. The supplied unknown list controls quantification only:
every `TUnknown` in the body and all nested kinds is still replaced.

## Environment, corpus, and controls

- Stack 3.3.1, GHC 9.6.6, lts-22.43; local `-O2 -Werror` from `stack.yaml`.
  The saved GHC command includes `-O2` and no profiling flags; both measured
  binaries report `rts_thr` and GHC 9.6.6. Node 22.23.2, Spago 0.93.43.
- Orb: Linux 6.1.158+, x86-64 KVM, Intel Xeon at 2.60 GHz, family 6/model 106;
  eight logical CPUs (four cores with SMT), about 16 GiB physical RAM,
  14 GiB workload cgroup limit (15,032,385,536 bytes), no swap, no CPU quota.
- Origin fetch and push were verified as exactly
  `https://github.com/vtrl/purescript`; history was unshallowed before creating
  the new branch from the accepted checkpoint. Only this trial branch was pushed.
- Pinned package set 60.4.0, prepared by the unchanged baseline script with
  Spago 0.93.43: 4,084 PureScript sources, 4,901 source/FFI inputs.
  `inputs.json` SHA-256:
  `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
- Unchanged baseline `ci/benchmark-compiler.py` SHA-256:
  `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
  Setup script SHA-256:
  `2c58732de6df1b38ab77b18e8083225d50dd9ee8a11f9b9c1f8d203f6e3e8c95`.
- All downloads, builds, and tests finished before timing. No concurrent
  build, profiling, or test process ran on this orb during timing. No RTS
  nursery override or compiler-default change was used.
- Each timed process is a full `purs compile --codegen js` over the sorted
  corpus, with `+RTS -N4 -s -RTS`. The output directory is deleted before
  **every** run. Warm means OS page-cache warm, never incremental compilation.
  Each block has a separate excluded full clean warm-up per executable.
  Product hashing runs outside the timed compiler process after every run.

## Correctness

All checks ran with `CI=true`, `HSPEC_ACCEPT` and `GHCRTS` unset, seed 9160,
and no golden acceptance:

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 \
  build purescript:exe:purs --lock-file=error-on-write
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 \
  test --lock-file=error-on-write --test-arguments='--seed=9160 --match=varIfUnknown'
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 \
  test --lock-file=error-on-write --test-arguments='--seed=9160'
```

- Focused tests: **3 examples, 0 failures**.
- Full suite: **1,305 examples, 0 failures**, including all 1,302 baseline
  examples; finished in 136.0198 seconds. `git diff --check` passed and no
  golden files changed.
- Existing passing/failing generalization, polykind generalization/hygiene,
  kind-polymorphic instantiation, and skolem-escape fixtures passed.
- Expected ASTs are written independently with explicit constructors, not
  generated through the function under test. Comparisons use derived `Show`
  because `Type`'s `Eq` deliberately ignores annotations and forall visibility.
  Tests cover distinct multi-digit IDs, named/default/empty prefixes, supplied
  order `[103,17,42]`, generalized kind references, distinct source spans and
  comments, an unknown outside the quantification list, empty quantification,
  existing `TypeVar`, nested forall/skolem/constraint kinds, kind applications,
  rows, operators, parentheses, and relevant read-only substitution state.

## First N4 screen: promising but not sufficient alone

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true python3 ci/benchmark-compiler.py \
  --corpus .build/package-set-benchmark \
  --baseline .build/perf/purs-parse-release \
  --compiler .build/perf/purs-varifunknown-tested-release \
  --capabilities 4 --samples 1 \
  --results .build/perf/varifunknown-n4-screen \
  --label trial-normal-O2-varIfUnknown-vs-accepted-Make
```

One measured B/C pair; one excluded clean warm-up each. Allocation is decimal
GB; RSS is KiB; maximum residency is GC-sampled bytes, not peak RSS.

| Run | Wall s | Peak RSS KiB | Allocated bytes | Max residency bytes |
| --- | ---: | ---: | ---: | ---: |
| Baseline warm-up (excluded) | 109.51 | 1,997,360 | 397,674,075,352 | 678,614,936 |
| Candidate warm-up (excluded) | 111.37 | 1,949,116 | 395,572,873,352 | 670,594,496 |
| Baseline measured | 112.25 | 2,051,628 | 397,641,226,128 | 733,506,504 |
| Candidate measured | 107.40 | 1,948,076 | 395,568,910,328 | 680,998,112 |
| Measured change | −4.321% | −5.047% | −0.521% | −7.159% |

The measured allocation reduction is 2.072 GB. There is no variance estimate
from one measured pair. The warm-ups have the opposite wall direction, so
the initial wall difference cannot be called a proven speedup. This screen
and equality evidence were reported to the lead before starting confirmation.

All four runs produced the same 8,985 products, checked after every repetition;
`products.json` SHA-256 matches the original campaign hash:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
All four saved stdout files contain equal multisets of **745 warnings**
(738 distinct contents). Only `Warning N of M:` header lines and each message's
outer whitespace are removed; message contents and internal whitespace remain.
The SHA-256 of UTF-8 JSON of the sorted messages (`ensure_ascii=False`) is
`ca95051d9e79750fe3b3f3b4138f9dcae16a630a4ce5559076fe41dfc784185d`.
Raw stdout hashes differ because the original compiler's warning order is
already nondeterministic; this is not a content difference.

## Counterbalanced confirmation

Same binaries, corpus, and command as the screen, changing `--samples 4`,
`--results .build/perf/varifunknown-n4-confirmation`, and the descriptive label.
Four measured pairs ran in B/C, C/B, B/C, C/B order after one new excluded clean
warm-up per binary. No N1 measurement. Across both blocks this is five measured
pairs and four excluded warm-up runs, 14 full clean compilations in total.
The confirmation block is summarized independently, without folding in the
favorable screen that caused selection for repetition.

| Pair | Order | Baseline wall s | Candidate wall s | Wall change | Allocation change | RSS change | Residency change |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | B/C | 112.90 | 112.85 | −0.044% | −0.523% | +1.806% | −0.785% |
| 2 | C/B | 112.30 | 114.06 | +1.567% | −0.520% | −0.363% | −5.320% |
| 3 | B/C | 116.53 | 111.50 | −4.316% | −0.518% | −3.414% | −5.212% |
| 4 | C/B | 112.17 | 113.48 | +1.168% | −0.515% | −4.900% | +3.727% |

| Metric | Baseline mean ± sample SD | Candidate mean ± sample SD | Mean paired change ± sample SD |
| --- | ---: | ---: | ---: |
| Wall seconds | 113.475 ± 2.061 | 112.973 ± 1.099 | −0.406% ± 2.695 pp |
| Peak RSS KiB | 2,040,994 ± 31,975 | 2,005,743 ± 61,682 | −1.718% ± 3.014 pp |
| Allocated bytes | 397,636,757,154 ± 7,519,900 | 395,573,267,592 ± 7,034,574 | −0.51894% ± 0.00347 pp |
| Sampled residency bytes | 723,042,612 ± 24,611,606 | 708,649,912 ± 16,660,556 | −1.897% ± 4.304 pp |

SD is sample standard deviation, **not** a confidence interval; pp means
percentage points. Ratios of group means differ from means of paired changes:
wall −0.443%, RSS −1.727%, allocation −0.51894%, residency −1.991%.
Median wall times are 112.60 s baseline and 113.165 s candidate; the median
paired wall change is +0.562%, another reason not to claim a speedup.
Wall ranges are 112.17–116.53 s baseline and 111.50–114.06 s candidate.

Confirmation warm-ups, excluded from all these statistics:

| Binary | Wall s | Peak RSS KiB | Allocated bytes | Sampled residency bytes |
| --- | ---: | ---: | ---: | ---: |
| Baseline | 106.79 | 2,057,748 | 397,632,010,704 | 731,076,736 |
| Candidate | 118.50 | 1,983,996 | 395,587,184,840 | 687,485,720 |

The screen and confirmation product manifests match the original hash, and the harness
checked every repetition. A final cross-block comparison confirms identical
745-warning content multisets (738 distinct contents) in all 14 stdout files,
using precisely the normalization and fingerprint recorded for the screen.
The machine-readable check is `.build/perf/warnings-equality-all.json`.

## Interpretation and remaining risks

- Allocation is the real normal-O2 benefit: all four confirmation pairs save
  0.515–0.523%, with mean saving 2,063,489,562 bytes. It is modest but large
  relative to the approximately 7 MB within-binary allocation SD. No profiled
  timing or allocation artifact is used for this conclusion.
- Wall and RSS differences are small relative to the observed spread and can
  change sign. Four pairs on one orb do not establish equivalence or rule out
  a small regression. Warm-up timing spread is additional evidence of noise.
  Maximum residency is GC-sampled and must not be conflated with peak RSS.
- Pure traversal changes evaluation/sharing behavior. Closures can retain the
  immutable name map until the resulting type is consumed; there is no new
  map cache or state invariant. The corpus and focused tests found no output
  differences, but other workloads/capability counts may behave differently.
- Recommend carrying the small source change and tests into the lead's
  provisional integration checks **for allocation reduction only**, not
  accepting a wall-speed or peak-memory claim. Composition with other workers'
  optimizations and N1 behavior remain unmeasured here. Do not assume gains
  add or merge the campaign to master based on this worker trial.
- This bounded source trial is complete; no alternative helper, naming cache,
  RTS setting, profiling pass, or further local benchmark matrix was added.

## Evidence and transfer

Worker [thread](https://ampcode.com/threads/T-01a08b0b-2da5-711e-b078-b5fb012bdc11).
Use `download_thread_file` for the exact measured candidate and evidence files;
the baseline is already preserved in the lead orb. Build/test/setup logs,
environment and GHC flags, source patch, all timing stdout/stderr and metadata,
per-run product checks, and warning-content equality results are under
`.build/perf/`. The transferable raw evidence bundle is
`.build/perf/varifunknown-evidence.tar.gz`; its adjacent `.sha256` file records
the archive hash. It includes this report, all run data, test/build/setup logs,
source patch, toolchain evidence, and corpus manifests, but not binaries or the
downloaded package sources. Download the exact candidate separately from the
path above; its hash is recorded in both benchmark metadata files.

This report is a separate commit from the provisional source change so the
evidence can be retained without integrating the optimization. The worker
thread remains available for follow-up and is not archived.
