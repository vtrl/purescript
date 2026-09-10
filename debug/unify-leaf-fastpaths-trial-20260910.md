# Five equal-leaf unification guards: uncached compiler trial

**Recommendation: hold as an allocation-reduction candidate; do not integrate
or claim a speedup yet.** The requested one-pair normal-O2 N4 screen saved
1.651% allocation (6.565 GB), but the measured candidate was 2.672% slower.
Warm-up wall/RSS/residency differences had the opposite direction, so these
metrics need counterbalanced verification. No repeat, N1, or additional variant
was run. The source remains provisional and separate from the prior varIfUnknown
trial, with all evidence preserved for lead prioritization.

## Scope and provenance

- Trial branch: [`perf/unify-leaf-fastpaths-20260910`](https://github.com/vtrl/purescript/tree/perf/unify-leaf-fastpaths-20260910).
- Exact campaign starting point:
  [4861c1e1](https://github.com/vtrl/purescript/commit/4861c1e1e418122a172ad4b707f4ba5f8a2c5361).
  Its compiler source is the accepted Make-only
  [38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
  The prior `varIfUnknown` allocation trial is **not** included.
- Source adaptation:
  [09c49076](https://github.com/vtrl/purescript/commit/09c49076095dd8797984812b6e612d00a271aee1).
  Exactly five guarded clauses were added at the entry to `unifyTypes`, before
  `gets checkSubstitution` and `withErrorMessageHint`. No other production code,
  state field, traversal, configuration, or benchmark harness changed.
- Original source: [restaumatic/purescript PR 18](https://github.com/restaumatic/purescript/pull/18),
  commit [e3425f4d](https://github.com/restaumatic/purescript/commit/e3425f4d8fe3e2000b40b15f9c327c632e468b10).
  Original author and committer: **Michal Kozakiewicz <morswin@gmail.com>**,
  both dated **2026-04-29T17:23:49Z**; unsigned, no trailers, no Claude trailer.
  The adapted source commit preserves the author/date and records the exact
  original commit URL in `Adapted-from`. The adaptation's committer is Justin
  Garcia; its Amp attribution is distinct from the original commit's provenance.
- Tests are a separate commit:
  [fe186599](https://github.com/vtrl/purescript/commit/fe186599bf9dd0169a5f68e93bd5f257d41dabb8).
  This evidence report is also separate from the source adaptation.
- Origin fetch and push were checked as exactly `https://github.com/vtrl/purescript`.
  History was already complete from the earlier trial. Only this trial branch
  was pushed; no master, foreign fork, or shared campaign branch was changed.

The five guards retain the existing post-substitution leaf equalities:

```haskell
unifyTypes (TypeConstructor _ c1) (TypeConstructor _ c2) | c1 == c2 = pure ()
unifyTypes (TypeVar _ v1)         (TypeVar _ v2)         | v1 == v2 = pure ()
unifyTypes (TypeLevelString _ s1) (TypeLevelString _ s2) | s1 == s2 = pure ()
unifyTypes (TypeLevelInt _ n1)    (TypeLevelInt _ n2)    | n1 == n2 = pure ()
unifyTypes (Skolem _ _ _ s1 _)    (Skolem _ _ _ s2 _)    | s1 == s2 = pure ()
```

The first four leaf forms have no substitutable children. Skolem kinds can
contain unknowns, but the existing pure substitution traverses those kinds
lazily and the successful skolem comparison inspects only integer identity,
not name, kind, or scope. None of this path mutates substitution state.
Failed guards retain the original substitution/hint/error path. Unknowns and
non-leaf types still follow the original implementation.

**Do not import the foreign −18.6% claim.** The original commit's parent,
[799e8208](https://github.com/restaumatic/purescript/commit/799e8208e35745414a6829c20160c46b53b33e90),
used an `S.Set (Type, Type)` unification cache. The original patch both added
these guards and removed cache membership checks/inserts/state bookkeeping.
That combined result was −18.6% against the cached parent and only −0.4%
against the foreign type-hash/HashSet tip. The local baseline has no such
cache, so only bypassing state lookup, substitution, and hint wrapping is
being evaluated here. No cache-removal changes or history were transplanted.

## Direct baseline equivalence and full correctness

Five focused examples were written first and run against the **unchanged local
baseline**, then rerun after adding the five guards. Both runs passed **5 examples,
0 failures**. Expectations are explicit state/AST/error values, not calls to
the optimized function or outputs copied from a generated golden.

- Four equal-leaf cases vary source spans/comments while preserving the value.
  Every `CheckState` field is compared, including all substitution maps and
  existing hints. `CheckState` has no Eq/Show instance, so tests compare a
  tuple of all fields using derived Show; this also retains annotations that
  the `Type` Eq instance intentionally ignores.
- Five unequal-leaf cases compare full `MultipleErrors` contents and the exact
  unification hint, including same-named constructors from different modules,
  distinct variable/string values, negative versus positive integers, and
  same-named/same-scope skolems with different integer identities.
- A same-ID skolem case varies name and scope, with a nested kind containing
  both a substitution chain and an unsolved unknown on one side and no kind
  on the other. It succeeds without changing any state, before and after.
- An asymmetric application solves two distinct unknowns, one on each side,
  to different concrete types, with independently specified final substitution.
- A non-leaf failure after equal constructor children retains both outer and
  inner unification hints and the exact unequal variable contents.

All verification used `CI=true`, unset `HSPEC_ACCEPT` and `GHCRTS`, seed 9160,
normal O2, and no golden acceptance:

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
# Run once before source adaptation, then again after the candidate build:
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed=9160 --match=unifyTypes'
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed=9160'
```

The full candidate suite passed **1,307 examples, 0 failures**, in 118.3841 seconds:
all 1,302 baseline examples plus five new tests. `git diff --check` passed; no
golden files changed. Baseline/candidate focused and full logs are preserved.

## Exact optimized build and executable identities

Stack 3.3.1, GHC 9.6.6, resolver lts-22.43; normal local `-O2 -Werror` from
`stack.yaml`, with `-O2` also verified in Cabal's saved setup configuration.
No `--fast`, profiling flags, or RTS-default changes were used.

```sh
/usr/bin/time -f '%e %U %S %M' -o .build/perf/unify-leaf/build.time \
  env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 build \
  --test --no-run-tests --lock-file=error-on-write
```

This was an **incremental optimized lib+exe+test build with optimized dependencies
already cached**, not a cold build. It began at 2026-09-10T12:32:17Z and took
23.41 s wall, 21.03 s user, 2.58 s system; peak build RSS 684,280 KiB. The built
executable mtime was 2026-09-10T12:32:40.558327153Z. Build/test/setup work finished
before timing. The executable hash remained unchanged after both candidate tests.

| Identity | Value |
| --- | --- |
| Accepted baseline path | `.build/perf/purs-parse-release` |
| Accepted baseline SHA-256 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| Accepted baseline size | 47,869,944 bytes |
| Tested candidate path | `.build/perf/purs-unify-leaf-tested-release` |
| Tested candidate SHA-256 | `372992fd5346d40d8e509faf3d9b0ac389acbb357c3b11b2e3299a7b24b56444` |
| Tested candidate size | 47,874,040 bytes (+4,096 bytes) |
| Source/tests patch SHA-256 | `338b7e093a4cbe588bfcfe2ab48a0122933a3e234102637c86282d3967808bcb` |

Both binaries are preserved separately with mode 0555. The baseline is the exact
lead-supplied Make binary, not the locally rebuilt baseline used for focused
tests. Baseline TH version metadata remains `6d636561… DIRTY`; candidate TH
metadata remains `38080a40… DIRTY` from the cached build. These strings are stale
identifiers, not additional source changes: use the exact binary hashes, source
commits, and saved patch for identity. The prior varIfUnknown executable remains
preserved separately and was not used in this trial.

## N4 screen controls and results

The orb has eight logical CPUs (four cores with SMT), Intel Xeon 2.60 GHz,
family 6/model 106, x86-64 KVM, Linux 6.1.158+, about 16 GiB physical RAM,
14 GiB workload cgroup limit, no swap and no CPU quota. No competing build,
test, profiling, or benchmark process ran on this orb during timing.

Reused pinned package set 60.4.0, prepared with the baseline setup script and
Spago 0.93.43/Node 22.23.2. Exact corpus: 4,084 PureScript sources, 4,901 inputs;
manifest SHA-256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
The unchanged harness is byte-identical to campaign checkpoint
[e9261835](https://github.com/vtrl/purescript/commit/e9261835), SHA-256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true python3 ci/benchmark-compiler.py \
  --corpus .build/package-set-benchmark \
  --baseline .build/perf/purs-parse-release \
  --compiler .build/perf/purs-unify-leaf-tested-release \
  --capabilities 4 --samples 1 \
  --results .build/perf/unify-leaf/n4-screen \
  --label fe186599-normal-O2-five-leaf-guards-vs-accepted-Make38080a40
```

One excluded full clean warm-up each, then one measured B/C pair. Every run
deletes the output directory and compiles the full sorted corpus with
`--codegen js +RTS -N4 -s -RTS`. Warm means OS page-cache warm, never incremental.
All product hashing runs outside the timed compiler process, after each run.

| Run | Wall seconds | Peak RSS KiB | Allocated bytes | Sampled residency bytes |
| --- | ---: | ---: | ---: | ---: |
| Baseline warm-up (excluded) | 114.64 | 1,983,032 | 397,651,517,096 | 704,384,648 |
| Candidate warm-up (excluded) | 113.06 | 2,045,144 | 391,067,523,432 | 738,714,120 |
| Baseline measured | 115.65 | 2,084,364 | 397,653,600,336 | 731,158,088 |
| Candidate measured | 118.74 | 1,973,596 | 391,088,889,784 | 681,888,176 |
| Measured change | +2.672% | −5.314% | −1.651% | −6.739% |

The measured allocation saving is 6,564,710,552 bytes. The warm-up also reduced
allocation (by 6,583,993,664 bytes), but its wall time improved and its RSS and
residency increased, opposite to the measured pair. Warm-ups are excluded and
are **not** additional samples. With only one measured pair there is no sample
variance estimate, speedup evidence, or reliable bound on a possible wall
regression. GC-sampled residency is not peak RSS. No normal-O2 result here
supports the foreign 18.6% speed claim.

The harness checked all 8,985 products after **every** run. All four runs match
the original campaign product-manifest SHA-256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
All four stdout files contain equal **745-warning content multisets** (738
distinct contents), ignoring only the `Warning N of M:` header lines and each
message's outer whitespace. Internal content and multiplicity are preserved.
The sorted-message JSON fingerprint is
`ca95051d9e79750fe3b3f3b4138f9dcae16a630a4ce5559076fe41dfc784185d`,
matching the earlier Make/varIfUnknown trials as well. Raw stdout hashes differ
because warning ordering was already nondeterministic. Exact checks are saved
in `.build/perf/unify-leaf/warnings-equality.json`.

This complete first screen was reported to the lead before any repetition.
No further timing was launched. The allocation signal makes this worth retaining
for independent counterbalanced verification if prioritized, but does not
justify integrating it despite the measured wall regression. Combining it with
other optimizations, N1/dense workloads, and larger workloads remains unverified.
Correctness tests and corpus equality support the five-clause adaptation; they
do not prove universal performance equivalence or eliminate evaluation-order
effects outside valid, finite compiler state.

## Raw evidence and follow-up

Worker [thread](https://ampcode.com/threads/T-01a08b0b-2da5-711e-b078-b5fb012bdc11).
Raw records are under `.build/perf/unify-leaf/`, including the exact source patch,
baseline/candidate focused tests, full suite, build timing/log, hardware/toolchain
identity, and all screen stdout/stderr/time/metadata/products/samples/summaries.
The measured candidate can be retrieved with `download_thread_file` at the path
above. Keep any integration provisional until independent measurements and
combined campaign checks; no master merge is authorized by this worker trial.

Transfer bundle: `.build/perf/unify-leaf-evidence.tar.gz`, with adjacent
`.sha256` file. It contains this report, the full raw trial directory, and corpus
manifests, but not the compiler binaries or downloaded package sources. Extract
into a new directory to preserve existing lead files. The exact candidate is
available separately at `.build/perf/purs-unify-leaf-tested-release`.

Source adaptation, tests, and report are separate commits, so the evidence can
be retained without integrating the source. The worker remains unarchived for
follow-up; no work on the earlier allocation-only candidate was overwritten.
