# External compiler-performance candidates: provenance and trial boundaries

These are research leads, not accepted performance claims. The campaign's
original baseline is [9160ce15](https://github.com/vtrl/purescript/commit/9160ce1518b5f11f9ebe32b445019f8dbb4f435a).
Its only accepted compiler change is [Make lifetime 38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The three independent trials below start from exact current campaign
[4861c1e1](https://github.com/vtrl/purescript/commit/4861c1e1e418122a172ad4b707f4ba5f8a2c5361),
whose compiler, tests, and build configuration are source-equivalent to that
accepted Make checkpoint. Later evidence-only commits need no trial rebase.

Read-only research covered relevant branches and pull requests of
[OxfordAbstracts/purescript](https://github.com/OxfordAbstracts/purescript) and
[restaumatic/purescript](https://github.com/restaumatic/purescript). No foreign
tip is imported wholesale. Only `vtrl/purescript` campaign branches may be
pushed; master is unchanged. Original authorship and actual coauthor trailers
must accompany adapted source commits. Claude attribution is verified per
commit, not inferred from a repository or PR description.

## Two INLINABLE pragmas: small independent source trial

- Source: [Restaumatic PR19](https://github.com/restaumatic/purescript/pull/19),
  [b831b298](https://github.com/restaumatic/purescript/commit/b831b2983b570550af52db1e88a53fe99f8fb588),
  parent `c84101d81a9c0ef5ec1b1eb2b60629ff7f4ba820`.
- Branch `traversal-inline` points to that source commit; PR merged into
  foreign branch `restaumatic` as `d3f69f9b30aa3dcf419b43b83eee288d9f21fe57`.
- Author/committer: Michal Kozakiewicz (`kozak`, `morswin@gmail.com`),
  2026-05-08. Actual trailer:
  `Co-authored-by: Claude Opus 4.7 (1M context) <noreply@anthropic.com>`.
- Exact change: `INLINABLE everywhereOnValuesTopDownM` and
  `INLINABLE everywhereOnValuesM` in `AST/Traversals.hs`; two added lines.
  Neither pragma exists in our original or accepted compiler source.

Foreign evidence: proprietary pr-admin corpus, 1,758 modules, interleaved
median-of-four after warmup. Two reported full-build comparisons were −9.6%
and −8.9%; nochange +8.4%/−3.0%, prelude −0.5%/+1.6%, leaf −4.3%/−1.5%.
The author attributed the first nochange regression to load contamination;
that explanation is not independently established here. Reported binary size
48,625,952→48,839,456 bytes (+0.4%); 1,340 tests passed. The foreign maintainer
warned that broader applicability needs measurement.

This is complementary to Make object-lifetime changes, but overlaps the
specialization mechanism of global flags below. GHC version, callers, code
growth, and specialization decisions can change the effect. Related foreign
branch [`typecheckm-specialization`](https://github.com/restaumatic/purescript/tree/typecheckm-specialization)
at `cbf770675050ff234e0063bb36c9092a4ad322b7` pursues overlapping work and is
not part of this trial.

Assigned trial: `perf/traversal-inlinable-20260910`, owns only the two pragmas
and an evidence report. Do not combine with global flags until isolated
measurements establish whether both merit an interaction test.

## Global specialization flags: Oxford imported an upstream change

- Original source: [upstream PR4584](https://github.com/purescript/purescript/pull/4584),
  [8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
- Author: `seastian <seastian@users.noreply.github.com>`, 2025-10-18;
  GitHub/web-flow committer, verified GitHub signature, no coauthor trailer.
  No Claude authorship evidence was found for this commit.
- [Oxford PR16](https://github.com/OxfordAbstracts/purescript/pull/16) is a
  downstream upstream-master sync, opened by `purefunctor`, merged by
  `noisyscanner` as `688fed5f…`. It is not original optimization authorship.
  Its imported [source commit](https://github.com/OxfordAbstracts/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064)
  has parent `9dd761a3805a0c04b90db915599c1c6d8a3bb68e`.
- Exact meaningful patch: append `-fspecialize-aggressively
  -fexpose-all-unfoldings` to local GHC options in `stack.yaml`, and add
  the same options for `package purescript` in `cabal.project`. Unrelated
  Cabal indentation changes are excluded. Neither flag exists in our baseline.

Foreign evidence: displayed ACME clean-build pair, `+RTS -N` resolving to N10,
wall 52.155→36.887 s (−29.3%), allocation 448.762→244.193 GB (−45.6%), copied
86.001→75.082 GB, maximum residency 1.390→1.410 GB (slightly worse), total
memory 3,891→3,888 MiB. Binary size reportedly grows 110→130 MB, with longer
compiler build time. A reviewer reported roughly 30%/40% on two work codebases,
without controlled raw series. These are not our corpus, GHC lineage, or
counterbalanced measurements; no product-equivalence evidence was supplied.

The flags are a duplicate of the upstream change, not of anything accepted
in this campaign. They are algorithmically separate from Make but may make
explicit traversal pragmas redundant. `cabal.project` is not normally in a
Hackage sdist: this configuration targets repository/release builds rather
than universally changing downstream library builds.

Assigned trial: `perf/specialization-flags-20260910`, owns only the two config
files and report. Trial both exact flags together, not an unbounded flag matrix.
Record compiler build time/peak RSS and tested binary size as well as runtime.

## Equal-leaf unification: adapt guards, not the foreign cache history

- Source: [Restaumatic PR18](https://github.com/restaumatic/purescript/pull/18),
  [e3425f4d](https://github.com/restaumatic/purescript/commit/e3425f4d8fe3e2000b40b15f9c327c632e468b10),
  parent `799e8208e35745414a6829c20160c46b53b33e90`.
- Author/committer: Michal Kozakiewicz (`morswin@gmail.com`), 2026-04-29;
  no coauthor trailer on the named commit. Do not invent Claude authorship.
- PR18 contains two commits; branch `unify-leaf-no-hash`/PR head is
  `35ad956a3c5e0195f5a2282b8b2cd78c7cfc12f8`, not the named patch.
  Merge into foreign `restaumatic` is `a79c297196f3dc2827a4c4f28762a3d8c886bfa8`.
- Named patch adds entry guards for equal constructors, variables, type-level
  strings, integers, and skolem IDs. It also removes Set cache lookups/inserts.
  **Our baseline has no such unification cache.** Adapt only the five guards;
  there is no local cache deletion to reproduce.

Foreign evidence: pr-admin 1,758 modules, median-of-six clean runs. Relative
to the Set-cache parent, full −18.6%, nochange +3.9%, prelude −0.9%, leaf −0.1%.
Relative to foreign shipped HashSet/type-hash `5713e832…`, full −0.4%, nochange
−1.4%, prelude +1.4%, leaf +2.6%. The author reports 86% of cache hits were
one/two-node types and removing about 488 KB of hash/cache code. All 1,340
tests passed. These comparisons do not isolate the guards on our cache-free
compiler, and median-of-six is not a six-pair counterbalanced experiment.

The guards bypass substitution and the error-hint bracket for known-equal
leaves. Our current post-substitution code already uses those equalities.
Skolem is subtle: it carries an optional kind containing unknowns, but the
existing success clause checks integer identity, not name/kind/scope. Direct
tests must preserve the existing outcome/state for that case, and ensure
unequal leaves retain error contents/hints and unknown solving remains intact.

This overlaps same-file work but is not a duplicate of read-only
`varIfUnknown`: one changes unification admission, the other generalization
traversal. Keep them separate while measuring. Competing foreign branches
include [`type-hash`](https://github.com/restaumatic/purescript/tree/type-hash)
(`43f6b613025e961732453255240ea3dab13f8038`),
[`unify-cache`](https://github.com/restaumatic/purescript/tree/unify-cache)
(`98600b486b3a7c9848e310feda82b53253bf2b44`),
[`unify-lazy-subst`](https://github.com/restaumatic/purescript/tree/unify-lazy-subst),
and [`ptr-eq-unify-ship`](https://github.com/restaumatic/purescript/tree/ptr-eq-unify-ship)
(`b9fcf10…`). Cache/hash alternatives conflict with cache-removal history;
lazy substitution and pointer equality overlap the same fast path. None is
implicitly imported. Pattern-survey instrumentation is diagnostic only.

Assigned trial: `perf/unify-leaf-fastpaths-20260910`, owns `Unify.hs`, direct
`TestAst.hs` cases, and report. No prior `varIfUnknown` source change included.

## Other leads stay deferred rather than expanding the trial matrix

The initial branch survey also identified lazy substitution, row-prefix
unification, TypeFlags, pointer equality, incremental Rock, and interning
work. Lazy substitution changes the same unifier and needs a separate semantic
review; row-prefix work targets unusually wide records. Broad representation,
unsafe identity, and incremental-system changes are not justified by the
current bounded evidence. Oxford nursery tuning overlaps the campaign's
already-rejected nursery direction. These are research leads, not validated
patches or assumed additive wins.

## Shared acceptance protocol

Use normal optimized Stack 3.3.1/GHC 9.6.6/lts-22.43, full seeded tests with
`CI=true` and `HSPEC_ACCEPT`/`GHCRTS` unset. No fixture/golden acceptance.
First compare to exact accepted Make executable SHA256
`7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`:
pinned 4,084-module/4,901-input corpus, unchanged e926183 paired harness,
N4 excluded warmup each plus one measured pair. Verify all 8,985 original
products and complete 745-warning content multisets, including duplicates.
All build/setup/test work finishes before timing in each orb.

Report the screen before expensive repetitions. Positive candidates need
repeated within-machine paired measurements, relevant N1/N8/boundary checks,
and lead additive correctness/measurement before acceptance. Measure any
specialization interaction rather than adding standalone percentages. Preserve
negative trials and outliers. Five current orbs, no new workers, below the
eight-Ultra-orb ceiling. No external-fork performance claim is accepted yet.
