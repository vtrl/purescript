# Forcing parser warnings before logging: additive Make trial

This is an isolated trial, not an accepted compiler change. The accepted
campaign compiler is still [Make lifetime 38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The new branch `perf/parse-warnings-force-20260910` starts at report-only
[3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d),
with compiler source, tests, and build flags identical to that accepted Make
checkpoint. No binding, CST packing, unification, or specialization trial is
included. The candidate source/test commit is
[2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f).

## Hypothesis and narrow change

`Make.buildModule` logs converted parser warnings before entering the `listen`
that forces compiler warnings and externs. `Logger.tell` appends to an IORef;
forcing that append to weak head normal form need not force its right-hand
warning list. The completed job also references parser warnings outside the
later `listen` force. An unevaluated list, even one eventually returning empty,
can close over the parser result/state after compilation finishes.

Change one functional line from `let pwarnings' = ...` to
`pwarnings' <- evaluate . force $ CST.toMultipleWarnings fp pwarnings`.
This happens inside the existing semaphore, after dependency waits, before
`tell`. Parser warnings and parse errors still run when a dependency failed;
no lock is held during dependency waits. The generic logger and CST parser
contracts are unchanged. A timing/RSS result alone cannot prove the exact
retaining root; that remains a mechanistic hypothesis without a retainer profile.

## Correctness and identity

A new asymmetric Make test checks two exact warning locations: one module
skipped because its dependency has a type error, and an independent module
that compiles. Both use deprecated case indentation to produce real parser
warnings. It expects the dependency type error and both warnings with their
original distinct source spans. Moving parser-warning handling into only the
successful-build path would fail this test. The new test passed first against
the unchanged Make compiler (one example, zero failures).

The candidate then passed the normal optimized full suite: **1,303 examples,
zero failures**, 106.7781 s Hspec elapsed. No goldens or fixtures were accepted.
This includes the new warning test, existing failed-dependency parse-error
test, normal parser-warning goldens, and all incremental Make tests.

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

Stack 3.3.1, GHC 9.6.6, lts-22.43, existing locals `-O2 -Werror`. No profiling
or `--fast` build. The baseline-check build recompiled the restored original
`TypeChecker/Monad.hs` after the rejected binding trial, before the candidate
Make rebuild. Candidate source remains Make-only plus this warning force.

- Exact accepted baseline executable `.build/perf/purs-parse-release`, SHA256
  `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
- Tested candidate `.build/perf/parse-warnings-force/purs-candidate`, SHA256
  `92643eb2d1100711e7e8da39168202d76246ae0a6e335edf640bcac7c0d52e1d`,
  preserved read-only after the full suite.
- Both installed stripped executables are 47,869,944 bytes. Incremental
  version metadata can be stale; use source/patch/binary identities.
- Exact source/test patch SHA256
  `8fe311c640a75f031d67152c0e302226d8453c6b6d7abcaaf8c7532ea81375d8`.
- Pinned corpus: 4,084 PureScript sources/4,901 inputs, manifest SHA256
  `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
- Unchanged e926183 paired harness SHA256
  `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.

## N4 screening protocol

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true python3 ci/benchmark-compiler.py \
  --corpus .build/perf/corpus --baseline .build/perf/purs-parse-release \
  --compiler .build/perf/parse-warnings-force/purs-candidate \
  --results .build/perf/parse-warnings-force/n4-screen \
  --label Make38080-vs-parser-warning-force-O2 --capabilities 4 --samples 1
```

One excluded full clean warmup per binary, then one measured B/C pair.
Output is deleted before every compile, even warmups; OS page cache is warm,
not incremental output. Runtime flags are `+RTS -N4 -s -RTS`. No concurrent
builds/tests/profiles or other compute workload runs in this lead orb. Machine:
eight Xeon 2.60 GHz vCPUs, Linux 6.1.158+, 14 GiB cgroup limit, no swap or CPU
quota. Raw metadata preserves exact commands, inputs, and hardware.

| Metric | Accepted Make | Warning-force candidate | Paired change |
| --- | ---: | ---: | ---: |
| Wall seconds | 110.72 | 109.90 | −0.7406% |
| Peak RSS KiB | 1,967,444 | 1,587,744 | −19.2992% |
| Allocated bytes | 397,639,781,464 | 397,626,571,208 | −0.003322% |
| Maximum residency bytes | 683,120,616 | 560,696,616 | −17.9213% |

Excluded warmups: baseline 111.00 s / 1,987,832 KiB RSS / 700,314,680 bytes
residency; candidate 108.89 s / 1,458,720 KiB / 521,457,880 bytes. No variance
estimate is available from one measured pair. Both warmup and measured run
show a material peak-memory signal, but differ in magnitude; wall differences
do not establish a speedup. Total allocation is essentially unchanged.

All four compiles match the original 8,985-product manifest byte-for-byte,
SHA256 `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
All four warning-content multisets match exactly: 745 warnings, 738 distinct
bodies, preserving duplicate multiplicity and all internal whitespace. Only
numbered headers and outer body whitespace are removed. Raw warning ordering
is not a correctness criterion. The paired changes were independently
recomputed from the saved samples, and source/binary hashes reverified.

Decision: proceed to a separate five-pair N4 confirmation block with new
excluded warmups. Keep this favorable selection screen separate from its
confirmation statistics. N1/N8 and relevant boundary checks remain required
before acceptance. No performance effect is yet accepted into the campaign.

Raw screen logs, test/build output, source patch, exact harness, and verification
are preserved in the lead review bundle
`.amp/in/artifacts/performance-20260910/parser-warning-force-screen.tar.gz`.
Working data is under `.build/perf/parse-warnings-force/`; the executable is
preserved separately from later builds. Only this isolated trial/report branch
is pushed; accepted campaign source and master remain unchanged.
