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

## N4 confirmation was interrupted after three complete pairs

The original five-pair confirmation did not finish. At the next inspection,
neither the harness nor compiler process remained; Amp's process had restarted
around the interruption. `candidate-4.stderr` ends during module 2,802 of
4,084, its stdout/time files are empty, and `summary.json` does not exist. The
command's exit status is unavailable. This is an interrupted block, not a
completed five-pair result or a performance outlier to discard.

All original logs remain intact, including the incomplete fourth candidate.
The three complete pairs, in B/C, C/B, B/C order, are:

| Pair | Baseline wall s | Candidate wall s | Baseline RSS KiB | Candidate RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| 1 | 111.82 | 110.07 | 2,041,264 | 1,446,308 |
| 2 | 109.31 | 107.78 | 2,046,488 | 1,473,784 |
| 3 | 110.74 | 106.52 | 1,966,344 | 1,400,080 |

Descriptive statistics of only these complete pairs: mean paired wall
−2.2585% ± 1.3468 pp sample SD, RSS −28.6430% ± 0.5961 pp, residency
−27.0666% ± 2.1774 pp, allocation +0.002113% ± 0.009705 pp. These must not
be relabeled as the prespecified five-pair confirmation. All eight completed
compiles, including warmups, passed product checks and their 745-warning
content multisets independently match the original baseline transcript.
The partial fourth run has no correctness or timing claim.

A fresh, separate five-pair N4 block is assigned on `a1.xxlarge`, under the
user's new ceiling of 16 total orbs and latest requirement of xxlarge only for
new workers. Fixed `-N4`, exact binaries, corpus, and counterbalancing remain
unchanged. Do not pool its timings with the interrupted smaller-orb block.
Long blocks now use managed services with an atomic run-once guard and recorded
exit status. Services restart even after success, so the wrapper stays idle
after completion until collection rather than accidentally rerunning samples.

## Independent N1, N8, and boundary evidence

The independent full-corpus N1 screen completed one measured pair after
excluded warmups: 246.83→243.38 s (−1.3977%), RSS 1,809,712→1,224,052 KiB
(−32.3621%), residency −27.1378%, allocation −0.001153%. It supports a
memory signal, not an established speedup. Its dense-1000 N1 five-pair gate,
plus two excluded warmups (12 total compiles), found mean paired wall
−0.7806% ± 5.0145 pp sample SD, effectively unchanged RSS/residency, and only
+432 allocated bytes. Local products and empty diagnostics matched.

The final N1 audit passed all four package runs against the original
8,985-product manifest and exact 745-warning content multiset. The lead
independently checked all 16 raw run logs, fixed N1 RTS settings, complete
module progress, measured GNU-time/RTS values against sample rows, paired
order, warning multiplicities, and saved manifest/harness identities. The
worker's [report-only checkpoint](https://github.com/vtrl/purescript/commit/ca508b102588569881386200cb9f5f0977cddd2a)
contains no compiler changes. Preserved archive `parser-warning-force-n1-dense.tar.gz`
has SHA256 `c322360672137ddff2b4c57438f889c7eba614a0e7569b636b4ebbba00bec578`.

Independent N8 three-pair verification completed, with wall pairs
71.84→70.74, 73.31→71.27, 75.51→72.82 s. Mean paired wall change is
−2.6254% ± 1.0247 pp sample SD, RSS −25.0243% ± 1.1587 pp, residency
−24.0960% ± 1.6376 pp; allocation is essentially unchanged. Every pair
improved wall/RSS/residency, and all eight compiles including warmups match
8,985 original products and 745 warning contents.

Tiny Sequence ten-pair blocks at N1/N4/N8 completed with all 66 compiles
matching 135 products and three warning contents. Mean paired wall changes:
N1 +2.9807% ± 6.8193 pp, N4 −2.1867% ± 5.4516 pp, N8 +0.7208% ± 4.8286 pp.
RSS changes are small and mixed. N1 mean wall increased by 33 ms; keep this
qualification rather than asserting latency equivalence or universal speedup.
Boundary archives are `parser-warning-force-n8.tar.gz` and
`parser-warning-force-tiny.tar.gz` in the lead review artifacts directory.

The candidate remains unaccepted pending the fresh N4 block and collection of
the independent correctness/evidence audit. No compiler changes were made during measurement,
and no rejected visibility, unification, or specialization variant was mixed in.

## Audit scope and diagnostic heap evidence

An independent audit recomputed the original screen and N8 results directly
from raw time/RTS files, without importing or executing the supplied harness.
All 12 completed runs have 4,084 distinct module progress entries, expected
RTS capabilities, and complete matching sample values. Warning multiplicity
is 737 bodies once and one body eight times, equal to the original transcript.

Important coverage limit: the corpus's 745 warnings contain **zero
`WarningParsingModule` diagnostics**. They verify unchanged other diagnostics
and the empty parser-warning path, not nonempty parser warnings. The new
asymmetric Make test and the independent focused source checks cover that
separate behavior. The independent optimized source build passed the full
seed-9160 suite (1,303/0), separate default-RTS Make subset (14/0), and five
focused warning/error/exception cases (5/0), with no tracked source or golden
changes. Its executable has a different hash from the supplied timing binary;
source identity does not imply byte identity. The existing
non-threaded test executable cannot support explicit N1/N8 test claims.

Each retained package product map equals the original 8,985-product map.
Per-run product equality relies on the unchanged harness's successful checks
before emitting each row; archives do not contain separate per-run product
trees. Binary provenance in those archives is recorded metadata, while the
lead separately preserves and hashes the actual supplied executables. Warning
canonical hashes from different workers use different serializations; compare
the complete multisets rather than equating those digest strings.

Exactly one separate normal-binary N1 type-heap diagnostic completed with
`-s -hT -i1 -l-au`, matching all original products and warning contents. Against
the saved accepted-Make profile, total sampled peak was 589.75→407.72 MiB,
independent CST peak 80.02→53.34 MiB, and near-end CST 51.20→0.36 MiB.
The final tenth by nonempty sample count averaged CST 53.39→3.49 MiB;
the candidate tail retained its 15.53 MiB excursion. Thus the observed late
51–55 MiB plateau is absent in this run, but CST is not eliminated.

This is one instrumented comparison across orbs, not a timing result or proof
of exact retaining roots. Sampling/GC/phase positions differ. Its role is
supporting heap-shape evidence only; normal-build gates still govern acceptance.
Raw heap/eventlog/argv/manifests/full series and independent analysis remain in
`.build/perf/warning-force-worker-heap-evidence.tar.gz`, SHA256
`5a98343b3b983577742251088a5a8c72654c4e42d22a04b8df14f5d67d88b93c`.
