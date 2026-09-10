# Equal-leaf unification: reopened binary-only xxlarge validation

**Recommendation: HOLD for campaign acceptance.** Both prespecified blocks
completed without interruption and passed product and diagnostic equality.
Allocation decreased consistently, but the wall-time observations do not
establish a stable speed improvement. Dense peak RSS and sampled residency
increased modestly. Keep the attributed source isolated; this worker does not
integrate it or propose/run full-corpus N1/N8 expansion.

This is a separate confirmation block, not a continuation, replacement, or
pooled analysis of an earlier screen. No outliers were dropped, no sample was
rerun, and no adaptive repetitions were added.

## Pinned source and executable identity

- Repository fetch and push URLs were verified exclusively as
  `https://github.com/vtrl/purescript`. History was unshallowed before history
  operations. Only `origin` was fetched; no foreign fork tips were imported.
- Report-only branch: `perf/leaf-validation-xxlarge-20260910`, created at exact
  [3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d)
  after fetching `perf/linux-package-set-20260910`. It was not moved to a later
  campaign tip. Compiler source and build configuration at this checkpoint
  are identical to accepted Make-only
  [38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
- Baseline came from the
  [lead thread](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c),
  `.build/perf/purs-parse-release`, saved locally as
  `.build/leaf-validation-xxlarge/purs-baseline`.
- Candidate came from the
  [original worker](https://ampcode.com/threads/T-01a08b0b-2da5-711e-b078-b5fb012bdc11),
  `.build/perf/purs-unify-leaf-tested-release`, saved locally as
  `.build/leaf-validation-xxlarge/purs-candidate`.
- Both executables remained mode 0555, with the supplied SHA256 verified before
  and after timing. Neither was rebuilt. Supplied build provenance is normal
  O2, GHC 9.6.6, Stack 3.3.1, lts-22.43. Both binaries independently report
  GHC 9.6.6 and `rts_thr` through `+RTS -N1 --info -RTS`.
- Their embedded development version strings contain old `DIRTY` commit
  metadata; executable identity is the supplied SHA256, not that stale string.
  Binary-to-source/O2 correspondence relies on the original worker's build
  provenance; this binary-only assignment does not independently reproduce it.
- Candidate source
  [09c49076](https://github.com/vtrl/purescript/commit/09c49076095dd8797984812b6e612d00a271aee1)
  adapts only five equal-leaf guards (constructor, variable, type-level string,
  type-level integer, skolem) from restaumatic/purescript PR 18,
  [e3425f4d](https://github.com/restaumatic/purescript/commit/e3425f4d8fe3e2000b40b15f9c327c632e468b10).
  Author: Michal Kozakiewicz <morswin@gmail.com>. The original has no Claude
  trailer. The attributed adaptation is already a separate commit and includes
  an Amp co-author trailer. No `varIfUnknown` change or unification cache is
  included. The exact attributed patch is archived in `provenance/`.
- Tests are separate at
  [fe186599](https://github.com/vtrl/purescript/commit/fe186599bf9dd0169a5f68e93bd5f257d41dabb8).
  Original worker results supplied to this assignment: full 1307/0 and focused
  baseline/candidate 5/0. These are inherited evidence, not tests rerun here.
- This worker made no compiler, source, test, or build-configuration changes.

| Identity | SHA256 |
| --- | --- |
| Baseline executable | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| Candidate executable | `372992fd5346d40d8e509faf3d9b0ac389acbb357c3b11b2e3299a7b24b56444` |
| Baseline `Unify.hs` | `a74f8dabd88938d1f7e6a536863c59060f0d3044f77af4a0d895366c25ccb172` |
| Candidate `Unify.hs` | `b912f367a7582498c219422c2e51d58f8f9afb5752f56ce7e5ae602ab5284074` |
| Unchanged `ci/benchmark-compiler.py` | `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399` |
| Full `inputs.json` | `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| Full `products.json` | `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed` |
| Exact Dense1000 source | `408e5f107f6b2202d3efb86a2660b089030545a3faef54b3abf9d7b021d9f5a7` |
| Local dense `products.json` | `1e9864aee02a61c0520d1c3624cf92caf82c64ae1328df731edfb8e8c0fa8ec1` |
| Downloaded dense reference archive | `f669d9970eea4e687b56be8b9dc222a56afe8bb47a6e4301c07024a218e922d6` |

The harness also matches its exact contents at
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b).

## Environment and fixed controls

Provisioned a1.xxlarge orb: Linux 6.1.158+, x86-64 KVM, Intel Xeon Processor
at 2.60 GHz, family 6/model 106/stepping 6. It exposes 16 logical CPUs,
8 cores with 2 threads/core, one socket; effective CPUs 0–15. MemTotal is
32,880,800 KiB; swap is zero. Workload cgroup
`/amp.slice/amp-workload.slice` has `cpu.max=max 100000` and
`memory.max=32212254720` (30 GiB). Before/after snapshots show zero CPU
throttling and OOM events. Full CPU identity, cgroup hierarchy, memory,
process-list and kernel snapshots are archived. No CPU affinity was imposed;
RTS capabilities, not available host CPU count, were fixed at N4/N1. Host
scheduling/noise cannot be excluded by absence of cgroup throttling.

All downloads, dependency preparation, scripts, and checks needed to start
both blocks finished before timing. Spago 0.93.43 ran under Node 22.23.2.
The unchanged `ci/prepare-package-set-benchmark.sh` prepared package set
60.4.0 pinned to registry
`5d834cd364da1d49a1bd1b0219ab49fb15f20601`: exactly 4,084 PureScript sources
and 4,901 source/FFI inputs, with the expected input hash above. Setup is not
timed. Dense inputs were extracted into a NEW ignored directory from the
lead's `.amp/in/artifacts/performance-20260910/binding-dense-regression.tar.gz`.
Only the exact Dense1000 inputs were copied to a fresh local corpus directory;
prior archive contents remain preserved, not overwritten. Dense functions
were compiled only; their recursive cycle was never executed.

Compilations were sequential in an otherwise idle orb, apart from ordinary
Amp/system processes and lightweight status reads. No other build, test,
benchmark, or dev service overlapped. Both blocks use warm OS caches but full
clean compiles: the harness deletes output before EVERY run, including each
warmup. Codegen is `js`; RTS is `+RTS -N4 -s -RTS` or `+RTS -N1 -s -RTS`,
never bare `-N`, with no extra RTS tuning. `LC_ALL=C`, `LANG=C`, and empty
`GHCRTS` are set by the unchanged harness. Hashing runs outside compiler timing.

Each block ran once through a managed service without a portal. The ignored
`run-once.sh` wrapper atomically creates its per-block state directory; a
preexisting guard refuses to repeat and idles. On completion it records the
exit status, then `exec sleep infinity`. Thus service auto-restart cannot add
samples. Both blocks completed exit 0, without interruption/restart, then
their result, status and logs were collected before the service was stopped.
No services remain running. The earlier CLI-restart interruption reported by
the lead occurred elsewhere, not in these blocks.

N4 service: 13:19:18–13:41:14 UTC on 2026-09-10. Dense service:
13:42:32–13:42:52 UTC. N4 has 5 measured BC/CB/BC/CB/BC pairs plus one
excluded clean warmup per binary. Dense has 10 measured alternating pairs
starting BC plus one excluded clean warmup per binary. Total: 34 clean
compiles, 30 measured and 4 excluded warmups.

## Reproduction commands and equality audit

Run from `/home/user/workspace/repo`; `W` and `E` below expand only paths.
The exact executed commands, including labels, are in each `*-state/command.sh`.
Result paths must be fresh; do not rerun these existing blocks.

```sh
W=.build/leaf-validation-xxlarge
E=.amp/in/artifacts/leaf-validation-xxlarge-20260910
export PATH="$PWD/$W/bin:$PWD/$W/tools/node_modules/.bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
GHCRTS=-N1 bash ci/prepare-package-set-benchmark.sh .build/package-set-benchmark

python3 ci/benchmark-compiler.py --corpus .build/package-set-benchmark \
  --baseline "$W/purs-baseline" --compiler "$W/purs-candidate" \
  --samples 5 --capabilities 4 --results "$E/n4" --label '[pinned identities]'
python3 ci/benchmark-compiler.py --corpus "$W/dense1000" \
  --baseline "$W/purs-baseline" --compiler "$W/purs-candidate" \
  --samples 10 --capabilities 1 --results "$E/dense1000-n1" --label '[pinned identities]'

# Actual launch used these guarded managed services, not the direct commands:
amp orb service start leaf-validation-n4 \
  --command 'bash /home/user/workspace/repo/.build/leaf-validation-xxlarge/run-once.sh n4' \
  --cwd /home/user/workspace/repo
# Only after collecting and stopping N4:
amp orb service start leaf-validation-dense \
  --command 'bash /home/user/workspace/repo/.build/leaf-validation-xxlarge/run-once.sh dense1000-n1' \
  --cwd /home/user/workspace/repo

python3 .build/leaf-validation-xxlarge/analyze.py
# n4 PASS: 12 clean compiles; 8985 products; 745 warnings/run
# dense1000-n1 PASS: 22 clean compiles; 2 products; 0 warnings/run
```

Every compile's products were hashed and compared by the unchanged harness.
It retains one shared manifest because all repetitions match, and would write
a per-run mismatch manifest and fail otherwise. N4's 8,985 codegen/externs
products match the original manifest downloaded directly from lead
`.build/perf/baseline-n1-warm/products.json`.

Every N4 stdout has the same CONTENT multiset of 745 warning bodies (738
distinct) as lead `.build/perf/baseline-n1-warm/1.stdout`. Only numbered
`Warning N of M:` headers and outer body whitespace are removed. All internal
whitespace and multiplicity remain. Raw ordering/hashes may differ; warning
count alone is not the check. The audit parser's checks include duplicate
bodies and unequal interior spacing. Original transcript SHA256 is
`6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280`;
the archived sorted body-to-count JSON SHA256 is
`63f52eb35b1b495c3c26c756b9376f6776c01a723b4bc32de907bd8ff21f9fac`.

All 22 dense compiles match both local `Dense/index.js` and `Dense/externs.cbor`
exactly; stdout is empty, and pre-RTS stderr is identical. Cross-orb dense
externs embed source paths, so no comparison against another orb's externs
was used to assert a code regression. Local product hashes are archived.
These checks validate compilation and diagnostics, not runtime behavior or
every possible unification boundary.

## Five-pair N4 evidence

Each change is computed within pair as 100 × (candidate / baseline − 1).
Reported SD is the sample SD of those percentage changes, in percentage
points (pp), not a confidence interval or ratio of group means.

| Metric | Paired mean % | Sample SD pp | Paired median % | Paired range % |
| --- | ---: | ---: | ---: | ---: |
| Wall | -2.207977 | 3.182574 | -3.460998 | -5.890102 to +2.219551 |
| Peak RSS | +0.505953 | 2.534126 | +1.739807 | -3.126646 to +3.062607 |
| Allocation | -1.651331 | 0.003453 | -1.650157 | -1.656301 to -1.647800 |
| Sampled residency | +0.967230 | 4.397976 | +0.270463 | -3.474027 to +7.652666 |

| Pair/order | B wall s | C wall s | Wall change % | B RSS KiB | C RSS KiB | B allocated bytes | C allocated bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 BC | 106.28 | 100.02 | -5.890102 | 2000452 | 2035256 | 397643755000 | 391057576448 |
| 2 CB | 110.04 | 109.74 | -0.272628 | 1933908 | 1993136 | 397642841384 | 391081110320 |
| 3 BC | 113.84 | 109.90 | -3.460998 | 2035280 | 1971644 | 397652420800 | 391094868344 |
| 4 CB | 108.13 | 110.53 | +2.219551 | 2029032 | 2007532 | 397653552880 | 391079021208 |
| 5 BC | 111.12 | 107.08 | -3.635709 | 1996012 | 2034208 | 397639569680 | 391087264208 |

N4 sampled-residency B/C bytes, pairs 1–5:
686998856/704633704, 708339384/683731480, 705237672/707145080,
698578440/683350176, 701696560/755395056.

| Metric | B mean | C mean | B median [range] | C median [range] |
| --- | ---: | ---: | --- | --- |
| Wall s | 109.882 | 107.454 | 110.04 [106.28, 113.84] | 109.74 [100.02, 110.53] |
| RSS KiB | 1998936.8 | 2008355.2 | 2000452 [1933908, 2035280] | 2007532 [1971644, 2035256] |
| Allocation B | 397646427948.8 | 391079968105.6 | 397643755000 [397639569680, 397653552880] | 391081110320 [391057576448, 391094868344] |
| Residency B | 700170182.4 | 706851099.2 | 701696560 [686998856, 708339384] | 704633704 [683350176, 755395056] |

Excluded N4 warmups B/C: wall 109.31/108.45 s, RSS 1937892/2008340 KiB,
allocation 397630007464/391094295056 B, residency 682516128/699892648 B.
Four measured pairs have lower candidate wall time and one higher. All three
BC pairs improve, whereas the two CB pairs are near flat or slower; ordering
effects remain plausible. This observation does not change the fixed analysis.

## Ten-pair Dense1000 N1 evidence

| Metric | Paired mean % | Sample SD pp | Paired median % | Paired range % |
| --- | ---: | ---: | ---: | ---: |
| Wall | -3.191718 | 7.764519 | -1.183473 | -25.000000 to +2.409639 |
| Peak RSS | +0.853786 | 0.077109 | +0.853149 | +0.706071 to +0.950802 |
| Allocation | -1.672170 | 0.000055 | -1.672153 | -1.672328 to -1.672153 |
| Sampled residency | +0.629646 | 0 | +0.629646 | +0.629646 to +0.629646 |

| Pair/order | B wall s | C wall s | Wall change % | B RSS KiB | C RSS KiB |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 BC | 0.85 | 0.84 | -1.176471 | 145028 | 146052 |
| 2 CB | 0.87 | 0.87 | 0.000000 | 144904 | 146088 |
| 3 BC | 0.84 | 0.83 | -1.190476 | 144944 | 146104 |
| 4 CB | 0.83 | 0.85 | +2.409639 | 144776 | 146096 |
| 5 BC | 0.83 | 0.82 | -1.204819 | 144912 | 146116 |
| 6 CB | 0.84 | 0.83 | -1.190476 | 144936 | 146088 |
| 7 BC | 1.20 | 0.90 | -25.000000 | 144780 | 146120 |
| 8 CB | 0.93 | 0.92 | -1.075269 | 144840 | 146108 |
| 9 BC | 0.88 | 0.87 | -1.136364 | 144720 | 146096 |
| 10 CB | 0.85 | 0.83 | -2.352941 | 144840 | 146180 |

Dense allocation B/C is 1186540272/1166699504 bytes for every pair except
pair 9 baseline, which is 1186542384 bytes. Sampled residency B/C is exactly
59256120/59629224 bytes in every pair. Raw individual records also include
user/system CPU times and monotonic elapsed time.

| Metric | B mean | C mean | B median [range] | C median [range] |
| --- | ---: | ---: | --- | --- |
| Wall s | 0.892 | 0.856 | 0.85 [0.83, 1.20] | 0.845 [0.82, 0.92] |
| RSS KiB | 144868 | 146104.8 | 144872 [144720, 145028] | 146100 [146052, 146180] |
| Allocation B | 1186540483.2 | 1166699504 | 1186540272 [1186540272, 1186542384] | 1166699504 [1166699504, 1166699504] |
| Residency B | 59256120 | 59629224 | 59256120 [59256120, 59256120] | 59629224 [59629224, 59629224] |

Excluded dense warmups B/C: wall 0.91/0.87 s, RSS 144904/145932 KiB,
allocation 1186540272/1166699504 B, residency 59256120/59629224 B.
The large pair-7 difference remains in every reported statistic. Most other
wall differences are only one or two 0.01-second timer increments. The short
fixture and high paired dispersion limit speed conclusions. The consistent
RSS increase is about 1,237 KiB on average, not a memory improvement.

## Earlier screens and limits remain separate

The previously supplied N4 one-pair screen was 115.65→118.74 s (+2.67%),
allocation 397653600336→391088889784 B (-1.65%), with opposite warmup wall
direction. The earlier five-short-pair dense screen had mean paired wall
+5.269%, sample SD 16.162 pp, median +15.8%, RSS +0.824%, allocation -1.672%.
Those screens did not establish a win or a reliable regression. Their trial
was closed without acceptance, then reopened at the user's explicit request.
They are not relabeled or pooled here. No wall means are compared across orb
sizes. The downloaded binding archive supplies dense inputs, not additional
equal-leaf samples to mix into this analysis.

The current blocks repeat the allocation reduction while preserving observed
compiler products and warnings. They do not establish a general compiler
speedup, runtime equivalence, or behavior at all capabilities. Full-corpus
N1/N8 were not run. The original Haskell suite was not rebuilt or rerun here.
Hold acceptance rather than claim a win or reject from noisy timing screens.

## Raw evidence layout

All paths below are relative to the extracted evidence directory. Binaries and
corpora are excluded from the review archive; immutable originals remain in
ignored `.build/leaf-validation-xxlarge/` in this worker.

- `n4/`, `dense1000-n1/`: every stdout, stderr and GNU time file; metadata;
  shared product manifest; ordered samples, paired changes, harness summaries,
  and `audit-analysis.json` with every per-run audit/hash and full-precision
  statistics (including group sample SDs).
- `n4-state/`, `dense1000-n1-state/`: atomic started guard, timestamps, exact
  command, completion status, full harness console log, service status/logs,
  and before/after process/cgroup snapshots.
- `analysis.json`, `audit.log`: combined analysis and decisive audit output.
- `reference/`: original full product manifest, original warnings transcript,
  and content-preserving warning body multiset.
- `manifests/`: pinned full and dense input metadata, without corpus files.
- `setup/`: installation/preparation logs, binary/source/harness hashes,
  versions, CPU/memory identity, and executable RTS info.
- `scripts/`: exact unchanged harness/preparation script, run-once wrapper,
  and independently inspectable audit/statistics script.
- `provenance/`: separately attributed source and test patches, plus the empty
  checkpoint/compiler diff proving the pinned report checkpoint is Make-only.

The audit script uses the worker's absolute evidence path. To review elsewhere,
extract at that path or change only its `ROOT` constant in an analysis copy;
do not change any archived raw logs. The shared manifest and exit-0 harness
completion are the evidence that every run passed product comparison.

## Evidence transfer

Source: [validation worker thread](https://ampcode.com/threads/T-01a08b6f-47e8-7317-a4e4-2a090a7927e4).
Download with `download_thread_file` using workspace-relative path
`.amp/in/artifacts/leaf-validation-xxlarge-20260910.tar.gz`.
The archive is 2,280,388 bytes; SHA256:
`49407a2360c335d56f8baca7823e91a822d10c1c4eb12523ce28ab8364ebdf91`.
It contains this report's evidence snapshot, 165 individually checksummed
files plus `SHA256SUMS`; archive read-back verified every checksum.
The sibling `.tar.gz.sha256` file records the archive digest.

Only this report is committed on the trial/report branch. The raw archive is
an Amp artifact, not a Git-tracked binary. Do not cherry-pick the candidate
source on the strength of the observed wall means in this report.
