# Independent final cumulative evidence audit

**No material source, binary, diagnostic, product, or statistical contradiction
was found in the supplied cumulative evidence.** All seven prescribed blocks
are complete: **51 measured pairs, 14 excluded warmups, 116 compiles**. Every
sample is retained. This audit does not accept or integrate compiler source;
that decision remains with the lead. No compiler, build, test suite, benchmark,
worker analyzer, or new worker was run by this offline audit.

The sole report change is based exactly on campaign
[8bc296a5](https://github.com/vtrl/purescript/commit/8bc296a5d4218180c078d5ff481c4c4b6cb761fe),
still source/test/config equivalent to accepted Make
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
Origin fetch and push were verified exclusively as
`https://github.com/vtrl/purescript`; the repository was unshallowed before
history inspection. Only `perf/cumulative-independent-audit-20260910` is pushed.
The warning and standalone specialization audit branches and archives remain
unchanged; their results are not pooled with this matrix.

## Raw reconstruction agrees, with boundary costs retained

Each entry below is the mean of numbered within-pair changes
\(100(C_i/B_i-1)\), followed by sample SD in percentage points, denominator
\(n-1\). These are not ratios of pooled means, confidence intervals, or sums
of standalone effects. BC/CB alternates beginning BC separately in every block.
Every block has exactly one excluded B warmup and one excluded C warmup.

| Block | Pairs | Wall change % ± SD pp | RSS change % ± SD pp | Allocation change % ± SD pp | Sampled residency change % ± SD pp |
| --- | ---: | ---: | ---: | ---: | ---: |
| Full N1 | 3 | −35.141806 ± 1.181716 | −20.680230 ± 2.636633 | −43.640171 ± 0.001522 | −14.114288 ± 4.844388 |
| Full N4 | 5 | −31.524660 ± 1.373529 | −18.723925 ± 4.728211 | −43.628408 ± 0.002146 | −13.049836 ± 7.839196 |
| Full N8 | 3 | −28.174550 ± 5.100648 | −12.847684 ± 3.420442 | −43.630697 ± 0.001781 | −11.486589 ± 6.271002 |
| Dense N1 | 10 | −12.878218 ± 12.458748 | +15.849986 ± 0.097215 | −43.744271 ± 0.000000 | +15.492755 ± 0.000076 |
| Tiny N1 | 10 | −39.226109 ± 6.758445 | +8.048707 ± 1.883594 | −49.674182 ± 0.000086 | −4.490108 ± 7.873652 |
| Tiny N4 | 10 | −42.310228 ± 4.649015 | +9.345215 ± 2.928655 | −49.629070 ± 0.049767 | +3.519288 ± 17.695890 |
| Tiny N8 | 10 | −44.610587 ± 0.829668 | +6.362433 ± 3.106633 | −49.699331 ± 0.091767 | +0.879583 ± 8.819787 |

Absolute process peak RSS must remain visible alongside percentages:

| Block | B → C mean RSS MiB | Mean absolute change MiB | C measured range MiB |
| --- | ---: | ---: | ---: |
| Full N1 | 1745.167 → 1384.694 | −360.473 | 1328.426–1457.250 |
| Full N4 | 1957.048 → 1589.966 | −367.081 | 1478.637–1715.504 |
| Full N8 | 2121.947 → 1848.314 | −273.633 | 1807.750–1880.734 |
| Dense N1 | 138.526 → 160.482 | +21.956 | 160.332–160.578 |
| Tiny N1 | 46.584 → 50.327 | +3.743 | 49.863–51.535 |
| Tiny N4 | 60.950 → 66.628 | +5.678 | 64.172–68.625 |
| Tiny N8 | 80.980 → 86.111 | +5.131 | 83.422–88.262 |

All full-corpus pairs favor C on the four primary metrics. Dense wall pairs
**2 and 6 are slower** (+1.4286%, +4.4776%); neither is removed. Tiny mean
RSS rises at every capability, and tiny N4/N8 mean sampled residency rises.
There is no universal memory-offset claim.

The N1 raw measured walls are B/C 196.21/129.87, 200.69/129.35,
203.73/130.25 seconds; warmups 196.85/131.52 are excluded. B drifts upward
while C is narrower. N1 measured **Gen0 maximum pauses increase from
8.5–10.6 ms to 51.8–54.2 ms**, despite Gen1 maxima decreasing from
519.8–539.4 ms to 326.3–369.0 ms. The independent reader also checks copy
bytes, INIT/MUT/GC/EXIT/Total CPU and elapsed times, generation collections,
parallel counts, average/maximum pauses, and all 96 boundary CSV records.
Lower wall/GC time is not improvement in every pause metric. Sampled residency
is neither allocation nor RSS, and no retaining-root conclusion follows.

## Exact source union, actual bytes, and supplied correctness logs

Candidate source
[d0d4b331](https://github.com/vtrl/purescript/commit/d0d4b3313f37ad9f79997eb6a852dc83aa2f0336)
has warning pick
[e6b308ca](https://github.com/vtrl/purescript/commit/e6b308ca2af071637104ae7583043446c021a319)
as its parent. Byte-for-byte Git comparisons confirm the original source/test
[2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f)
and flags
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63).
Only Make.hs, TestMake.hs, stack.yaml and cabal.project differ outside reports.
No extra source, dependency, RTS, test, golden, or harness change is present.

| Exact object | SHA256 |
| --- | --- |
| Warning source/test patch | `8fe311c640a75f031d67152c0e302226d8453c6b6d7abcaaf8c7532ea81375d8` |
| Flags configuration patch | `4eb993e4bb57772e2b20f7cbd7a484cb358fdb666fc6920b52b4cc818a1aec58` |
| Complete union patch | `e1975195ba218be2fd423a9f32d0099950581cb11a95e9d5941146f81a4bd677` |
| Compiler/test/config tree manifest | `d52adaa25fa0fc9b880bd1cbcd0f86a239a83fcdc47ea0fe0b56a202e294aa48` |
| Actual B, 47,869,944 bytes | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| Actual C, 70,523,064 bytes | `4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d` |

C's executable cost is **+22,653,120 bytes / +47.3222%**. The supplied B/C
bytes were directly hashed, including both copies from N1/N4 and the lead's
separate C. Hashes, not B's older dirty development version string, identify B.
Every timing metadata record identifies these same B/C bytes and C source.

Cherry-pick authors/dates/trailers agree. The flags retain **seastian
<seastian@users.noreply.github.com>**, upstream
[PR4584](https://github.com/purescript/purescript/pull/4584) /
[8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064)
attribution in Git/archived provenance; this audit did not newly fetch upstream.

`prepare.py` independently checks actual logged GHC arguments: **Stack 3.3.1,
GHC 9.6.6, lts-22.43, local -O2 -Werror** plus both intended specialization
flags. No fast/profile/golden-acceptance argument appears. The supplied full
and Make commands use `CI=true`, unset `HSPEC_ACCEPT`/`GHCRTS`, seed 9160,
and `--no-color`. Full suite **1303/0**, Make **14/0**, all exit statuses zero.
The asymmetric skipped/independent parser-warning test passes at
`build/cumulative-evidence/full-suite.log:1333` and `make-suite.log:41`.
Those are audits of supplied logs, **not tests run in this audit**; they use
default RTS, not an explicit N1/N8 suite claim.

In candidate Make.hs:253–288, dependency waits precede semaphore admission;
warning forcing and `tell` precede parser-error unwrapping and the successful/
skipped branch. The asymmetric test at TestMake.hs:179–202 requires B's skipped
warning at 7:3–7:4 and independent D's warning at 6:3–6:4, alongside A's type
error. Moving warning handling exclusively into the successful-build branch
would lose B's required diagnostic. The exact source/test union preserves the
previous independently audited warning implementation; no new semantic variant
is inferred from the flag change.

Build 1268.90 s / max RSS 4,552,768 KiB includes dependencies and tests,
**not matched relative build cost**. The compiler link is `build.log:4390`,
before both suites. Later Make copies/registers but records no Haskell relink;
test fixture compiles are not compiler rebuilds. Installed/frozen/post-test
strip hashes agree in `installed-binary.sha256`, `post-test-rehash.txt` and
`final-binary.txt`. The unstripped binary itself is not supplied, so that
strip reproduction remains logged evidence. The builder's missing workload
cpuset file is retained; its parent effective 0–15 cpuset is recorded.

## Diagnostic and product claims match their actual coverage

Unchanged harness SHA256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`
matches all three supplied harnesses and the accepted harness. Every raw
`.time`, `.stderr`, `.stdout` is parsed without importing worker analyzers.
Every full run has all 4084 unique progress ordinals/modules and explicit
N1/N4/N8 as assigned; tiny has 57 and dense one. Full ordered source lists
and expanded argv agree; actual N1 warmup argv matches metadata too.

All 28 full-corpus runs match **745 warnings / 738 distinct bodies** against
the original transcript: 737 bodies once and one eight times. The comparator
ignores ordinal headers/order and surrounding blank lines, preserving duplicate
multiplicity, positions and all internal whitespace. Full warning content has
**zero WarningParsingModule**. Parser-warning behavior is therefore covered by
the supplied regression suite, not this benchmark's warning mix. Tiny's 66
runs retain three warnings each; dense's 22 are warning-free.

Original full input manifest (4084 sources / 4901 inputs) SHA256
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`
and original 8985-product manifest SHA256
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`
match. All supplied N1 input bytes were rehashed. N4/N8 worker-local full input
bytes are not supplied: exact manifests and pre/postflight assertions are
distinct from the direct N1 byte check. Tiny/dense actual inputs are supplied
and rehashed; dense equality uses its local two-product reference, not a
cross-orb externs assumption.

Harness lines 113–129 save each block's first product map and compare subsequent
runs with it before emitting the successful row. Together with the reference
checks, seven initial captures and 109 comparisons support product equality for
**all 116 runs**, not 116 independently retained trees.
Separately rehashed: each N1/N4/N8 final 8985-product tree, last tiny's 135
products, and dense's two products. N4's full 8987-file tree also matches
manifest SHA256 `e2b6366c31ee02c889378db84f21775eaa2fe55cc99781ee44bd78d0d561502a`;
the extras are cache-db.json/package.json. Intermediate tiny N1/N4 final maps
are retained worker records; only the last tiny tree is directly supplied.

## Setup failure, capture activity, and extraction normalization are explicit

The boundary's first service failed before harness entry on missing optional
`/proc/pressure`. Its `run-once.sh:22,37–42` establishes error propagation and
snapshot-before-harness ordering. Exit 1, started guard, log, and absent result
directory/metadata/time files remain under `boundaries/.build/perf/cumulative-boundaries/`.
The invalid >15-character `pgrep -x` name is **not absence evidence**. A distinct
guarded root with optional PSI handling begins 14:48:18Z; there is no measured
retry, extra screen, or discarded partial sample.

All 14 measured-block endpoint snapshots show **29.5 GiB** workload memory.max,
16 CPUs, no swap, unlimited workload CPU quota, and zero throttle/memory-event
deltas. N4 records memory.high=29 GiB and NRestarts=0. No constant limit or
absence of outside activity is inferred for unsampled intervals. N1's brief
first-pair validator ran while C2 continued; N4 brief status reads are retained.
Their timing effect is not measured. Small samples, fixed order and VM drift
limit generalization; no new acceptance criterion is introduced.

**Permission-provenance correction:** original N1/N4 tar members and runtime
logs are **0555**, not 0755. This audit's extracted copies are 0755. Inspection
of every original `TarInfo.mode` and Python 3.11.6 `tarfile.data_filter` shows
0555 → 0755 owner-write normalization. `extraction_modes.py:21` preserves the
controlled call `tar.extract(member, path=temporary, filter="data")`, reproducing
that transformation for all four binaries with unchanged hashes. The initial
extraction invocation was not retained as a script/log; the controlled check
does not pretend to recover it. This is **not an original archive/runtime
permission discrepancy**. Both failed reader-attempt logs are preserved: the
initial extracted-mode assumption and a boolean-versus-object checkpoint schema
assumption were corrected after inspection, without altering measurement data.

## Transfer and replay

Original delivery hashes were checked, as were all extracted payload bytes.
Each benchmark archive's exact regular-member set matches its payload manifest
plus the manifest itself, with no extra payload:

| Delivery | Bytes | Payloads | SHA256 |
| --- | ---: | ---: | --- |
| Supplied build | 217437 | 45 | `459d46b9cefa5a07734d3dd1dbf1fe0132e86165424b12f5268f94802e8dc160` |
| N1 | 66461433 | 16756 | `c4cdf5a327a445a38859a2f3dd692cecc88efb970aca32a641c74a627c13ac3e` |
| N4 | 48957092 | 9069 | `6cd5b71d3e46ca228ce2ec5b0c0f6336b840c146cec73d72fbeefe36dcc8df6f` |
| Boundary raw | 3281463 | 488 | `c9874a504036ac6a12de8fcf64d07b93b61a7359dc879bd2220a2f2c8c5c4de7` |
| Boundary products | 19111380 | 9122 | `2b40177c0cda4d1d02f0332cf2bb694cae724b53900538a6db8ca6e75ef438d9` |

Both boundary root manifests were preserved/verified before overlay. Exact
worker reports were independently read from
[N1](https://github.com/vtrl/purescript/commit/4a377bfe39489ab114a8ffbd2584ca84e00f6bce),
[N4](https://github.com/vtrl/purescript/commit/cb22ee82dd9fa72558f5cc3e0bf891a3edfbf315), and
[boundaries](https://github.com/vtrl/purescript/commit/ebab35a14b7942697f25f603d713834fb62915bb);
each is a report-only direct child of the prescribed campaign checkpoint.

Independent bundle, available from this
[audit thread](https://ampcode.com/threads/T-01a08b71-7140-741c-b98f-a8a370d2b222):
`.amp/in/artifacts/cumulative-independent-audit-20260910/raw-audit.tar.gz`

- **73,597,056 bytes**, SHA256 `7187d478e02dedd3decb579c51db096ec26c1f5c449d841f1e08fd2314ece7c3`.
- **32,715 payload files** plus root SHA256SUMS: full raw/build/test logs,
  independent readers/results, worker reports, exact required source/FFI bytes,
  all retained final trees, manifests, service/guard/counter/argv evidence and
  failed audit-attempt logs. Binaries, large original archives, unused package
  files and package/build caches are excluded. Only required corpus sources
  under their original `.spago` paths are retained.
- `independent-results.json` SHA256 `17779034218085d4bdceb75a4e8e93cb8b8fcca3c27497b89caafbfd0621fa20`;
  `raw-results.json` SHA256 `4204a518c8fb078df04f69c607faea0c50b7c2aa592421aacd522054b7b380a6`.

Commands actually executed, all exit 0:

```text
python3 -B tmp/cumulative-audit/prepare.py
PASS: exact union/build/binary provenance; supplied full 1303/0 and Make 14/0.
python3 -B tmp/cumulative-audit/final_audit.py
PASS: 7 blocks, 51 measured pairs, 14 excluded warmups, 116 raw compiles.
tar -xzf <audit-bundle> -C <fresh-directory>
python3 -B <fresh-directory>/cumulative-independent-audit/replay.py
PASS: all 32715 bundle payload checksums.
PASS: all raw results exactly reproduce the original independent calculation.
PASS: warnings, required input bytes, retained products, secondary RTS/counters.
```

Fresh-extraction replay requires no Git or binary execution; full original
archive/binary/Git provenance checks are deliberately not claimed by this
reduced replay. Full original checks are recorded in final-audit/preparation
logs. Complete replay output/exit status is adjacent to the bundle as
`replay.log` / `replay.exit`. No audit blocker remains; source acceptance and
integration remain separate lead-owned actions.
