# Cumulative warning forcing + specialization: correctness trial

This is an experimental trial, not acceptance or integration into the accepted
campaign branch. No corpus preparation or benchmarks were run. Standalone
performance results motivate testing this union, but cannot be added together or
used to assume that warning forcing offsets specialization's memory increase.

## Source and provenance

- Repository fetch/push: `https://github.com/vtrl/purescript` only.
- Branch: `perf/cumulative-warning-specialization-20260910`.
- Exact base: `c44600e8f7a390ad73a46f8932e8f90a21b40a8a`.
- `git diff 38080a40fc3a53de813a0ac46187e8c709dcbcc7 c44600e8 -- . ':!debug'`
  is empty: the checkpoint preserves the accepted Make-lifetime source,
  applications, library, tests, and build configuration.
- Warning-force cherry-pick: `e6b308ca2af071637104ae7583043446c021a319`,
  from `2d5cc3bb3b766551e0b6c341dc84fc760915298f`, author Justin Garcia.
- Specialization cherry-pick / final tested source:
  `d0d4b3313f37ad9f79997eb6a852dc83aa2f0336`, from
  `c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63`, author seastian. Its provenance
  remains upstream PR 4584 / `8ac0fb2962a7df318a74216872465dc2868c6064`.
- Both were applied with `git cherry-pick -x`, without conflicts or edits.
- Tested tree: `8fe67bd6750d99c383210f2e9a702156adda2cea`.

The only changed paths relative to the base are `src/Language/PureScript/Make.hs`,
`tests/TestMake.hs`, `stack.yaml`, and `cabal.project`. The source/test diff's
SHA256 is `8fe311c640a75f031d67152c0e302226d8453c6b6d7abcaaf8c7532ea81375d8`;
the configuration diff's SHA256 is
`4eb993e4bb57772e2b20f7cbd7a484cb358fdb666fc6920b52b4cc818a1aec58`.
Both equal their respective original committed patches byte-for-byte.
Combined diff SHA256:
`e1975195ba218be2fd423a9f32d0099950581cb11a95e9d5941146f81a4bd677`.
The archived `git ls-tree -r` manifest over source/app/lib/tests and build files
hashes to `d52adaa25fa0fc9b880bd1cbcd0f86a239a83fcdc47ea0fe0b56a202e294aa48`.
No unrelated optimization, dependency, RTS, or fixture changes were made.

## Environment and execution

Worker12 orb: 16 visible CPUs, Intel Xeon @ 2.60 GHz; workload cgroup memory
limit 32,212,254,720 bytes (30 GiB), CPU quota `max 100000`.
Stack 3.3.1, GHC 9.6.6, lts-22.43, Node 22.23.2.
Actual GHC process arguments were captured in `actual-ghc-command.txt`.
They end in the existing warning settings and
`-O2 -Werror -fspecialize-aggressively -fexpose-all-unfoldings -fhide-source-paths`.
Stack's verbose build log also captures its exact Cabal configuration and
library/executable/test component build command. No `--fast`, profiling,
optimization override, acceptance environment, or test RTS override was used.

The ignored `run-cumulative.sh` wrapper atomically creates `start.guard` before
work, refuses duplicate execution and idles, records each command's actual exit
status and completion time, then idles with `exec sleep infinity`.
It ran under `amp orb service start cumulative-validation`, without a portal.
An executor replacement did not interrupt the service or repeat work. The
service was stopped only after collecting successful completion at
2026-09-10T14:33:18Z. No concurrent compute campaign was run in this orb.

Fixture setup (`npm install --prefer-offline`, Bower install, freshness marker)
completed before building. Uncached Haskell dependencies were resolved/built by
the normal build before validation. Main commands, with the toolchain PATH set:

```sh
stack --no-terminal --verbose --jobs=8 build --test --no-run-tests --lock-file=error-on-write
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal test --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal test --lock-file=error-on-write --test-arguments='--seed 9160 --no-color --match make'
```

## Results and validation overhead

| Command | Result | Wall seconds | GNU time maxRSS KiB |
| --- | --- | ---: | ---: |
| Optimized dependencies + library + executable + tests build | exit 0 | 1268.90 | 4552768 |
| Full suite, seed 9160 | 1303 examples, 0 failures | 104.92 | 692416 |
| Make subset, seed 9160, default RTS | 14 examples, 0 failures | 15.89 | 501292 |

Full Hspec execution itself took 103.1562 seconds; Make Hspec execution took
2.0436 seconds. The three command walls total 1389.71 seconds. These are combined
build/test setup overheads from this orb, **not a matched baseline build-cost
comparison**. The build includes uncached dependency work and test compilation.
After the full suite, generated docs output caused Stack to recheck/build and
copy/register components during the Make command; this was not hidden from its
timing. There was no new compiler link in that command. The original unstripped
executable timestamp is 14:27:34Z, before the full suite. Independently stripping
that executable after all tests reproduced the installed/frozen binary exactly.

The warning regression was inspected and passed in both suites. Module A fails
with `TypesDoNotUnify`; B depends on A and is skipped, while D is independent.
It requires exactly two `WarningParsingModule` diagnostics, with asymmetric
spans B `(7,3)-(7,4)` and D `(6,3)-(6,4)`, and no extra warnings. This exercises
nonempty parser warnings; the motivating 745-warning corpus had zero such
codes and is not a substitute for this test. No golden files were accepted or
updated; final tracked worktree was clean before this report.

## Final tested binary and transferable evidence

- Read-only mode 0555: `.build/perf/purs-cumulative-tested`.
- Installed stripped ELF x86-64, dynamically linked, 70,523,064 bytes.
- SHA256: `4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d`.
- Version: `0.15.15 [development build; commit: d0d4b3313f37ad9f79997eb6a852dc83aa2f0336]`.
- Copied only after full suite and Make completed; post-test installed/frozen
  rehashes and independent strip comparison agree. No subsequent build ran.
- Raw archive: `.build/perf/cumulative-warning-specialization-20260910-evidence.tar.gz`.
- Archive bytes: 217,437.
- Archive SHA256: `459d46b9cefa5a07734d3dd1dbf1fe0132e86165424b12f5268f94802e8dc160`.

The archive includes complete command logs, per-command timings/start/end/status,
guarded wrapper, hardware/toolchain identity, actual GHC arguments, source tree
manifest, source provenance, exact cumulative patch, build configuration copies,
and final binary hashes. The binary is separate for direct transfer; dependency
caches are deliberately excluded. Artifacts live in worker thread
https://ampcode.com/threads/T-01a08b9e-a9df-77b8-b32b-4f5a6d1e93cf.

No correctness blockers remain. Cumulative performance, binary-size tradeoffs,
and campaign acceptance remain unmeasured/pending lead assignment.
