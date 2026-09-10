# Traversal INLINABLE: rejected standalone screen, 2026-09-10

**Reject the standalone two-INLINABLE trial on this campaign baseline. No repetitions were run, and this change must not be combined into the pending flags trial.** The measured N4 pair was 3.74% slower, its excluded warmup was also slower, allocation was effectively unchanged, and the 1.10% RSS reduction was not a compelling positive signal. This is a screening decision, not statistical proof of slowdown or a claim that the private upstream result is false.

The source is preserved as an attributed, unaccepted trial checkpoint, with this evidence report in a separate report-only commit. The accepted compiler remains [Make 38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7); neither the trial source nor this report advances the accepted baseline.

## Authoritative provenance and exact adaptation

- Upstream: [restaumatic/purescript commit b831b298](https://github.com/restaumatic/purescript/commit/b831b2983b570550af52db1e88a53fe99f8fb588), [PR #19](https://github.com/restaumatic/purescript/pull/19). The authoritative patch and PR were read before application. The original patch is preserved, including its full commit message.
- Original author: **Michal Kozakiewicz <morswin@gmail.com>**, author date **2026-05-08 15:21:48 UTC**. The adapted source commit preserves both.
- Exact original trailer, preserved in the adapted source commit: `Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>`.
- Adapted source: [e10a641c](https://github.com/vtrl/purescript/commit/e10a641ca84335409faa1f4e2d28f3f9a2330a54), on `perf/traversal-inlinable-20260910`, based on exact campaign [4861c1e1](https://github.com/vtrl/purescript/commit/4861c1e1e418122a172ad4b707f4ba5f8a2c5361). Compiler source, tests, Cabal, and Stack configuration at that base were verified identical to accepted Make-only source.
- The complete source delta is two `INLINABLE` additions in `src/Language/PureScript/AST/Traversals.hs`, immediately after the signatures of `everywhereOnValuesTopDownM` and `everywhereOnValuesM`. Function bodies are unchanged. Both upstream and adapted diffs have stable patch ID `b1d7a2f61864e7b546cef5de5c17fce927f9fb58`.
- No CST packing, binding-visibility variant, aggressive-specialization flag, test fixture, golden update, or other source change was mixed into this trial. GHC's generated CST interface was checked to ensure the prior rejected packing was absent from the rebuilt executable.

The upstream author reported approximately 9% full-build improvements on a private 1,758-module `pr-admin` workload: −9.6% in one run and −8.9% in another. The first run had load averages around 4–7; no-change results varied from +8.4% to −3.0%. The fork also reported 1,340 passing tests and a 213 KB binary increase. These are **untrusted, author-reported measurements on a different fork/workload**, not reproduced evidence for this baseline. The private workload, complete raw samples, and machine details were not supplied in the commit/PR, and the maintainer cautioned against generalizing from one codebase.

## Initial N4 measured pair

| Metric | Accepted Make | Two-INLINABLE candidate | Change |
| --- | ---: | ---: | ---: |
| Wall seconds | 113.20 | 117.43 | +3.7367% |
| Peak RSS, KiB | 1,986,060 | 1,964,296 | −1.0958% |
| Allocated bytes | 397,628,763,936 | 397,629,988,512 | +0.000308% |
| Maximum residency bytes | 712,814,344 | 673,067,688 | −5.5760% |

Excluded warmups were Make 107.19 s / 1,973,556 KiB RSS and candidate 110.82 s / 2,014,720 KiB RSS. Warmup wall change was +3.3865%. There is only one measured pair, so every sample SD is unavailable (`null`); warmups are not extra measured samples. No samples were discarded. The private upstream full-build gain was not reproduced by this screen.

All four clean full-corpus compiles matched the **8,985 original product hashes** and the same **multiset of 745 warning bodies**, including duplicate multiplicity (738 unique bodies). Only warning numbering and surrounding newline separators were removed; raw warning order was not treated as a correctness property.

## Normal optimized build and full seeded suite

Toolchain: Stack 3.3.1 / GHC 9.6.6 / lts-22.43, local `-O2 -Werror`, Node 22.23.2. No `--fast`, profiling build, or broad compiler flags were used. Build passed in **149.65 s**. This incremental build also replaced cached objects from the earlier rejected CST trial, so its duration is not an isolated measurement of INLINABLE build overhead.

The full seeded suite passed **1,302 examples, 0 failures**, in 105.1640 s Hspec / 107.46 s command wall. Failing test names: none. No golden acceptance was enabled. Build/test times are recorded operational evidence, not performance comparisons.

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal --jobs=8 build --test --no-run-tests --lock-file=error-on-write
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

Both baseline and candidate are **47,869,944 bytes**: size change 0 bytes / 0%. Their binary and executable-section hashes differ, so equal size is not a claim of identical machine code or evidence of a performance benefit. The incremental version string retained old metadata; source commit, exact patch, and binary checksum identify the candidate.

## Measurement controls and preserved evidence

The unchanged [e926183 paired harness](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b) ran `--capabilities 4 --samples 1 --cache warm --codegen js`, using the exact accepted Make binary as `--baseline` and the preserved candidate as `--compiler`. It performed excluded B/C warmups followed by one B/C pair, deleting output before every compile and hashing after timing. Compile arguments included `+RTS -N4 -s -RTS`, with empty `GHCRTS`, `LC_ALL=C`, and `LANG=C`. No other builds, tests, profiler, or compute workload ran concurrently.

Corpus: 4,084 PureScript sources / 4,901 inputs, all hashes verified against manifest SHA-256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`. Product manifest SHA-256, identical to original: `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`. All warning multisets had canonical SHA-256 `ff88352d7dbe03c782ee39603fb62d9b611d12d3557fabcb803046455facee70`.

Machine: isolated Debian 12 orb, Linux 6.1.158+ x86_64, eight logical Intel Xeon CPUs at 2.60 GHz. Exact command, timestamp, CPU/memory snapshots, and compiler identities are preserved in screen metadata. No heap-profile wall time was used.

- Baseline: `.build/perf/parse-release-heap/purs-parse-release`, SHA-256 `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
- Candidate: `.build/perf/traversal-inlinable-20260910/purs-candidate`, SHA-256 `c68584b1f51d5ea9bc102415f55675f4972d71527831c671f13f08b578729396`.
- Adapted patch SHA-256 `8703a3e95156e9173d52f03e25018fb9007b572d3e37e88bbe0a3f166c772978`; original upstream patch SHA-256 `bf26f66b9112ab43ad46151c3db460ea8b5adae7d5dfb80f72e3b5f944049da3`.
- Immutable raw archive: `.amp/in/artifacts/traversal-inlinable-20260910/first-screen.tar.gz`, SHA-256 `3fc9b8ac6b604d781e81a14513f11b63514cbfa734399a6358588645df2c0f3c`. It contains both patches, exact author/source/binary identity, build/full-suite logs, representation and section checks, all raw screen logs/time files, metadata, samples, paired changes, summary, products, warnings, and checksums. This separate report records the subsequent explicit rejection decision.

Those paths identify preserved worker-orb artifacts, not binaries or archives committed with this report. Previous trials and binaries remain intact. Only the two-pragma attributed source checkpoint and this separate report belong to this trial branch; neither is accepted compiler source.
