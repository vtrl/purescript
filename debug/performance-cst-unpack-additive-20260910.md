# Additive CST unpacking: rejected at screening, 2026-09-10

**Do not retain the four-pragma CST packing change in this campaign. This is an evidence-only report; no CST source or test change accompanies it.** The accepted Make lifetime fixes address most of the motivation for the original standalone packing trial. On that new baseline, one additive N4 pair reduced peak RSS by 2.94% but increased wall time by 1.44%; the excluded additive warmup was also slower. That small memory signal did not justify further repetitions for this campaign.

This is a screening rejection, **not statistical proof of a slowdown and not a claim that other packing opportunities do not exist**. There is only one measured pair, so variance is unavailable. No measured samples or outliers were discarded. The exact trial patch, optimized binary, and raw results remain preserved for review; only the worker's four uncommitted pragma additions were removed after subsequent binary-only timing finished.

## Trial scope and source identity

The trial started from exact accepted [Make source 38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7) on a new `perf/cst-unpack-additive-20260910` branch. It added `UNPACK` only to already-strict `SourceRange.srcStart/srcEnd`, `TokenAnn.tokRange`, and `SourceToken.tokAnn` in `src/Language/PureScript/CST/Types.hs`. `SourcePos` already unpacks its strict `Int` fields. The generated GHC interface confirmed all four new fields were unpacked.

No binding-visibility change, old report/status, Make source change, Cabal change, global RTS default, test fixture, or golden update was included. The compiler source and tests now exactly equal the accepted Make parent; this branch retains only this report. The independent original packing checkpoint and its results remain separate and unaccepted.

## One clean N4 measured pair

| Metric | Accepted Make | Make + CST unpacking | Change |
| --- | ---: | ---: | ---: |
| Wall seconds | 113.90 | 115.54 | +1.4399% |
| Peak RSS, KiB | 2,055,676 | 1,995,152 | −2.9442% |
| Allocated bytes | 397,632,793,200 | 397,342,119,904 | −0.073101% |
| Maximum residency bytes | 699,736,224 | 671,664,824 | −4.0117% |

Excluded warmups were Make 109.22 s / 1,958,424 KiB RSS and additive 112.83 s / 1,886,604 KiB RSS. The baseline warmup-to-measurement variation exceeds the measured wall difference. Every raw summary SD is correctly `null`; warmups do not provide extra measured samples.

All four full-corpus compiles, including warmups, matched all **8,985 original product hashes** and the same **multiset of 745 warning bodies**. Warning comparison preserves duplicate multiplicity (738 unique bodies), removing only numbering headers and surrounding newline separators. Raw warning ordering is nondeterministic and was not used as a correctness criterion.

## Build and verification

Stack 3.3.1 / GHC 9.6.6 / lts-22.43, normal local `-O2 -Werror`; Node 22.23.2. No `--fast` or profiling build. The incremental build passed in 19.58 s. The full suite passed **1,302 examples, 0 failures**, in 102.4329 s Hspec / 104.25 s command wall. Failing test names: none. Test/build timings are not performance comparisons.

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
stack --no-terminal --jobs=8 build --test --no-run-tests --lock-file=error-on-write
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack --no-terminal --jobs=8 test \
  --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

The unchanged [e926183 paired harness](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b), copied to ignored scratch, ran with `--capabilities 4 --samples 1 --cache warm --codegen js`, the exact Make binary as `--baseline`, and the preserved additive binary as `--compiler`. It performed excluded B/C warmups and one B/C pair, deleting the output directory before every compile and hashing products after timing. Runtime arguments were `+RTS -N4 -s -RTS`, with empty `GHCRTS` and `LC_ALL=C`. No tests, builds, profiler, or other compute workload ran concurrently.

The corpus contained 4,084 PureScript sources / 4,901 inputs, all verified against input manifest SHA-256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`. Original/result product manifests were byte-identical, SHA-256 `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`. All warning multisets had canonical SHA-256 `ff88352d7dbe03c782ee39603fb62d9b611d12d3557fabcb803046455facee70`.

Machine: isolated Debian 12 orb, Linux 6.1.158+ x86_64, eight logical Intel Xeon CPUs at 2.60 GHz. Full command, timestamp, machine/CPU/memory snapshots, and binary identities are in the raw metadata. No heap-profile wall time was used in this decision.

## Preserved evidence

- Accepted Make binary: `.build/perf/parse-release-heap/purs-parse-release`, SHA-256 `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a`.
- Additive binary: `.build/perf/cst-unpack-additive-20260910/purs-candidate`, SHA-256 `2a73e77189c0ca580c12c41341d4d4319c75d3a648b009ea2681862f4b552d82`.
- Trial patch, not retained in compiler source: `.build/perf/cst-unpack-additive-20260910/source.patch`, SHA-256 `b38bad3af076fec4c65b1afd94267c3961cbbd9a9ee6e5f637ef864281f61c93`. Trial `CST/Types.hs` SHA-256 was `15be7c71660e09532eb01b37e9238aacdcc0fe470da24cc32612a9fdc351a203`.
- Complete raw archive: `.amp/in/artifacts/cst-unpack-additive-20260910/evidence.tar.gz`, SHA-256 `5ec7d1e424a9d069dca36988fb2b12e77aaa8f9ef013ea57c6a59024b55416e8`. It contains the source patch, exact identity, representation check, full build/test logs, all screen logs/time files, samples, paired changes, summary, metadata, products, warning comparisons, and checksums. Its contemporaneous screen report predates this explicit rejection decision.

These paths identify preserved worker-orb artifacts, not files committed with this report. The incremental candidate's `Version.hs` string was stale, so its source parent, patch, and binary hashes—not that string—identify the tested build. The later supplied Make+binding binary was tested separately without rebuilding or incorporating CST packing; it is not an additive CST result.
