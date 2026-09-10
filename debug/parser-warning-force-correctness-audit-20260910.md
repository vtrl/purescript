# Parser-warning-force: independent correctness and evidence audit

2026-09-10. Audit by [this worker](https://ampcode.com/threads/T-01a08b71-7140-741c-b98f-a8a370d2b222), reporting to the [campaign lead](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c).

**No candidate correctness bug identified.** The independent normal optimized build and full seeded suite passed: **1,303 examples, 0 failures**. Raw N4 screen and N8 evidence agrees with the reported metrics, warning contents, and recorded product checks. This report is not performance acceptance or a retaining-root proof.

The significant coverage limits are explicit: the existing test executable is non-threaded, so explicit N1/N8 Make-test coverage is unavailable without changing build flags; none of the corpus's 745 warnings is a parser warning; and per-run product identity is supported by harness assertions, not separately retained product trees for every run. No production source, existing test, build flag, corpus, or golden was changed in this audit. No performance experiment ran here.

## Exact source and independent build

Origin fetch and push URLs were both verified as `https://github.com/vtrl/purescript`. The checkout was unshallowed before history inspection. The audit branch was created at exact candidate [2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f), whose parent is exactly [3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d). The complete difference from accepted [Make38080](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7) to that checkpoint consists of six debug Markdown reports; source, tests, and configuration are unchanged.

The candidate diff contains only the `evaluate . force` warning change and its comment in Make, and the asymmetric regression in TestMake. Both archived source patches equal the exact Git diff byte-for-byte: SHA256 `8fe311c640a75f031d67152c0e302226d8453c6b6d7abcaaf8c7532ea81375d8`.

Tools actually used: Stack 3.3.1, GHC 9.6.6, lts-22.43, Node 22.23.2, npm 10.9.8. The normal local `-O2 -Werror` build used no `--fast`, profiling, or global specialization flags. The full build log and the observed library GHC command are preserved. The latter ends with `-O2 -Werror -fhide-source-paths`.

```sh
export PATH="$HOME/.local/share/purescript-orb/bin:$HOME/.local/share/purescript-orb/node/bin:$PATH"
unset HSPEC_ACCEPT GHCRTS
export CI=true
stack --no-terminal --verbose build --test --no-run-tests --lock-file=error-on-write
stack --no-terminal test --lock-file=error-on-write --test-arguments='--seed 9160 --no-color'
```

The build completed 34 actions successfully. Repository fixture installation preceded the build/test run; the full test log starts directly with Hspec, not fixture installation. The test result was `Finished in 135.4401 seconds; 1303 examples, 0 failures`. This is a correctness-run duration, not a performance comparison with the lead's 106.7781-second run.

The managed one-shot service used an atomic started-directory guard, wrote `completion.exit` = 0, and slept rather than restarting the tests. It was stopped after collecting results. The independent build's version is a clean candidate commit stamp, not the stale DIRTY version string in the measured archives.

| Independently tested artifact | Bytes | SHA256 |
| --- | ---: | --- |
| Test executable in Stack dist | 65,890,232 | `95b0c05aaab0025917f81d25c702b58dd5549ccff89908b67807aaf41966e584` |
| Unstripped compiler in Stack dist | 68,107,696 | `ec4bd827dda9447389e9feef917f3a8a40396af794fffc868fa6e8908fdf30bb` |
| Installed stripped compiler on Stack PATH | 47,869,944 | `e22ab9ca63f9ed912047582faceddce9745e5da5e5fb259deea796ca6906a63a` |

The IDE support harness invokes `purs compile` through PATH (`tests/Language/PureScript/Ide/Test.hs:132`); the installed compiler identity was also recorded during and after the suite. The installed and unstripped compiler share the ELF BuildID; normal Stack installation stripped the former. None of these independent hashes is represented as the archived measured candidate hash. The archived binaries are absent, so this audit cannot establish that their only difference from this build is the version stamp.

All 1,897 tracked files hashed before the audit report were verified unchanged after tests. Their sorted path/hash manifest SHA256 is `22ae62f2fafbe505e77631e83ddaebb70603fdfa79e2945f1ac350825a24834d`. A separate 1,852-file source/test/config manifest has SHA256 `8dd340aca05b238fcb764e4237612d1afac5d3cbf88edb69af43d657ffb71460`. Config files and individual hashes are in the bundle. In particular:

- `Make.hs`: `f391e030067e9710ced29139d0a7d7e96390816af37e961754c5316e1bb37ff8`.
- `TestMake.hs`: `7031288dfbfcff875d20ac213d6a2236369e8447fd1d47af6e6aa11165c42a6f`.
- `stack.yaml`: `6ef1c7178418c0e6cde7ff5afa2e73a384d5859afb5769b9c6f1acc47d571e68`.
- `stack.yaml.lock`: `de4134bbf019a938fb55eb2c9d2b15f09d9113d680d6c3385a0a08a6b8378e86`.
- `purescript.cabal`: `40d884157f3c796fdcd301306460db27f20ae5493436e798ffece54ad3a85d92`.

## Path review: warning emission remains before errors and skips

The following observations are from candidate source, not a heap-census inference.

| Path / contract | Evidence and result |
| --- | --- |
| Admission and dependencies | `Make.hs:157–181,247–263`: the capabilities-sized QSem is unchanged. Every `getResult` dependency wait precedes QSem acquisition. Warning conversion and deep evaluation remain inside `bracket_`, after waits. No semaphore is held while awaiting dependencies. The change does not bound all compiler activity, only the existing module-build region. |
| Why an empty warning list matters | `CST/Parser.y:804–817` stores a lazy full parse; its warning projection comes from the returned ParserState. `CST.hs:92–102` lazily maps positioned warnings. `Errors.hs:373–377` gives MultipleErrors a list-based NFData instance, and `CST/Errors.hs:64–80` recursively derives NFData for warning payloads. Forcing the result evaluates the list and payloads before handing them to long-lived consumers. |
| Logger | `Control/Monad/Logger.hs:39–46`: tell appends to an atomic IORef; listen uses a separate log and appends that result once. The Logger remains unchanged. Its strict IORef update is not a promise of deep evaluation of all list elements or pending appends. |
| Success | `Make.hs:261–286`: parser warnings are told once before rebuild's listen. The successful job stores parser plus rebuild warnings. `Make.hs:190–202` collects externs/errors, not another tell of job warnings; dependency `mexterns` discards the warning half. Thus storage in BuildJobSucceeded does not duplicate user diagnostics. |
| Failed dependency / skipped body | `Make.hs:253,263–287`: after dependencies finish, parser warnings are emitted and parser errors unwrapped before `mexterns = Nothing` yields BuildJobSkipped. Warning emission does not depend on successful typechecking. |
| Body parse error | `CST.hs:86–94`, `Make.hs:249,261–263,289`: warnings already entered the Logger when unwrapParserError throws MultipleErrors. catchError converts the error to BuildJobFailed; markComplete still runs. The existing skipped-dependent parser-error regression checks this independently of the new warning regression. |
| Type error | `Make.hs:281–286`: parser warnings precede rebuild/listen. Typechecking's MonadError becomes a failed job; earlier parser warnings remain in the outer Logger (`Make/Monad.hs:51–66`). |
| Header parse error | `CST.hs:38–48,76–94`, `CST/Parser.y:809–817`: a failed header is rejected before make receives a PartialResult; body-warning handling is not reached. This candidate does not change that contract. |
| IO exception | `Make.hs:183–185,249,258–261`: MonadError handling is not an IOException catcher. bracket releases admission on an IO exception; the existing outer onException marks the barrier failed with mempty and rethrows in the worker. The scratch progress-exception check verifies completion with the already-emitted warning. It does not promise delivery of diagnostics under every asynchronous cancellation. |
| Prebuilt modules | `Make/BuildPlan.hs:95–123`: prebuilt modules supply no warnings and are not rebuilt. This existing incremental behavior is unchanged. |

Deep evaluation intentionally changes when a bottom or exception in a lazily supplied warning payload would be observed. No such malformed/infinite hand-constructed input is produced by the normal finite source parser in these checks; it is not evidence of a product regression. Already-existing worker-exception behavior is not expanded into a new API guarantee.

The source gives a plausible lifetime explanation: an evaluated warning value no longer needs its unevaluated parser-state projection. Neither this source trace nor a type heap census identifies the exact runtime retaining root, proves every source-text backing allocation is released, or establishes that the Logger is the sole root. No root claim is made.

## Regression sensitivity and focused checks

`tests/TestMake.hs:178–202` constructs a failing A, dependent B, and independent D. B's offside parser warning is at 7:3–7:4; D's is at 6:3–6:4. It asserts the entire warning multiset with exact paths/spans and exactly the type error. Moving warning handling into only the successful-build branch drops B's warning, so the test fails. Swapping file/position attribution, omitting a warning, or duplicating one also fails. It deliberately ignores inter-thread warning order. It does not itself prove strictness, and passing against the unchanged baseline is the intended contract test rather than a memory-effect test.

The supplied baseline test log reports `1 example, 0 failures`; the supplied candidate full-suite log reports `1303 examples, 0 failures` in 106.7781 seconds. Their source association is supplied provenance; the independent clean build above removes reliance on those logs for candidate correctness.

The existing test binary was inspected with `+RTS --info -RTS`: GHC 9.6.6, `RTS way = rts_v`, `Flag -with-rtsopts = ""`. `purescript.cabal:436–442` does not add threaded/rtsopts to tests, unlike the purs executable at line 404. Therefore explicit N1/N8 Make-test runs were not attempted and no such concurrency coverage is claimed. The unchanged default binary was run with `--match make --seed 9160 --no-color`: **14 examples, 0 failures**, including both diagnostic regressions. Its existing support-code updater ran before Hspec; no setup time is used as a test/performance comparison.

An ignored 51-line `FocusedDiagnostics.hs` was run against the independently built package using:

```sh
env -u HSPEC_ACCEPT -u GHCRTS CI=true stack exec -- \
  runghc -package=purescript tmp/parser-warning-audit/FocusedDiagnostics.hs
```

**Five checks passed**, comparing exact error lists, failure/success status, and warning paths/spans: successful offside syntax; body parse error following the warning; type error following the warning; header parse error with no warning; intentional progress IOException following the warning. The last verifies the existing failed-barrier release with Left mempty, not conversion of arbitrary IO exceptions into normal diagnostics. The scratch interpreter uses its default RTS; this is not explicit-N coverage. Initial audit-script development errors are preserved in the raw bundle and distinguished from candidate failures.

## Independent archive recomputation

Received archive bytes were hashed locally:

| Archive | SHA256 |
| --- | --- |
| Lead `parser-warning-force-screen.tar.gz` | `324d981ef3200f3a2a3a6cb8622088c9e2e718b4eb77af05953bf9c923f49d45` |
| N8 worker `n8-raw.tar.gz` | `97a958e4cf4a5cbb8672b2110be1fc60cf460c13a1597c3688a35f8d5c5f66d9` |

Both harness copies equal the Git contents of [e926183](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b), SHA256 `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`. Both metadata records identify measured B as `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` and C as `92643eb2d1100711e7e8da39168202d76246ae0a6e335edf640bcac7c0d52e1d`; N8 identity agrees. Their recorded 47,869,944-byte sizes and source association come from supplied identity/report evidence, not binary bytes in these archives. Both version strings retain an older DIRTY stamp and must not be substituted for source identity.

An independent Python audit parsed every raw `.time`, `.stderr`, and `.stdout`, without importing or running the supplied harness. It recalculated all four paired metrics before checking summaries. It also checked the exact compile argument list against the source paths in the input manifest, package-set identity, per-run RTS capabilities, all 4,084 numbered unique module progress entries per run, alternating order, and agreement between raw values, printed harness rows, and samples.json.

All **four screen** runs and **eight N8** runs pass these checks. Screen order is warmup B/C, B1/C1. N8 is warmup B/C, B1/C1, C2/B2, B3/C3. Warmups exist in raw logs and are excluded from summaries. No measured pair or outlier was dropped. The exact source lists and package-set metadata match between blocks: package set 60.4.0, 4,084 PureScript sources / 4,901 input manifest entries, manifest SHA256 `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

| Raw-data result | Wall | Peak RSS | Maximum residency | Allocated bytes |
| --- | ---: | ---: | ---: | ---: |
| N4 B1 → C1 | 110.72 → 109.90 s | 1,967,444 → 1,587,744 KiB | 683,120,616 → 560,696,616 B | 397,639,781,464 → 397,626,571,208 B |
| N4 paired change | −0.740607% | −19.299152% | −17.921286% | −0.003322% |
| N8 baseline mean | 73.553333 s | 2,198,641.333333 KiB | 768,197,226.666667 B | 397,603,832,578.666667 B |
| N8 candidate mean | 71.610000 s | 1,648,412.000000 KiB | 582,924,458.666667 B | 397,597,656,613.333333 B |
| N8 mean paired change ± sample SD | −2.625442% ± 1.024722 pp | −25.024303% ± 1.158746 pp | −24.096042% ± 1.637639 pp | −0.001553% ± 0.005191 pp |

N8 wall pairs are 71.84→70.74, 73.31→71.27, and 75.51→72.82 seconds. Percentages are calculated per matched pair and then summarized, not from a ratio of group means. N4 has only one pair, so no sample SD or speed/acceptance claim is appropriate. N8 is still limited three-pair evidence; allocation is effectively unchanged.

## Warning and product claims: what is directly retained

All 12 stdout files contain exactly **745 complete warnings / 738 distinct bodies**. Their CONTENT multisets agree across variants, warmups, and N4/N8. Normalization removes only ordinal headers and surrounding newline separators; indentation, internal whitespace, text, source spans, and duplicates remain. The duplicate distribution is **737 bodies once, one body eight times**, not seven unrelated duplicate bodies. Canonical multiset SHA256 is `ff88352d7dbe03c782ee39603fb62d9b611d12d3557fabcb803046455facee70`.

The audit also counted warning codes from complete bodies: **zero WarningParsingModule**. Thus these archives directly verify preservation of other diagnostics and the empty-parser-warning case, while nonempty parser-warning semantics are established by the candidate regression and scratch checks. Raw warning order is nondeterministic and is not part of the comparison.

Both blocks' retained product maps are byte-identical to the original manifest: **8,985 entries**, SHA256 `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`. They contain 4,084 index.js, 4,084 externs.cbor, and 817 foreign.js entries; there are no corefn.json entries under the chosen JS-only codegen.

The exact harness (`ci/benchmark-compiler.py:99–129`) uses subprocess `check=True`, hashes outputs outside the timed process, fails on product differences, and only then prints a run row. Four/eight completed run rows therefore support the recorded successful exit and product checks for screen/N8 respectively. But only the first product manifest is retained on success; there are not 12 independent product manifests or output trees to rehash offline. Likewise the corpus manifest can be rehashed and checked for path/count consistency, but the corpus bytes are not in these archives. These are evidence limits, not observed mismatches. No stronger per-run product claim is made.

## Transfer and follow-up scope

Complete evidence is available from the audit thread at `.amp/in/artifacts/parser-warning-correctness-20260910/raw-audit.tar.gz`, **SHA256 `a4ab708e00c4b4c5d1adfd2acc328bc3d070eb4bdee18f0196ebcd01b13051ae`**. The bundle contains both original received archives and extracted raw data, complete independent build/test logs, exact commands/config files/hash manifests, the scratch check, the independent audit script/JSON output, and a 104-file integrity manifest. It excludes binaries, package caches, and generated scratch products.

The interrupted N4 confirmation is not treated as five pairs or as a running job. Neither that partial block nor the separately commissioned fresh five-pair block is included here. N1/tiny/heap work belongs to the other workers. This audit remains available for a later cumulative evidence review; no thread archival or further worker creation was performed.
