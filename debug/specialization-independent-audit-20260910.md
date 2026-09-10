# Independent offline audit of the isolated specialization flags

**Verdict: the supplied raw evidence agrees with the reported statistics,
products, warnings, and final executable identities. No material contradiction
was found.** This is an evidence audit, not source acceptance or a rebuild.
No compiler, worker analysis script, benchmark wrapper, or test was executed
here. No new timing samples or combined-candidate conclusions were added.

This report-only branch, `perf/specialization-independent-audit-20260910`, starts
at exact accepted report checkpoint
[c44600e8](https://github.com/vtrl/purescript/commit/c44600e8f7a390ad73a46f8932e8f90a21b40a8a).
Its compiler source/test/configuration matches accepted Make
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The repository was unshallowed before history inspection; origin fetch and
push are exclusively `https://github.com/vtrl/purescript`.

## All eight independently recomputed distributions agree

An independent Python reader parsed every raw `.time`, `.stderr`, and `.stdout`
before comparing with worker JSON: **120 complete runs, 52 measured pairs,
16 excluded warmups**. Each block starts with excluded B/C warmups, then fixed
BC/CB alternating pairs beginning BC. No recorded samples are missing, extra,
or discarded. For each metric it computes \(100(C_i/B_i-1)\), then the mean
and sample SD of those paired changes, not a ratio of group means. SD uses
\(n-1\) and is in percentage points, not a confidence interval. Machines,
capability settings, and the separate screen are not pooled.

| Block | Pairs | Wall change % ± SD pp | Peak RSS change % ± SD pp | Allocation change % ± SD pp | Sampled residency change % ± SD pp |
| --- | ---: | ---: | ---: | ---: | ---: |
| N4 screen, separate | 1 | −32.666040; no SD | +3.069520; no SD | −43.617022; no SD | +9.287910; no SD |
| N4 confirmation | 5 | −29.586016 ± 2.733698 | +4.193179 ± 4.276131 | −43.629611 ± 0.002135 | +6.133728 ± 7.681501 |
| Full N1 | 3 | −32.249497 ± 2.918351 | +3.417739 ± 4.533932 | −43.640594 ± 0.000669 | +5.380407 ± 8.486006 |
| Full N8 | 3 | −29.727265 ± 1.168095 | +10.860666 ± 4.326520 | −43.630813 ± 0.001815 | +15.820723 ± 8.162666 |
| Dense1000 N1 | 10 | −20.605367 ± 6.564159 | +15.814554 ± 0.049489 | −43.741197 ± 0.000983 | +15.492779 ± 0.000000 |
| Tiny N1 | 10 | −39.790122 ± 5.192040 | +8.574874 ± 2.183244 | −49.674083 ± 0.000134 | −1.873794 ± 11.873751 |
| Tiny N4 | 10 | −43.500633 ± 2.626689 | +9.246350 ± 2.426674 | −49.578167 ± 0.027562 | +3.984877 ± 8.691995 |
| Tiny N8 | 10 | −42.052822 ± 1.802810 | +6.129482 ± 1.822527 | −49.617766 ± 0.042453 | +0.452090 ± 9.011001 |

Saved means, medians, ranges, sample SDs, and individual paired changes match
within floating-point tolerance. N4's extra copied-byte, MUT/GC, collection,
and pause summaries also agree. Every raw GNU-time row agrees with its emitted
row. The separately recorded monotonic timer is consistent with GNU time but
cannot be remeasured offline.

The reader checked the complete argv, source list, package-set metadata, exact
binary identities, explicit capabilities in argv and RTS output, and complete
unique module counters: 4084 per full run, 57 per tiny run, 1 per dense run.
Module-name sets agree within each fixture. Stderr before RTS statistics
contains only those compilation counters, with no additional diagnostics.
Completion files and emitted rows confirm the reported block ends.

## Actual bytes, products, and complete warning contents agree

The supplied N1 archive permits direct hashing without executing either binary.
Both actual read-only ELF files match every timing block's metadata:

| Executable | Bytes | SHA256 |
| --- | ---: | --- |
| B: accepted Make | 47869944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: final tested specialization | 70523064 | `4a918e8fe3b147cf1bd24adf26cc8286d3d72dbc0f5dbe928aa563166fd773c1` |

C is 22,653,120 bytes / **47.322220% larger**. The earlier `c790...` candidate
is the initial compiler-only build, not measured C. N4 `binary-formats.txt`
describes that initial build; `tested-binary.json` and all timing metadata
identify final C. This is documented provenance, not interchangeable identities.
B's older dirty embedded version is not substituted for its assigned hash.

Independently rehashed inputs: full **4901**, tiny **78**, dense **1**. Full
`inputs.json` SHA256 is
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
The full corpus has 4084 PureScript sources and set 60.4.0 metadata.

Independently rehashed retained final products: N1 full **8985**, boundary full
**8985**, tiny **135**, dense **2**. Both full trees match the original map,
SHA256 `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
Dense uses its unchanged local source path and local B/C reference because
externs can embed absolute paths; this is not a cross-orb externs claim.
Boundary integrity manifests pass for **444 raw files and 9122 products**.

All **32 full runs** preserve the exact original **745-warning content multiset
(738 distinct bodies)**: 737 bodies occur once, one occurs eight times. All
**66 tiny runs** preserve their exact three warnings; **22 dense runs** have
none. The comparison removes ordinal headers and outer newline padding,
preserving duplicates and internal whitespace. Reordering passes; duplicate
loss and an internal-whitespace mutation fail. This stricter comparison also
agrees with the workers' outer-whitespace-normalized canonical hash:
`21c39f0ba7c0a8370f1027327ad0c2559cd688dde940896e02c18b8f7626f436`.
The newline-only canonical hash is
`ff88352d7dbe03c782ee39603fb62d9b611d12d3557fabcb803046455facee70`;
serialization/outer trimming explains the difference.

**Zero full-corpus warnings are `WarningParsingModule`.** These records do not
establish parser-warning coverage or exact heap retaining roots.

Per-run product equality is supported by the unchanged harness, not direct
rehashing of 120 retained trees. `ci/benchmark-compiler.py` lines 93–103 clear
output and require compile success; lines 111–126 hash/compare products before
emitting each completed row. It saves only the first map; successful trees are
deleted before the next run. All block maps match their references. This audit
directly rehashed the final trees listed above; N4's supplied archive retains
maps and assertions, not a final tree.

## Source, build, and full test evidence match the stated isolated flags

Git and the archived patch confirm that source/config commit
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63)
changes only `stack.yaml` and `cabal.project`: the two flags
`-fspecialize-aggressively -fexpose-all-unfoldings`. Existing `-O2 -Werror`,
resolver, dependencies, source, tests, and RTS defaults are unchanged. No
warning-force, traversal, visibility, CST, unifier, or other candidate is included.
Local Git credits seastian and records upstream
[PR 4584](https://github.com/purescript/purescript/pull/4584), commit
[8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
Archived upstream patch SHA256:
`9982045a76b085f54799c60ee2d87bc57c055f0636403e197f76692ba2720b15`.
This offline audit did not independently query GitHub's upstream history.

All three harness copies equal
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b),
SHA256 `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.
Its environment fixes locale and clears `GHCRTS`; recorded argv adds only the
prescribed `+RTS -N1/-N4/-N8 -s -RTS`, with no extra RTS overrides.

Both captured 382-argument GHC library commands contain GHC 9.6.6, normal
`-O2 -Werror`, and both specialization flags. They differ only in Cabal's
library unit ID, explaining the test-enabled rebuild. Both logs record all
180 library compilations. The supplied full test log has **1302 passing
examples, zero failures**, seed 9160/no-color, Hspec elapsed 95.5453 s
(`n4/specialization-trial/full-tests.log` lines 2368–2374). Its `.time` line 1
records `env -u HSPEC_ACCEPT -u GHCRTS CI=true`; exit status is 0. No golden
acceptance is enabled. This audits supplied logs, not a test run here.

Raw initial build cost: 768.70 s, GNU-time max RSS 4,152,924 KiB. The distinct
cgroup peak is 4,601,724,928 bytes, not RSS. The 1026.93 s full test command
includes the subsequent rebuild and is not a benchmark sample. There is **no
matched baseline build-cost measurement**. Stack 3.3.1 is worker-reported;
no separate `stack --version` capture was found. GHC 9.6.6 is directly visible
in captured commands; lts-22.43 is pinned in Git.

## Material limits remain

- N4 baseline declines from 129.14 to 118.51 s and paired effects shrink from
  −33.6147% to −26.9429%. Keep that drift and the favorable separate one-pair
  screen visible; screen selection is not replication.
- Full N1/N8 have three pairs each; N4 has five. Fixed alternating order is
  not randomization. SD does not calibrate shared-host drift. These observations
  support the recorded workload effect, not a universal speedup.
- Boundary cgroup limits change from 32,212,254,720 bytes (30 GiB;
  `environment/machine.txt` lines 550–551) to 31,675,383,808 bytes (29.5 GiB;
  `environment/final-checks.txt` line 21). Transition time was not sampled;
  a constant per-run limit cannot be asserted or retroactively assigned.
- Higher mean RSS accompanies lower allocation in every block. Sampled
  residency varies with GC and is not RSS. Executable size is not resident
  memory. Tiny/dense wall times are short and quantized to 0.01 s by GNU time.
- Clean warm-cache compiles do not isolate startup, cold executable paging,
  incremental compilation, or IDE work. Relative build-cost and cumulative
  candidate interactions are unmeasured. No new acceptance threshold is added.

## Executed checks and transferable evidence

Executed here: `python3 tmp/specialization-audit/audit.py`, exit 0:

```text
PASS: all archives, actual binaries, input bytes, retained final products, and all 120 raw runs.
```

`tmp/specialization-audit/independent-results.json` retains every independently
parsed row, paired calculation, distribution, warning count/hash, archive
identity, direct file check, source object, and build/test check. The companion
script/output, source snapshot, supplied raw logs/metadata and full build/test
logs are in the audit bundle. Binaries, caches, and duplicate corpus/product
trees are excluded; repeat direct byte checks with the original archives.

All original archives passed exact size/SHA256 checks:

| Worker / artifact | Bytes | SHA256 |
| --- | ---: | --- |
| [N4 worker](https://ampcode.com/threads/T-01a08ab3-d46b-76a9-a89a-bfbaebce03d8), `.amp/in/artifacts/specialization-flags-20260910-evidence.tar.gz` | 2491252 | `267b98d0d5bbcdde20387e02962f329fe877fd77a23549d71f41e786812dd51d` |
| [N1 worker](https://ampcode.com/threads/T-01a08b7a-0c64-7514-8a41-d8fa5cd246b9), `.amp/in/artifacts/specialization-n1-xxlarge-20260910.tar.gz` | 65425517 | `71f428a414ed9756d5c00b56070ffaddc6d78fb2b3a98d63e29f8cf3386894f4` |
| [Boundary worker](https://ampcode.com/threads/T-01a08b7b-01a8-7007-b2c0-01d41c78f59d), `.amp/in/artifacts/specialization-boundaries-20260910/raw-evidence.tar.gz` | 2020530 | `dc949a495ae689ff37fd6c2c2d0603603a103133882a42ce7db0e2d74af48d00` |
| Same boundary prefix, `verified-products.tar.gz` | 19111996 | `51d6c43c261fe7e6c249e8c5863cec4c0405ff1e438a1eae33883203dda089a6` |

Final reports were read separately from Git:
[N4](https://github.com/vtrl/purescript/commit/cb4e5d58b0728c5a2de3a116b11eefffc75187a0),
[N1](https://github.com/vtrl/purescript/commit/1467ecd5e669743a7b76daa9262ec29210a4a024),
and [boundaries](https://github.com/vtrl/purescript/commit/ad45e204e1e2e1f80b47fb6fe21456ce86d12895).
N4's archive intentionally predates final report insertion; no raw sample is
missing. These reports and their hashes are retained in this audit bundle.

Bundle in [this thread](https://ampcode.com/threads/T-01a08b71-7140-741c-b98f-a8a370d2b222):
`.amp/in/artifacts/specialization-independent-audit-20260910/raw-audit.tar.gz`.
Size **7,562,531 bytes**; SHA256
`69431d8a570db757443362d79e5a8f879177885e946bcf7035188cbdf1223611`.
All **548 retained regular files** passed archive round-trip hashing; the
additional root `SHA256SUMS` enables independent integrity verification.
The earlier parser-warning audit branch/artifact remain unchanged; its raw SHA256 was rechecked as
`a4ab708e00c4b4c5d1adfd2acc328bc3d070eb4bdee18f0196ebcd01b13051ae`.
