# Independent binary-only N1 specialization validation

This report belongs to `perf/specialization-n1-xxlarge-20260910` in
`https://github.com/vtrl/purescript`. It does not integrate or accept a compiler
change. The worker runs only the prespecified full-corpus N1 block, without
rebuilding either executable or changing compiler source, tests, configuration,
fixtures, or goldens.

All three pairs favor the candidate for wall time and allocation. The independent
within-pair mean is **−32.2495% wall time (sample SD 2.9184 percentage points)**
and **−43.6406% allocation (SD 0.000669 pp)**. Mean peak RSS rises **3.4177%**
and maximum sampled residency rises **5.3804%**, with large memory variation.
All eight runs, including the two excluded warmups, match the original products
and complete warning-content multiset. The final candidate's slower 143.89 s
sample and the first pair's larger memory effect are retained, not rerun.

## Source and executable identity

The report branch starts at the exact synchronized accepted campaign
[3f570156](https://github.com/vtrl/purescript/commit/3f57015676d590bc55758cac58b65a16ee15b59d).
Its entire tree outside `debug/` matches accepted Make
[38080a40](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7),
verified with `git diff --exit-code`; the baseline was not advanced. The checkout
was unshallowed before history inspection. Origin fetch and push URLs are both
exclusively `https://github.com/vtrl/purescript`.

The fetched specialization trial branch's source/config commit is
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63),
and its source worker's report checkpoint is
[5cffa728](https://github.com/vtrl/purescript/commit/5cffa728b8cd7dfc3c0e4e87091c66f486dce733).
The functional diff against accepted Make contains only these additions:

```diff
-  "$locals": -O2 -Werror
+  "$locals": -O2 -Werror -fspecialize-aggressively -fexpose-all-unfoldings
```

```cabal
package purescript
  ghc-options: -fspecialize-aggressively -fexpose-all-unfoldings
```

Compiler source, dependency/resolver versions, tests, and RTS defaults match
accepted Make. Neither a warning-force, traversal-INLINABLE, unifier, CST, nor
visibility trial is included. This measures the two flags together, not their
individual effects or interaction with any other proposed change.

Original author: **seastian <seastian@users.noreply.github.com>**. Authoritative
upstream: [purescript/purescript PR 4584](https://github.com/purescript/purescript/pull/4584),
commit [8ac0fb29](https://github.com/purescript/purescript/commit/8ac0fb2962a7df318a74216872465dc2868c6064).
[OxfordAbstracts PR 16](https://github.com/OxfordAbstracts/purescript/pull/16)
imported that work. No documented Claude authorship is claimed.

| Binary | Bytes | SHA256 |
| --- | ---: | --- |
| B: supplied accepted Make | 47869944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: final tested specialization | 70523064 | `4a918e8fe3b147cf1bd24adf26cc8286d3d72dbc0f5dbe928aa563166fd773c1` |

Both binaries were transferred from their designated workers, hash-checked,
and set to mode 0555 before timing. The candidate version embeds the clean full
source commit above. The baseline embeds an older dirty development version;
that string is not substituted for its supplied accepted-Make identity. The
executable hashes are the exact measurement identities. The earlier `c790...`
initial-build candidate was neither downloaded nor executed here.

The source worker records normal optimized builds with GHC 9.6.6, Stack 3.3.1,
and lts-22.43. Its optimized seed-9160 full suite passed **1302 examples, zero
failures**, without golden acceptance. Enabling tests triggered a rebuild and
relink because Cabal's library unit ID changed; the final tested binary above
is the one used here. These are source-worker build/test evidence, not a claim
that this binary-only worker reran the suite.

## Fixed corpus, hardware, and protocol

The unchanged `ci/prepare-package-set-benchmark.sh` prepared set 60.4.0 from
registry `5d834cd364da1d49a1bd1b0219ab49fb15f20601`, using Spago 0.93.43 and Node
22.23.2. It fetched **4084 PureScript sources / 4901 inputs**. Manifest SHA256:
`2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.

The unchanged paired harness is
[e9261835](https://github.com/vtrl/purescript/commit/e92618350a6dc9f6b7e842f6f818ae1aa08a1f5b),
`ci/benchmark-compiler.py`, SHA256
`fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399`.

This assigned a1.xxlarge worker exposes 16 logical CPUs, Intel Xeon @ 2.60 GHz,
family 6/model 106/stepping 6, one socket, eight cores, two threads/core, KVM,
one NUMA node, and effective CPUs 0–15. Physical memory is 32,880,800 KiB;
the workload cgroup limit is **32,212,254,720 bytes (30 GiB)**, with no swap.
CPU quota is `max 100000` at workload and parent cgroups. The service itself
has `memory.max=max` and inherits the workload memory limit. Kernel:
Linux 6.1.158+ x86-64. Full CPU/cache flags, cgroup hierarchy, memory, process
lists, and start/end observations are retained in the evidence archive.

The capability count is fixed at **N1**, regardless of these 16 exposed CPUs.
There is no CPU affinity pin or nursery override. The harness clears `GHCRTS`,
uses `LC_ALL=C` and `LANG=C`, and invokes `+RTS -N1 -s -RTS` for every run.
All setup, binary downloads, npm installation, and corpus fetching completed
before timing. No competing benchmark, build, or test ran on this orb.

```sh
python3 ci/benchmark-compiler.py \
  --corpus .build/package-set-benchmark \
  --baseline .build/perf/purs-parse-release \
  --compiler .build/perf/purs-specialization \
  --results .build/perf/n1 \
  --label '<full campaign, accepted Make, candidate and report identities>' \
  --capabilities 1 --samples 3 --cache warm --codegen js
```

The exact expanded command and label are in `n1/metadata.json` and the preserved
`run-once.sh`. Output is deleted before every compile; product hashing happens
after the timed process. One excluded B warmup and one excluded C warmup precede
the three measured pairs in fixed **BC / CB / BC** order. There are no adaptive
reruns, extra capabilities, discarded samples, or cross-orb raw-mean comparisons.

The long block runs through `amp orb service start specialization-n1 --command
'bash /home/user/workspace/repo/.build/perf/run-once.sh' --cwd
/home/user/workspace/repo`, without a portal. Its wrapper atomically creates a
per-block started directory, refuses reexecution if it exists, records the
actual harness exit status, then executes `sleep infinity`. This avoids both
CLI-update termination and service auto-restart reruns. Preparation used a
separate guard and service, stopped before timing.

## Every run and all three within-pair effects

The block started at **2026-09-10 13:31:20 UTC** and completed at
**13:54:09 UTC**, with harness exit status **0**. The service then contained
only `sleep infinity`; its status/logs were captured before it was stopped.
The start/end workload cgroup reports no CPU throttling. The first-pair
checkpoint was sent to the lead while candidate-2 was already running; the
block was not paused or restarted for the checkpoint.

Raw GNU-time wall/user/system values are in seconds; RSS is KiB; allocation and
maximum sampled residency are bytes. Every sample is shown in execution order.

| Run | Wall s | User s | System s | Peak RSS KiB | Allocated B | Sampled residency B |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Warmup B, excluded | 203.41 | 200.78 | 3.09 | 1816756 | 397547666408 | 621518392 |
| Warmup C, excluded | 135.37 | 132.71 | 3.08 | 2045316 | 224058221448 | 738429936 |
| B1 | 200.00 | 197.52 | 2.95 | 1797304 | 397552110456 | 612258464 |
| C1 | 135.78 | 133.24 | 2.98 | 1952156 | 224055911584 | 705170760 |
| C2 | 133.61 | 131.39 | 2.73 | 1816940 | 224058243912 | 622546520 |
| B2 | 206.30 | 203.59 | 3.22 | 1811892 | 397554121800 | 621027144 |
| B3 | 203.82 | 201.21 | 3.22 | 1805684 | 397545605568 | 612882936 |
| C3 | 143.89 | 141.51 | 2.88 | 1830220 | 224057334784 | 617303208 |

For each pair, the independent calculation uses \(100(C-B)/B\). Negative
changes are reductions. Pairs are matched by sample number, including the
candidate-first second pair. These are means of the three within-pair changes,
not the ratio of pooled wall-time means. Sample SD uses the \(n-1\) denominator;
it is expressed in percentage points and is not a confidence interval.

| Pair / order | Wall change % | Allocation change % | RSS change % | Sampled residency change % |
| --- | ---: | ---: | ---: | ---: |
| 1 / BC | −32.110000 | −43.641121 | +8.615793 | +15.175339 |
| 2 / CB | −35.235095 | −43.640820 | +0.278604 | +0.244655 |
| 3 / BC | −29.403395 | −43.639841 | +1.358820 | +0.721226 |

| Metric | Mean change % | Sample SD pp | Median change % | Range of changes % |
| --- | ---: | ---: | ---: | ---: |
| Wall time | −32.249497 | 2.918351 | −32.110000 | [−35.235095, −29.403395] |
| Allocation | −43.640594 | 0.000669 | −43.640820 | [−43.641121, −43.639841] |
| Peak RSS | +3.417739 | 4.533932 | +1.358820 | [+0.278604, +8.615793] |
| Maximum sampled residency | +5.380407 | 8.486006 | +0.721226 | [+0.244655, +15.175339] |

Candidate wall time spans 133.61–143.89 s and baseline spans 200.00–206.30 s.
Allocation savings are stable across pairs; wall and memory values are not
constant. In particular, the first pair's memory increases are much larger
than the later pairs'. Sampled residency is a GC-sampled maximum, not total
allocation or RSS. No sample, including a potential outlier, was removed.

## Build and executable tradeoff

The measured candidate is **22,653,120 bytes / 47.3222% larger**. The source
worker reports its clean optimized compiler build took 768.70 s with GNU-time
max RSS 4,152,924 KiB. There is no same-machine baseline clean-build timing,
so neither relative build-time nor build-RSS growth is quantified. The subsequent
test-enabled rebuild is separate from those clean-build values and from this
worker's compilation measurements.

## Independent correctness checks and transferable evidence

`python3 .build/perf/analyze.py` independently validated **8 completed runs**,
each with **8985 original products**, **745 warnings**, and **738 distinct
warning bodies**. It checked every raw GNU-time value against the row emitted
by the harness, parsed allocation and residency again from RTS output, verified
`using -N1`, and recomputed all paired statistics. It also checked the exact run
order and run count, with no extra completed run accepted. Its paired values
agree with the unchanged harness to less than 1e-10 percentage points.

The harness hashes every run's products after the timed process and compares
them with the first run before emitting a completed row. The shared manifest
matches the original accepted-Make reference, SHA256
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`:
4084 `index.js`, 817 `foreign.js`, and 4084 `externs.cbor` files. The independent
validator also rehashed the final actual output tree against that reference.
The unmodified harness retains one shared successful product manifest rather
than eight identical per-run manifests; each emitted row is evidence that that
run passed the full comparison. No fabricated per-run manifest is substituted.

Warning validation compares the complete content multiset against the original
lead-provided `baseline-n1-warm/1.stdout` (raw SHA256
`6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280`).
Only the numbered `Warning N of M:` headers and outer body whitespace are
removed. Duplicate counts and all internal whitespace are retained. Each raw
stdout is preserved. The canonical sorted body-to-count JSON, serialized with
`ensure_ascii=False`, indentation 2, and a final newline, has SHA256
`21c39f0ba7c0a8370f1027327ad0c2559cd688dde940896e02c18b8f7626f436`.

After timing, all 4901 input hashes, both executable hashes and read-only modes,
and the harness hash were rechecked. No compiler source/test/configuration
change or rebuild occurred in this worker. Validation code and wrappers live
only in ignored `.build/perf/`; this branch contains only this report.

The complete transferable archive is
`.amp/in/artifacts/specialization-n1-xxlarge-20260910.tar.gz`, with its SHA256 in
the adjacent `.tar.gz.sha256` file and the delivery message. It includes:

- Both exact read-only executables, the full prepared corpus and final output
  tree, input/source manifests, package-set and Spago lock/configuration files.
- All eight `.time`, `.stdout`, and `.stderr` records, full expanded command and
  machine metadata, shared products, sample rows, summary, and paired changes.
- Original product/warning references, canonical warning bodies with counts,
  per-run raw-file hashes and validation certificates, independent final product
  manifest, independent analysis script/output, and the first-pair checkpoint.
- Preparation log, guarded wrappers, started/completed/exit records, service
  status/logs and shutdown evidence, full hardware/cgroup observations, source
  identity/diff evidence, source-worker report, exact harness, and this report's
  commit patch. Benchmark setup npm dependencies themselves are excluded; their
  package manifests are included, and no Node tool is needed to audit the data.

To audit without compiling or fetching anything, extract the archive into an
empty directory and run `python3 .build/perf/analyze.py` there. The bundle
retains its `.build/` paths. The started guard is intentionally preserved:
do not run the measurement wrapper as an audit or silently remove the guard.

## Bounded recommendation

This independent fixed-N1 full-corpus block supports a substantial wall-time
and allocation improvement for the two flags together, with exact product and
warning-content preservation. Consider it alongside the separately owned N4
confirmation and N8/boundary evidence, without pooling raw means across orbs
or treating this three-pair point estimate as universal. The executable grows
47.3222%; all three measured RSS/residency changes are positive, but their
magnitudes vary substantially. The available build evidence does not quantify
relative build cost.

No cold-cache, incremental, startup, IDE, tiny/dense, alternative-capability, or
combined-candidate conclusion follows from this worker's data. No extra block
was run, and no compiler integration or acceptance is made here. This worker
remains available for a later explicitly supplied synchronized accepted combined
binary; no unspecified cumulative comparison has been started.
