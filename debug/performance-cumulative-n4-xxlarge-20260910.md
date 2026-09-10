# Cumulative warning forcing + specialization: five full-corpus N4 pairs

**This N4 block favors the cumulative candidate; source acceptance remains
pending the complete synchronized matrix and independently owned audit.** All
five measured pairs have lower wall time, peak RSS, allocation, and maximum
sampled residency. These are directly measured cumulative-binary effects on one
pinned workload, not sums of standalone effects and not universal claims.

| Metric | Mean paired change | Sample SD (percentage points) | Median paired change | Paired range |
| --- | --- | --- | --- | --- |
| Wall | −31.524660% | 1.373529 | −31.523643% | −33.503370% to −29.683208% |
| Peak RSS | −18.723925% | 4.728211 | −18.688337% | −24.922499% to −12.272350% |
| Allocation | −43.628408% | 0.002146 | −43.629106% | −43.631029% to −43.625376% |
| Maximum residency | −13.049836% | 7.839196 | −9.524322% | −23.848806% to −4.581055% |

The experiment is exactly the prescribed **five pairs**, with two excluded clean
warmups and then **BC/CB/BC/CB/BC**, fixed **N4**, warm cache, JS codegen. All
outliers remain. There was no preliminary screen, extra warmup, capability
expansion, retry, sample substitution, or pooling with previous experiments.

## Exact source and frozen binaries

The report-only branch `perf/cumulative-n4-xxlarge-20260910` was created at exact
pushed campaign checkpoint
[8bc296a5d4218180c078d5ff481c4c4b6cb761fe](https://github.com/vtrl/purescript/commit/8bc296a5d4218180c078d5ff481c4c4b6cb761fe).
This checkpoint remains compiler/build-configuration equivalent to accepted
Make-only [38080a40fc3a53de813a0ac46187e8c709dcbcc7](https://github.com/vtrl/purescript/commit/38080a40fc3a53de813a0ac46187e8c709dcbcc7).
The checkout was already unshallowed; origin fetch and push were verified
exclusively as `https://github.com/vtrl/purescript`, then origin was fetched.
The post-timing fetch still found that campaign checkpoint. No moving default
branch was substituted, and no source/config/test/harness changes were made.

Experimental candidate source:
[d0d4b3313f37ad9f79997eb6a852dc83aa2f0336](https://github.com/vtrl/purescript/commit/d0d4b3313f37ad9f79997eb6a852dc83aa2f0336).
It combines warning-force source/test
[2d5cc3bb](https://github.com/vtrl/purescript/commit/2d5cc3bb3b766551e0b6c341dc84fc760915298f)
with attributed specialization flags
[c7df45fb](https://github.com/vtrl/purescript/commit/c7df45fbb92b824ca7ae3e89b6e8f0d25bcb3c63):
`-fspecialize-aggressively -fexpose-all-unfoldings`. The flags retain seastian's
upstream provenance; no other trial was included. The exact combined source,
test, and configuration patch was archived **unapplied**, SHA256
`e1975195ba218be2fd423a9f32d0099950581cb11a95e9d5941146f81a4bd677`.

The candidate was transferred directly from the
[lead](https://ampcode.com/threads/T-01a08a95-ee2d-757c-9930-0bba3e678f5c)
at `.build/perf/purs-cumulative-tested`; B reused the previously preserved exact
Make binary. Both were retained mode **0555** and rehashed after timing.

| Identity | Bytes | SHA256 |
| --- | --- | --- |
| B: preserved Make `bin/purs-baseline` | 47,869,944 | `7e56d47ac5cdbb7e13e278f97b60abe2311b642bbb3dc545b86139e4da7a1b3a` |
| C: frozen `bin/purs-cumulative-tested` | 70,523,064 | `4c80fd2b411d53b82b0aca5dce5a2e4463e9cbf54413b7a90274d152779cba2d` |

Supplied build provenance is normal optimized **GHC 9.6.6 / Stack 3.3.1 /
lts-22.43**, not fast or profiling builds. Both executables independently report
GHC 9.6.6 and RTS way `rts_thr`. Candidate version text names its exact source;
the baseline retains its original development/dirty version text, so binary
hashes and supplied source provenance, not that text, identify B:

```text
B: 0.15.15 [development build; commit: 6d636561f72b7985962e2931e462aa017a38f7fc DIRTY]
C: 0.15.15 [development build; commit: d0d4b3313f37ad9f79997eb6a852dc83aa2f0336]
```

No rebuild or compiler test suite ran here. The supplied full **1,303/0** and
Make **14/0** results belong to the separately owned
[cumulative correctness trial](performance-cumulative-warning-specialization-20260910.md),
whose provenance report is copied into the archive. The standalone audit and
lead's 120-record replay are external evidence, not additional samples here.
Neither the warning-only nor flags-only executable was substituted for C.

## One uninterrupted supervised invocation

Worker: [existing xxlarge N4 orb](https://ampcode.com/threads/T-01a08b75-7528-73ec-a04d-cb025d6b814e).
No new thread or orb was created. Actual machine metadata:

- Linux **6.1.158+**, x86-64, KVM; Intel Xeon @ 2.60 GHz, family 6/model 106.
- **16 logical CPUs**, eight cores × two SMT, effective CPUs **0–15**.
  Fixed N4 was used despite the larger worker. Ancestor CPU quota is
  `max 100000`; no throttled periods/time were recorded.
- `MemTotal=32880800 KiB`, no machine swap. Workload
  `memory.max=31675383808` bytes (**29.5 GiB**),
  `memory.high=31138512896` bytes (**29 GiB**), unchanged between this block's
  before/after snapshots. The former standalone block had a 30 GiB maximum;
  this worker ran no limit-changing command. Actual metadata, not an assumed
  prior limit, is used here. No high/max/OOM memory events were recorded.
- Timing began **2026-09-10T14:44:58Z**, completed **15:03:24Z**. Benchmark
  exit status **0**, service **NRestarts=0**. There was exactly one invocation
  and exactly 12 clean compiles. All setup/transfer/corpus verification finished
  first; only brief status reads ran alongside timing, with no competing builds,
  tests, downloads, setup, or other benchmark workload.

The proven wrapper atomically creates a per-block `started` directory. A
duplicate start sleeps indefinitely instead of rerunning. After the benchmark
returns it atomically records the actual exit status, captures after-metadata,
and `exec sleep infinity`. Completion files, service status/journal, and raw
samples were collected before stopping the service. Final service status showed
its main process sleeping and no compiler child. No service remains running.
An immediate start-time argv read raced the initial metadata snapshot; the
following read found the expected running harness. No restart was attempted.

Exact service command:

```sh
amp orb service start cumulative-n4-confirmation \
  --command 'bash /home/user/workspace/repo/.build/perf/cumulative-n4-confirmation/run-once.sh' \
  --cwd /home/user/workspace/repo
```

No portal was requested. Exact benchmark argument vector, shell-wrapped only
for readability:

```sh
python3 /home/user/workspace/repo/ci/benchmark-compiler.py \
  --corpus /home/user/workspace/repo/.build/package-set-benchmark \
  --baseline /home/user/workspace/repo/.build/perf/cumulative-n4-confirmation/bin/purs-baseline \
  --compiler /home/user/workspace/repo/.build/perf/cumulative-n4-confirmation/bin/purs-cumulative-tested \
  --capabilities 4 --samples 5 --cache warm --codegen js \
  --results /home/user/workspace/repo/.build/perf/cumulative-n4-confirmation/results \
  --label 'cumulative-warning-force-and-specialization-full-corpus-N4-five-pairs; baseline-source=38080a40fc3a53de813a0ac46187e8c709dcbcc7; report-base=8bc296a5d4218180c078d5ff481c4c4b6cb761fe; candidate-source=d0d4b3313f37ad9f79997eb6a852dc83aa2f0336; GHC9.6.6-Stack3.3.1-lts22.43-normal-optimized; frozen-tested-binary-no-rebuild'
```

`argv.sh` preserves the shell-escaped argv; `results/metadata.json` preserves the
full compiler command with all 4,084 ordered source paths. Each invocation used
`purs compile <sources> --output <corpus>/benchmark-output --codegen js +RTS -N4 -s -RTS`,
GNU time `-f '%e %U %S %M'`, `LC_ALL=C`, `LANG=C`, and empty `GHCRTS`.
The output directory was deleted before each run, including both warmups.
Warm cache means OS page cache, not an incremental/no-op build.

## Every sample and independently computed summary

Raw `.time` and RTS `.stderr` records were parsed independently, without importing
the harness. For pair \(i\), change is \(100(C_i-B_i)/B_i\); the reported mean
is the arithmetic mean of the five paired changes, not a ratio of group means.
Sample SD uses denominator **4**. All reconstructed rows, ordering, group
summaries, paired means, medians, SDs, and ranges match the harness output.
B/C columns always list baseline then candidate, irrespective of run order.

| Pair | Order | Wall B / C (s) | Wall change | RSS B / C (KiB) | RSS change |
| --- | --- | --- | --- | --- | --- |
| 1 | BC | 108.88 / 75.00 | −31.116826% | 1,965,324 / 1,636,964 | −16.707678% |
| 2 | CB | 108.49 / 74.29 | −31.523643% | 2,016,748 / 1,514,124 | −24.922499% |
| 3 | BC | 109.78 / 73.00 | −33.503370% | 2,074,112 / 1,637,952 | −21.028758% |
| 4 | CB | 107.78 / 73.51 | −31.796252% | 2,002,420 / 1,756,676 | −12.272350% |
| 5 | BC | 109.22 / 76.80 | −29.683208% | 1,961,480 / 1,594,912 | −18.688337% |

| Pair | Residency B / C (bytes) | Residency change | Allocation B / C (bytes) | Allocation change |
| --- | --- | --- | --- | --- |
| 1 | 677,324,440 / 616,774,960 | −8.939509% | 397,636,465,456 / 224,166,062,880 | −43.625376% |
| 2 | 695,558,472 / 567,885,336 | −18.355486% | 397,628,186,648 / 224,146,113,152 | −43.629219% |
| 3 | 748,086,736 / 569,676,984 | −23.848806% | 397,631,221,056 / 224,155,422,872 | −43.627308% |
| 4 | 693,367,280 / 661,603,744 | −4.581055% | 397,643,636,656 / 224,147,627,560 | −43.631029% |
| 5 | 671,793,896 / 607,810,080 | −9.524322% | 397,628,860,712 / 224,146,942,440 | −43.629106% |

Excluded warmups B/C: wall **108.49 / 71.85 s**; RSS **1,981,004 / 1,636,932 KiB**;
residency **698,654,128 / 609,430,856 bytes**; allocation
**397,630,539,400 / 224,152,676,696 bytes**. Their full raw logs and correctness
checks are retained but enter none of the measured summaries.

| Variant | Metric/unit | Mean | Sample SD | Median | Range |
| --- | --- | --- | --- | --- | --- |
| B | Wall/s | 108.830 | 0.754 | 108.88 | 107.78–109.78 |
| C | Wall/s | 74.520 | 1.484 | 74.29 | 73.00–76.80 |
| B | RSS/KiB | 2,004,016.800 | 45,783.410 | 2,002,420 | 1,961,480–2,074,112 |
| C | RSS/KiB | 1,628,125.600 | 87,748.773 | 1,636,964 | 1,514,124–1,756,676 |
| B | Allocation/bytes | 397,633,674,105.600 | 6,448,690.734 | 397,631,221,056 | 397,628,186,648–397,643,636,656 |
| C | Allocation/bytes | 224,152,433,780.800 | 8,483,689.377 | 224,147,627,560 | 224,146,113,152–224,166,062,880 |
| B | Residency/bytes | 697,226,164.800 | 30,196,642.364 | 693,367,280 | 671,793,896–748,086,736 |
| C | Residency/bytes | 604,750,220.800 | 38,650,458.802 | 607,810,080 | 567,885,336–661,603,744 |

## GC, mutator, copy, and pause records remain available

All RTS text, including sparks and work balance, is retained for all 12 runs.
`rts-details.json` also parses copy bytes, all five RTS phase CPU/elapsed times,
generation collection/parallel-collection counts, and average/maximum pauses.
The table below shows every measured run; times and pauses are seconds.

| Run | Bytes copied during GC | MUT CPU / elapsed | GC CPU / elapsed | Gen0 avg / max pause | Gen1 avg / max pause |
| --- | --- | --- | --- | --- | --- |
| baseline-1 | 68,185,382,120 | 148.449 / 44.569 | 131.710 / 64.255 | 0.0017 / 0.0060 | 0.1269 / 0.3060 |
| candidate-1 | 60,650,899,816 | 99.780 / 30.086 | 88.453 / 44.856 | 0.0019 / 0.0097 | 0.0874 / 0.1887 |
| baseline-2 | 68,033,696,824 | 148.230 / 44.673 | 130.644 / 63.743 | 0.0017 / 0.0080 | 0.1262 / 0.2773 |
| candidate-2 | 60,889,526,280 | 98.211 / 29.889 | 87.793 / 44.343 | 0.0019 / 0.0095 | 0.0858 / 0.1666 |
| baseline-3 | 67,976,794,520 | 150.617 / 45.087 | 132.233 / 64.628 | 0.0017 / 0.0118 | 0.1297 / 0.3136 |
| candidate-3 | 60,944,890,776 | 95.992 / 29.118 | 86.044 / 43.829 | 0.0019 / 0.0080 | 0.0852 / 0.1760 |
| baseline-4 | 68,051,429,560 | 146.092 / 43.664 | 129.593 / 64.046 | 0.0017 / 0.0076 | 0.1212 / 0.2407 |
| candidate-4 | 60,894,241,288 | 96.893 / 29.387 | 86.640 / 44.068 | 0.0019 / 0.0078 | 0.0866 / 0.2508 |
| baseline-5 | 67,537,319,936 | 148.615 / 45.006 | 130.653 / 64.156 | 0.0017 / 0.0150 | 0.1239 / 0.2793 |
| candidate-5 | 60,410,140,576 | 101.723 / 30.827 | 90.005 / 45.914 | 0.0019 / 0.0097 | 0.0884 / 0.1768 |

The archive also contains measured-group summaries for these RTS fields. CPU
times accumulate work across threads and must not be read as elapsed wall time.
Sampled maximum residency and process peak RSS are different measurements.

## Original-reference correctness and post-timing rehashes pass

The reused corpus was prepared with the unchanged pinned setup script, Spago
**0.93.43**, package set **60.4.0**, registry commit
[5d834cd364da1d49a1bd1b0219ab49fb15f20601](https://github.com/purescript/registry/commit/5d834cd364da1d49a1bd1b0219ab49fb15f20601).
Before this block, all **4,901** actual source/FFI files were independently
hashed against the manifest, with **4,084** PureScript sources. The same complete
input-byte verification passed after timing. No preparation/fetch was timed.

| Object | SHA256 |
| --- | --- |
| Unchanged `ci/benchmark-compiler.py`, [e926183](https://github.com/vtrl/purescript/commit/e926183) | `fda1d0fcff33506aca529c8ccf6cd892fff623c08cd4e89b68764584a05d2399` |
| Unchanged `ci/prepare-package-set-benchmark.sh` | `2c58732de6df1b38ab77b18e8083225d50dd9ee8a11f9b9c1f8d203f6e3e8c95` |
| Package-set JSON | `8cab74af472f4f4e142d2ff04e630a2aaf604b7af40987aacab0ae704fdadbb0` |
| `inputs.json` | `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff` |
| `purs-files.json` | `f157d71fde6dd11db4946391b7e6fe3a719d85ad5aaf7a552c2720dcdc549936` |
| Original and current 8,985-product manifest | `f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed` |
| Original lead `baseline-n1-warm/1.stdout` | `6d886df55458c8f76fe20a652a035e05656d063b1c24d0fb45a4f2ce612d5280` |
| `run-once.sh` | `fd5fccfdb4b087c842a768db2d753f7ea09624b19c711014ebde902a2bab6f36` |
| Independent analysis script | `0987c0d0837968e5b798100788c437ea7b7cc51bf285705aa184526201044339` |
| Independent analysis JSON | `16f6fa1670f4256ad2764f8fab2d461c18744d21f90a26d5f74a6c0584c93317` |
| Parsed RTS detail JSON | `caf0cf3f9e53bdae6678b094c1fc112534342caba0e131e894abd478057fd041` |
| Retained complete final-tree manifest | `e2b6366c31ee02c889378db84f21775eaa2fe55cc99781ee44bd78d0d561502a` |

`python3 .build/perf/cumulative-n4-confirmation/analyze.py` exited **0**:

```text
PASS: 12 clean runs; 5 complete measured pairs; exact prescribed order; -N4 throughout.
PASS: all 12 transcripts match original 745-warning / 738-distinct multiset, preserving duplicates and internal bytes.
PASS: original 8985 products match; final products and all 4901 inputs rehashed unchanged.
PASS: retained final tree has 8987 files; every file directly rehashed equal to final output.
PASS: independent raw-row reconstruction and mean/sample-SD statistics match harness output.
PASS: both exact binaries and copied harness rehashed; all GC/MUT/copy/generation-pause data parsed and retained.
```

The warning checker removes only ordinal headers and outer body whitespace,
preserving internal bytes and duplicate multiplicity in a `Counter`. It checks
745 ordered headers, 745 bodies, and 738 distinct bodies against the original
lead transcript, not merely against another run in this block. All 12 Counters
match. Warning locations and content are not normalized away.

The unchanged harness verifies inputs at block entry and asserts product-map
equality **after every run**. Its common product map matches the original
8,985-product reference, establishing original equality for all 12 runs by
transitivity. Separately, the post-timing audit directly rehashes the live final
candidate-5 product tree and every file in its retained copy. The full retained
tree has **8,987 files**: the 8,985 products plus `cache-db.json` and `package.json`.
Intermediate output trees are not claimed to be retained. Every stderr records
all 4,084 distinct compilation progress ordinals and explicit RTS use of N4.

## Complete raw archive and bounded conclusion

Worker-relative transfer path:
`.amp/in/artifacts/cumulative-n4-xxlarge-20260910-raw.tar.gz`

Size **48,957,092 bytes**; SHA256:
`6cd5b71d3e46ca228ce2ec5b0c0f6336b840c146cec73d72fbeefe36dcc8df6f`.
Download from the worker thread linked above. The archive includes both exact
binaries, all 12 raw streams/time records, unchanged harness/setup script,
supplied build provenance, full argv/machine/cgroup metadata, original references,
unapplied cumulative patch, corpus manifests, independent statistics/checks,
parsed RTS details, managed-service guard/completion evidence, and the complete
retained **final output tree**. `gzip -t` passed; all **9,069 regular payload
checksums plus the internal manifest** were verified by reading the compressed
archive, with no duplicate or unexpected regular files. Fetched corpus inputs
and npm tools are not archived; the inputs remain on the worker and are fully
identified by the pinned manifest and before/after byte checks.

The earlier standalone warning branch, report, raw archive, binaries, and final
tree remain separate and preserved. Its archive still hashes to
`9243d1e91606bc35e5dd2653bd679f607a59e6ad3c71846151d1543d903a1279`;
its former corpus output was preserved at
`.build/perf/standalone-n4-retained-final-tree` before corpus reuse. No old sample
was incorporated here, and no raw wall mean is compared across orb types.

These measurements support the cumulative candidate on the pinned full corpus
at N4. They do not isolate the two changes' individual contributions, prove
additivity, establish behavior at other capability counts or tiny/dense workloads,
measure rebuild cost, or justify source acceptance before the complete matrix
and independent audit. No accepted-source integration or default-branch push
was performed; this branch contains only this report.
