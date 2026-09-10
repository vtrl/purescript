# RTS nursery trial: retain current defaults

Do not change `purescript.cabal` or recommend a larger allocation area from
this trial. On the same machine, neither `-A16m` nor `-A64m` improved the N1
package-set screen. The N4 `-A16m` screen was also slower. Copying decreased,
but wall time did not. Preserve this negative result; no compiler optimization
or executable-default change is included.

## Controls and scope

- Fork: <https://github.com/vtrl/purescript>, the only configured remote.
- Branch: `perf/nursery-trial-20260910`, created from campaign
  [c2834c2f](https://github.com/vtrl/purescript/commit/c2834c2f682a317cea46bad398408368241b9fee).
- The exact lead binary was downloaded, not rebuilt or obtained from npm.
  SHA256: `d105283e23997dc02c531fcfce7036554c6d68599ee528536bfafd35375e3d00`.
  Its compiler source is baseline
  [9160ce1](https://github.com/vtrl/purescript/commit/9160ce1518b5f11f9ebe32b445019f8dbb4f435a),
  normally optimized with `-O2`, not profiled. `+RTS --info` confirms GHC 9.6.6,
  `rts_thr`, and embedded `-with-rtsopts=-N`. The development version's `DIRTY`
  suffix came from the lead build; compiler source is unchanged.
- Orb: eight vCPUs, four cores with two SMT threads each, Intel Xeon @ 2.60 GHz
  family 6/model 106, x86-64 KVM, Linux 6.1.158+, about 16 GiB physical RAM,
  14 GiB workload cgroup memory limit, no swap, no explicit CPU quota.
- Package set 60.4.0: 506 packages, 4,084 PureScript sources, 4,901 inputs;
  input-manifest SHA256:
  `2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff`.
  Setup used Spago 0.93.43 and finished before measurement. One transient
  download failure was recovered by rerunning setup; the manifest then matched.
- Every run used the same binary, `--codegen js`, explicit `-N1`, `-N4`, or
  `-N8`, and `-s`. `GHCRTS` was cleared. All output was deleted before every
  run. Cache mode was warm, meaning OS page cache, not incremental compilation.
  A separate full clean warm-up was excluded from each block. No concurrent
  builds, fetching, or other compiler timings ran on this orb.
- The full N1 screen used the original harness. Subsequent N4 and tiny runs
  used the lead's source-equivalent cherry-pick of
  [b89f5b49](https://github.com/vtrl/purescript/commit/b89f5b4998c0fe0a85d699826b79c2d0fe27eaf9),
  which checks products after every repetition, outside timing. This worker
  did not author any harness or compiler algorithm changes.

## Full-package-set screen: no candidate merits repetition

Each row has **one measured sample** and one excluded warm-up. There is no
sample variance estimate; warm-ups must not be counted as repetitions. These
are screening observations, not precise population effect estimates. Lead
steering explicitly canceled repeated N1 blocks when the screen showed no
material wall win and limited further repetition to a credible N4 candidate.

Times are seconds; RSS and sampled residency use GiB; allocation and copying
use decimal GB. Sampled residency is not peak RSS.

| Capabilities | Nursery flag | Warm-up | Measured wall | Peak RSS | Max residency | Allocated GB | Copied GB | GC elapsed |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| N1 | default | 253.57 | 248.68 | 3.1712 | 1.0864 | 397.4265 | 68.7126 | 94.235 |
| N1 | `-A16m` | 251.03 | 251.46 | 3.1332 | 1.0560 | 397.1143 | 48.3196 | 83.831 |
| N1 | `-A64m` | 299.81 | 305.97 | 3.1713 | 1.0594 | 397.0335 | 29.7766 | 97.533 |
| N4 | default | 110.27 | 107.42 | 3.3433 | 1.1396 | 397.5162 | 72.0201 | 55.480 |
| N4 | `-A16m` | 122.18 | 129.33 | 3.2553 | 1.0888 | 397.2205 | 49.4871 | 73.460 |

| Capabilities | Nursery flag | User CPU | System CPU | MUT elapsed | GC CPU | Minor collections | Major collections |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| N1 | default | 244.83 | 4.57 | 154.324 | 94.089 | 94,350 | 37 |
| N1 | `-A16m` | 247.92 | 4.26 | 167.509 | 83.749 | 23,556 | 28 |
| N1 | `-A64m` | 302.66 | 4.09 | 208.308 | 97.464 | 5,874 | 18 |
| N4 | default | 277.84 | 12.18 | 51.799 | 117.557 | 34,315 | 37 |
| N4 | `-A16m` | 333.67 | 6.83 | 55.720 | 139.180 | 6,534 | 27 |

The measured N1 `-A16m` wall difference is +1.12%, despite 29.7% less copying
and 11.0% less GC elapsed time. Its increased mutator time offsets that saving.
N1 `-A64m` is 23.04% slower with essentially unchanged RSS. Its 56.7% copying
reduction does not reduce GC elapsed time. N4 `-A16m` is 20.40% slower and has
32.4% more GC elapsed time, despite 31.3% less copying. The small differences
in total allocated bytes do not constitute a compiler allocation optimization.

The warm-ups agree with rejection of N1 `-A64m` and N4 `-A16m`. N1 `-A16m`
is near parity, not a material gain. No five-sample finalist comparison was
run. No full N4 `-A64m` or full N8 matrix was run: there was no credible
finalist and no changed-global-default recommendation to validate. Do not
infer full-package-set N8 performance from this trial.

## Tiny clean compilation: memory cost without a stable latency benefit

The workload is the existing `tests/purs/passing/Sequence.purs` copied to
`src/Main.purs`, plus the pinned Prelude 6.0.1, Effect 4.0.0, and Console 6.1.0
source packages. It exercises a recursive type-class instance and effects:
57 PureScript modules, 78 inputs, 135 deterministic products. Input-manifest
SHA256: `3736e1f30a4ab6ceb49ed90349358f81e3ef75b17a0fcbf8c3253e8efb92d4cb`.

For each N setting, block 1 ran default then `-A16m`; block 2 ran `-A16m`
then default. Each setting/block had an excluded warm-up plus ten measured
clean compilations. The tables pool the twenty measured samples per setting.
SD is sample standard deviation, descriptive only: block drift is material.

| Capabilities | Nursery flag | Wall mean ± SD (s) | Wall range (s) | Mean peak RSS (MiB) | Max peak RSS (MiB) | Allocated GB mean | Copied MB mean | GC elapsed mean (s) |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| N1 | default | 1.3585 ± 0.0655 | 1.25–1.49 | 55.13 | 56.96 | 2.3166 | 154.92 | 0.3743 |
| N1 | `-A16m` | 1.3220 ± 0.0775 | 1.22–1.52 | 66.08 | 68.64 | 2.3145 | 91.45 | 0.2728 |
| N4 | default | 0.8530 ± 0.0927 | 0.71–1.06 | 72.16 | 74.63 | 2.3198 | 154.18 | 0.3346 |
| N4 | `-A16m` | 0.8335 ± 0.0370 | 0.78–0.91 | 115.88 | 117.38 | 2.3165 | 70.39 | 0.2583 |
| N8 | default | 0.8320 ± 0.0764 | 0.74–1.00 | 89.02 | 91.70 | 2.3185 | 151.87 | 0.3294 |
| N8 | `-A16m` | 0.8320 ± 0.0276 | 0.78–0.87 | 184.00 | 185.24 | 2.3159 | 55.95 | 0.2404 |

| Capabilities | Block 1 default / A16 means (s) | Block 2 default / A16 means (s) |
| --- | --- | --- |
| N1 | 1.339 / 1.365 | 1.378 / 1.279 |
| N4 | 0.791 / 0.846 | 0.915 / 0.821 |
| N8 | 0.882 / 0.817 | 0.782 / 0.847 |

The latency difference changes sign between blocks at every N setting.
Mean RSS rises 19.9%, 60.6%, and 106.7% at N1, N4, and N8 respectively.
In particular, the executable's all-eight-capability boundary has equal
pooled mean latency but about twice the memory footprint with `-A16m`.

## Correctness and evidence

All 8,985 full-corpus product hashes match across every screen setting:
4,084 `index.js`, 817 `foreign.js`, and 4,084 `externs.cbor` files.
`products.json` SHA256:
`f79055cf88fb744534e5a3098da629c5dcc87ffad45ebb2bcaf7bef87ff9aaed`.
The original N1 harness hashed only each setting's final measured output;
it did not verify its warm-up output. All N4 and tiny repetitions were checked
individually by the updated harness.

All 135 tiny products match across all 132 clean compilations, including
warm-ups. Tiny `products.json` SHA256:
`6e3f9b11fad45ffa3655f5826ce9752c3f08dda6b0dd9526bf1937511a385da4`.
Executing generated `Main.main()` under Node 22.23.2 printed `Done`.
No algorithm or default changed, so this trial did not rerun the compiler's
whole test suite; the campaign's tests worker owns that validation.

Raw evidence archive: `nursery-trial-20260910.tar.gz`, 3,116,181 bytes,
SHA256 `d1ae99e39e05fdd689cf0cfc7f5a520ee11200b6c1014198be796d96f7a9c274`.
It was transferred to the lead's `.build/perf/` and retained as this worker's
review artifact. It contains all 17 result groups, raw GNU-time/RTS logs,
commands, binary/input/product hashes, machine metadata, warm-ups, samples,
summaries, and tiny input manifests. `nursery-results.json` additionally
extracts copying, GC CPU/elapsed time, generation counts/pauses, and residency;
`nursery-tiny-summary.json` pools both tiny blocks with variance and ranges.
Extracted wall/user/system/RSS/allocation/residency values were checked against
the harness's saved samples. Full-corpus runs total ten; tiny runs total 132.

## Reproduction and limitations

Use the campaign's corpus preparation instructions and the binary hash above.
For a screen point, with setup complete and no other timings running:

```sh
python3 ci/benchmark-compiler.py \
  --corpus .build/perf/corpus --compiler .build/perf/purs-baseline \
  --results .build/perf/nursery-screen-n4-a16 \
  --label 9160ce1-O2-nursery-screen-a16 \
  --capabilities 4 --samples 1 --rts-arg=-A16m
```

Omit `--rts-arg` for default; use `--capabilities 1` and, for its other screen
point, `--rts-arg=-A64m`. Use new result directories. Full-screen chronological
order was N1 default, A16, A64, then N4 default, A16. For tiny reproduction,
restore the archive's tiny manifests and Main source, copy the three pinned
packages from the prepared corpus under the same `.spago/p/` paths, validate
the manifest, and use `--corpus .build/perf/tiny --samples 10` in the block
order stated above.

[GHC 9.6.6's RTS documentation](https://downloads.haskell.org/ghc/9.6.6/docs/users_guide/runtime_control.html#rts-options-to-control-the-garbage-collector)
describes the relevant tradeoff: a larger nursery can reduce promotion and
collections while worsening cache behavior. The allocation area is per
capability. Raising `-A` also changes the default large-object allocation
limit; `-A16m` enables 4 MiB nursery chunks, and `-A64m` changes the default
young-generation parallel GC load-balancing setting. These are practical
flag comparisons, not an experiment isolating those mechanisms. No hardware
cache counters were collected, so cache causation is not established.

Results apply to this machine, binary, corpus, and warm clean-compilation
mode. They do not establish cold-cache, incremental, IDE, other architecture,
larger-capability-count, or full N8 behavior. Retain current nursery defaults
and discard these two candidate settings for this campaign; checkpoint the
evidence only. Any later targeted RTS tuning requires a separate measured
trial rather than inferring a win from reduced copying.
