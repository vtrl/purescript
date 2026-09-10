#!/usr/bin/env python3
"""Time clean package-set compilations, never dependency fetching or Stack builds."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import statistics
import subprocess
import time


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def summarize(rows):
    summary = {}
    for field in ['wall_s', 'peak_rss_kib', 'allocated_bytes', 'max_residency_bytes']:
        values = [row[field] for row in rows]
        summary[field] = {'mean': statistics.mean(values), 'median': statistics.median(values),
                          'stdev': statistics.stdev(values) if len(values) > 1 else None,
                          'min': min(values), 'max': max(values)}
    return summary


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--compiler', type=Path, required=True)
    parser.add_argument('--baseline', type=Path, help='Alternate baseline/candidate pairs, reversing order each pair')
    parser.add_argument('--results', type=Path, required=True, help='New directory; never overwrites results')
    parser.add_argument('--label', required=True, help='Compiler commit and build identity')
    parser.add_argument('--capabilities', type=int, default=1)
    parser.add_argument('--samples', type=int, default=5)
    parser.add_argument('--cache', choices=['warm', 'cold'], default='warm')
    parser.add_argument('--codegen', default='js')
    parser.add_argument('--rts-arg', action='append', default=[], help='Extra RTS flag, e.g. --rts-arg=-A16m')
    args = parser.parse_args()
    if args.samples < 1 or args.capabilities < 1:
        parser.error('samples and capabilities must be positive')
    corpus = args.corpus.resolve()
    compiler = args.compiler.resolve(strict=True)
    baseline = args.baseline.resolve(strict=True) if args.baseline else None
    results = args.results.resolve()
    results.mkdir(parents=True, exist_ok=False)
    sources = json.loads((corpus / 'purs-files.json').read_text())
    manifest = json.loads((corpus / 'inputs.json').read_text())
    for name, expected in manifest.items():
        if sha256(corpus / name) != expected:
            raise SystemExit(f'Corpus changed: {name}')

    # One stable path for warm-up, repeated runs, and candidate/baseline pairs.
    # A lock prevents accidental simultaneous benchmarks on this corpus.
    import fcntl
    with (corpus / '.benchmark.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        output = corpus / 'benchmark-output'
        command = [str(compiler), 'compile', *sources, '--output', str(output),
                   '--codegen', args.codegen, '+RTS', f'-N{args.capabilities}', *args.rts_arg, '-s', '-RTS']
        env = dict(os.environ, LC_ALL='C', LANG='C', GHCRTS='')
        metadata = {
            'label': args.label, 'compiler': str(compiler), 'compiler_sha256': sha256(compiler),
            'version': subprocess.check_output([str(compiler), '--version'], env=env, text=True).strip(),
            'manifest_sha256': sha256(corpus / 'inputs.json'), 'source_count': len(sources),
            'package_set': json.loads((corpus / 'package-set.json').read_text()),
            'command': command, 'cache': args.cache, 'samples': args.samples,
            'capabilities': args.capabilities, 'codegen': args.codegen,
            'machine': platform.uname()._asdict(), 'cpu_count': os.cpu_count(),
            'cpu_info': Path('/proc/cpuinfo').read_text(), 'memory_info': Path('/proc/meminfo').read_text(),
            'timestamp_utc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
        }
        if baseline:
            metadata['baseline'] = {
                'compiler': str(baseline), 'compiler_sha256': sha256(baseline),
                'version': subprocess.check_output([str(baseline), '--version'], env=env, text=True).strip(),
            }
            metadata['order'] = 'baseline/candidate on odd pairs; candidate/baseline on even pairs'
        (results / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
        rows = []
        first_products = None
        # Warm-up is a full clean compile and is explicitly excluded from samples.
        variants = [('baseline', baseline), ('candidate', compiler)] if baseline else [(None, compiler)]
        runs = [(f'warmup-{name}' if name else 'warmup', name, binary) for name, binary in variants] if args.cache == 'warm' else []
        for sample in range(1, args.samples + 1):
            order = variants if sample % 2 else list(reversed(variants))
            runs.extend((f'{name}-{sample}' if name else str(sample), name, binary) for name, binary in order)
        for run, variant, binary in runs:
            shutil.rmtree(output, ignore_errors=True)
            if args.cache == 'cold':
                # Only use in an otherwise idle, disposable benchmark machine.
                # Fail rather than silently labelling a warm run as cold.
                subprocess.run(['sudo', '-n', 'sh', '-c', 'sync; echo 3 > /proc/sys/vm/drop_caches'], check=True)
            stats = results / f'{run}.time'
            run_command = [str(binary), *command[1:]]
            with (results / f'{run}.stdout').open('w') as stdout, (results / f'{run}.stderr').open('w') as stderr:
                started = time.perf_counter()
                subprocess.run(['/usr/bin/time', '-f', '%e %U %S %M', '-o', str(stats), *run_command],
                               cwd=corpus, env=env, stdout=stdout, stderr=stderr, check=True)
                elapsed = time.perf_counter() - started
            wall, user, system, rss = map(float, stats.read_text().split())
            stderr = (results / f'{run}.stderr').read_text()
            allocations = re.search(r'([\d,]+) bytes allocated in the heap', stderr)
            residency = re.search(r'([\d,]+) bytes maximum residency', stderr)
            if not allocations or not residency:
                raise SystemExit('Missing RTS statistics')
            # Hash outside the timed process. Detect nondeterminism across runs,
            # not just differences between the final baseline/candidate outputs.
            products = {str(path.relative_to(output)): sha256(path) for path in sorted(output.rglob('*'))
                        if path.is_file() and path.name in ['index.js', 'foreign.js', 'externs.cbor', 'corefn.json']}
            if first_products is None:
                first_products = products
                (results / 'products.json').write_text(json.dumps(products, indent=2) + '\n')
            elif products != first_products:
                (results / f'{run}.products.json').write_text(json.dumps(products, indent=2) + '\n')
                raise SystemExit(f'Compiler products changed between repetitions: {run}')
            row = {'run': run, 'wall_s': wall, 'monotonic_s': elapsed, 'user_s': user, 'system_s': system,
                   'peak_rss_kib': int(rss), 'allocated_bytes': int(allocations[1].replace(',', '')),
                   'max_residency_bytes': int(residency[1].replace(',', ''))}
            if variant:
                row['compiler'] = variant
            print(json.dumps(row), flush=True)
            if not run.startswith('warmup'):
                rows.append(row)
            (results / 'samples.json').write_text(json.dumps(rows, indent=2) + '\n')
        if baseline:
            groups = {name: [row for row in rows if row['compiler'] == name] for name, _ in variants}
            summary = {name: summarize(group) for name, group in groups.items()}
            changes = [{field: 100 * (candidate[field] / base[field] - 1) for field in summary['baseline']}
                       for base, candidate in zip(groups['baseline'], groups['candidate'])]
            summary['paired_change_percent'] = summarize(changes)
            (results / 'paired-changes.json').write_text(json.dumps(changes, indent=2) + '\n')
        else:
            summary = summarize(rows)
        (results / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
