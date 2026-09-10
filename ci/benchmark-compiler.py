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


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--compiler', type=Path, required=True)
    parser.add_argument('--results', type=Path, required=True, help='New directory; never overwrites results')
    parser.add_argument('--label', required=True, help='Compiler commit and build identity')
    parser.add_argument('--capabilities', type=int, default=1)
    parser.add_argument('--samples', type=int, default=5)
    parser.add_argument('--cache', choices=['warm', 'cold'], default='warm')
    parser.add_argument('--codegen', default='js')
    args = parser.parse_args()
    if args.samples < 1 or args.capabilities < 1:
        parser.error('samples and capabilities must be positive')
    corpus = args.corpus.resolve()
    compiler = args.compiler.resolve(strict=True)
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
                   '--codegen', args.codegen, '+RTS', f'-N{args.capabilities}', '-s', '-RTS']
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
        (results / 'metadata.json').write_text(json.dumps(metadata, indent=2) + '\n')
        rows = []
        # Warm-up is a full clean compile and is explicitly excluded from samples.
        runs = ['warmup', *map(str, range(1, args.samples + 1))] if args.cache == 'warm' else list(map(str, range(1, args.samples + 1)))
        for run in runs:
            shutil.rmtree(output, ignore_errors=True)
            if args.cache == 'cold':
                # Only use in an otherwise idle, disposable benchmark machine.
                # Fail rather than silently labelling a warm run as cold.
                subprocess.run(['sudo', '-n', 'sh', '-c', 'sync; echo 3 > /proc/sys/vm/drop_caches'], check=True)
            stats = results / f'{run}.time'
            with (results / f'{run}.stdout').open('w') as stdout, (results / f'{run}.stderr').open('w') as stderr:
                started = time.perf_counter()
                subprocess.run(['/usr/bin/time', '-f', '%e %U %S %M', '-o', str(stats), *command],
                               cwd=corpus, env=env, stdout=stdout, stderr=stderr, check=True)
                elapsed = time.perf_counter() - started
            wall, user, system, rss = map(float, stats.read_text().split())
            stderr = (results / f'{run}.stderr').read_text()
            allocations = re.search(r'([\d,]+) bytes allocated in the heap', stderr)
            residency = re.search(r'([\d,]+) bytes maximum residency', stderr)
            if not allocations or not residency:
                raise SystemExit('Missing RTS statistics')
            row = {'run': run, 'wall_s': wall, 'monotonic_s': elapsed, 'user_s': user, 'system_s': system,
                   'peak_rss_kib': int(rss), 'allocated_bytes': int(allocations[1].replace(',', '')),
                   'max_residency_bytes': int(residency[1].replace(',', ''))}
            print(json.dumps(row), flush=True)
            if run != 'warmup':
                rows.append(row)
            (results / 'samples.json').write_text(json.dumps(rows, indent=2) + '\n')
        summary = {}
        for field in ['wall_s', 'peak_rss_kib', 'allocated_bytes', 'max_residency_bytes']:
            values = [row[field] for row in rows]
            summary[field] = {'mean': statistics.mean(values), 'median': statistics.median(values),
                              'stdev': statistics.stdev(values) if len(values) > 1 else None,
                              'min': min(values), 'max': max(values)}
        (results / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
        # Compare only deterministic compiler products, not timestamp/cache metadata.
        products = {str(path.relative_to(output)): sha256(path) for path in sorted(output.rglob('*'))
                    if path.is_file() and path.name in ['index.js', 'foreign.js', 'externs.cbor', 'corefn.json']}
        (results / 'products.json').write_text(json.dumps(products, indent=2) + '\n')
        print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
