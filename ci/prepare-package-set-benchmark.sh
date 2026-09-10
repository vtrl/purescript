#!/usr/bin/env bash
# Like build-package-set.sh, but fetch a fixed set without timing setup/build tools.
set -euo pipefail

root=$(realpath "${1:-.build/package-set-benchmark}")
mkdir -p "$root"
cd "$root"

registry=5d834cd364da1d49a1bd1b0219ab49fb15f20601
package_set=60.4.0
curl -fLsS --retry 3 \
  "https://raw.githubusercontent.com/purescript/registry/$registry/package-sets/$package_set.json" \
  -o package-set.json
echo '8cab74af472f4f4e142d2ff04e630a2aaf604b7af40987aacab0ae704fdadbb0  package-set.json' | sha256sum -c -

# Use CI's Spago version. purs must be available for Spago's version check;
# Spago never invokes compilation here. Tool installation is not benchmarked.
[[ $(spago --version) == 0.93.43 ]]
python3 - <<'PY'
import json
from pathlib import Path

package_set = json.loads(Path('package-set.json').read_text())
config = {
    'package': {'name': 'compiler-benchmark', 'dependencies': sorted(package_set['packages'])},
    'workspace': {'packageSet': {'registry': package_set['version']}, 'extraPackages': {}},
}
Path('spago.yaml').write_text(json.dumps(config, indent=2) + '\n')
PY
spago fetch
spago sources --json > sources.json

python3 - <<'PY'
import glob
import hashlib
import json
from pathlib import Path

patterns = json.loads(Path('sources.json').read_text())
sources = sorted({name for pattern in patterns for name in glob.glob(pattern, recursive=True)})
if not sources:
    raise SystemExit('No sources fetched')
# Include foreign implementations in the reproducibility fingerprint.
inputs = sorted(set(sources) | {str(Path(p).with_suffix('.js')) for p in sources if Path(p).with_suffix('.js').is_file()})
manifest = {p: hashlib.sha256(Path(p).read_bytes()).hexdigest() for p in inputs}
Path('inputs.json').write_text(json.dumps(manifest, sort_keys=True, indent=2) + '\n')
Path('purs-files.json').write_text(json.dumps(sources, indent=2) + '\n')
print(f'{len(sources)} PureScript sources; {len(inputs)} total inputs')
print('Input manifest SHA256:', hashlib.sha256(Path('inputs.json').read_bytes()).hexdigest())
PY
echo '2d7ae344edad25ae4c41af1cbc0e6d1493b4e6e8167f7365481bb738602267ff  inputs.json' | sha256sum -c -
