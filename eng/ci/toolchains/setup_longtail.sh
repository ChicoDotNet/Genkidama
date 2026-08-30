#!/usr/bin/env bash
set -euo pipefail
sudo apt-get update
sudo apt-get install --no-install-recommends -y gobjc libgnustep-base-dev
curl -fsSL https://nim-lang.org/choosenim/init.sh -o /tmp/choosenim.sh
sh /tmp/choosenim.sh -y
export PATH="$HOME/.nimble/bin:$PATH"
choosenim stable
printf '%s\n' "$HOME/.nimble/bin" >> "$GITHUB_PATH"
nim --version
python3 - <<'PY'
import hashlib, json, os, pathlib, re, tarfile, urllib.request
index = json.load(urllib.request.urlopen('https://ziglang.org/download/index.json'))
stable = max((v for v in index if re.fullmatch(r'\d+\.\d+\.\d+', v)), key=lambda v: tuple(map(int, v.split('.'))))
package = index[stable]['x86_64-linux']
archive = pathlib.Path(os.environ.get('RUNNER_TEMP', '/tmp')) / 'zig.tar.xz'
urllib.request.urlretrieve(package['tarball'], archive)
digest = hashlib.sha256(archive.read_bytes()).hexdigest()
if digest != package['shasum']:
    raise SystemExit(f'Zig SHA-256 mismatch: {digest} != {package["shasum"]}')
target = pathlib.Path(os.environ.get('RUNNER_TEMP', '/tmp')) / 'zig-stable'
target.mkdir(exist_ok=True)
with tarfile.open(archive, 'r:xz') as tar:
    tar.extractall(target, filter='data')
root = next(target.iterdir())
with open(os.environ['GITHUB_PATH'], 'a', encoding='utf-8') as path:
    path.write(str(root) + '\n')
print(f'Installed Zig {stable}')
PY
