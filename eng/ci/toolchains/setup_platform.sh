#!/usr/bin/env bash
set -euo pipefail
sudo apt-get update
sudo apt-get install --no-install-recommends -y build-essential libffi-dev pkg-config unzip nasm
curl --fail --location --retry 3 'https://github.com/godotengine/godot-builds/releases/download/4.6.3-stable/Godot_v4.6.3-stable_linux.x86_64.zip' --output /tmp/godot.zip
rm -rf /tmp/godot && mkdir -p /tmp/godot
unzip -q /tmp/godot.zip -d /tmp/godot
sudo install -m 0755 /tmp/godot/Godot_v4.6.3-stable_linux.x86_64 /usr/local/bin/godot
echo 'GENKIDAMA_GODOT_BIN=/usr/local/bin/godot' >> "$GITHUB_ENV"
git clone --depth 1 --branch v1.28.0 --recurse-submodules https://github.com/micropython/micropython.git /tmp/micropython
make -C /tmp/micropython/mpy-cross -j2
make -C /tmp/micropython/ports/unix -j2
micropython_bin=/tmp/micropython/ports/unix/build-standard/micropython
"$micropython_bin" --version
echo "GENKIDAMA_MICROPYTHON_BIN=$micropython_bin" >> "$GITHUB_ENV"
curl --fail --location --retry 3 'https://github.com/RockstarLang/rockstar/releases/download/v2.0.31/rockstar-v2.0.31-linux-x64.tar.gz' --output /tmp/rockstar.tar.gz
echo '3d265c80d9d039ad92524f42951eb546d8b42c680965dfa551210457e399653f  /tmp/rockstar.tar.gz' | sha256sum -c -
rm -rf /tmp/rockstar && mkdir -p /tmp/rockstar
tar -xzf /tmp/rockstar.tar.gz -C /tmp/rockstar
rockstar="$(find /tmp/rockstar -type f -name rockstar -perm -111 | head -n 1)"
test -n "$rockstar"
"$rockstar" --version
echo "GENKIDAMA_ROCKSTAR_BIN=$rockstar" >> "$GITHUB_ENV"
