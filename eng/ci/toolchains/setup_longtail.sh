#!/usr/bin/env bash
set -euo pipefail

ZIG_VERSION="0.16.0"
ZIG_SHA256="70e49664a74374b48b51e6f3fdfbf437f6395d42509050588bd49abe52ba3d00"
NIM_VERSION="2.2.10"

sudo apt-get update
sudo apt-get install --no-install-recommends -y gobjc libgnustep-base-dev xz-utils

zig_archive="${RUNNER_TEMP:-/tmp}/zig-${ZIG_VERSION}.tar.xz"
zig_root="${RUNNER_TEMP:-/tmp}/zig-${ZIG_VERSION}"
curl -fsSL "https://ziglang.org/download/${ZIG_VERSION}/zig-x86_64-linux-${ZIG_VERSION}.tar.xz" -o "$zig_archive"
echo "${ZIG_SHA256}  ${zig_archive}" | sha256sum -c -
rm -rf "$zig_root"
mkdir -p "$zig_root"
tar -xJf "$zig_archive" --strip-components=1 -C "$zig_root"
printf '%s\n' "$zig_root" >> "$GITHUB_PATH"
export PATH="$zig_root:$PATH"
zig version

tmp_choosenim="${RUNNER_TEMP:-/tmp}/choosenim.sh"
curl -fsSL https://nim-lang.org/choosenim/init.sh -o "$tmp_choosenim"
sh "$tmp_choosenim" -y
export PATH="$HOME/.nimble/bin:$PATH"
printf '%s\n' "$HOME/.nimble/bin" >> "$GITHUB_PATH"
choosenim "$NIM_VERSION"
nim --version
