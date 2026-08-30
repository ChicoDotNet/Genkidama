#!/usr/bin/env bash
set -euo pipefail
runner_temp="${RUNNER_TEMP:-/tmp}"
env_file="${GITHUB_ENV:-$runner_temp/genkidama-env}"
path_file="${GITHUB_PATH:-$runner_temp/genkidama-path}"
touch "$env_file" "$path_file"

bash_version=5.3
bash_archive="bash-${bash_version}.tar.gz"
bash_sha="0d5cd86965f869a26cf64f4b71be7b96f90a3ba8b3d74e27e8e9d9d5550f31ba"
curl --fail --location --retry 3 --output "$runner_temp/$bash_archive" "https://ftp.gnu.org/gnu/bash/$bash_archive"
echo "$bash_sha  $runner_temp/$bash_archive" | sha256sum --check -
tar -xzf "$runner_temp/$bash_archive" -C "$runner_temp"
bash_prefix="$runner_temp/bash-$bash_version-install"
pushd "$runner_temp/bash-$bash_version" >/dev/null
./configure --prefix="$bash_prefix" >/dev/null
make -j2 >/dev/null
make install >/dev/null
popd >/dev/null
printf 'GENKIDAMA_BASH_BIN=%s/bin/bash\n' "$bash_prefix" >> "$env_file"
printf '%s/bin\n' "$bash_prefix" >> "$path_file"

lua_version=5.5.1
lua_archive="lua-${lua_version}.tar.gz"
lua_sha="1c4b4068d67061f2a2231ad2b5422e77acea1487ea9890f6320af614f4373dce"
curl --fail --location --retry 3 --output "$runner_temp/$lua_archive" "https://www.lua.org/ftp/$lua_archive"
echo "$lua_sha  $runner_temp/$lua_archive" | sha256sum --check -
tar -xzf "$runner_temp/$lua_archive" -C "$runner_temp"
make -C "$runner_temp/lua-$lua_version" all test >/dev/null
lua_bin="$runner_temp/lua-$lua_version/src/lua"
luac_bin="$runner_temp/lua-$lua_version/src/luac"
printf 'GENKIDAMA_LUA_BIN=%s\n' "$lua_bin" >> "$env_file"
printf 'GENKIDAMA_LUAC_BIN=%s\n' "$luac_bin" >> "$env_file"
printf '%s\n' "$runner_temp/lua-$lua_version/src" >> "$path_file"
"$bash_prefix/bin/bash" --version | head -n 1
"$lua_bin" -v
