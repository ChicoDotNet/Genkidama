#!/usr/bin/env bash
set -uo pipefail

failures=()

run_group() {
  local name="$1"
  shift
  echo "::group::$name"
  if ( set -euo pipefail; "$@" ); then
    echo "$name: passed"
  else
    local rc=$?
    echo "$name: failed (exit $rc)" >&2
    failures+=("$name")
  fi
  echo "::endgroup::"
}

validate_native() {
  test "$(find src/Systems/Rust/patterns -maxdepth 1 -name '*.rs' | wc -l)" -eq 39
  test "$(find 'src/Systems/C++/patterns' -maxdepth 1 -name '*.cpp' | wc -l)" -eq 39
  test "$(find src/Systems/C/patterns -maxdepth 1 -name '*.c' | wc -l)" -eq 39
  mkdir -p .ci

  docker pull rust:latest
  docker run --rm -v "$PWD:/work" -w /work rust:latest bash -lc '
    set -euo pipefail
    rustc --version
    for f in src/Systems/Rust/patterns/*.rs; do
      { cat "$f"; printf "\nfn main(){assert!(run());}\n"; } > .ci/cell.rs
      rustc --edition=2024 -D warnings .ci/cell.rs -o .ci/cell
      ./.ci/cell
    done
  '

  docker pull gcc:latest
  docker run --rm -v "$PWD:/work" -w /work gcc:latest bash -lc '
    set -euo pipefail
    gcc --version | head -n1
    for f in src/Systems/C/patterns/*.c; do
      { cat "$f"; printf "\nint main(void){return run()?0:1;}\n"; } > .ci/cell.c
      gcc -std=c23 -Wall -Wextra -Werror .ci/cell.c -o .ci/cell
      ./.ci/cell
    done
    g++ --version | head -n1
    for f in src/Systems/C++/patterns/*.cpp; do
      { cat "$f"; printf "\nint main(){return run()?0:1;}\n"; } > .ci/cell.cpp
      g++ -std=c++23 -Wall -Wextra -Werror .ci/cell.cpp -o .ci/cell
      ./.ci/cell
    done
  '
  echo 'native-compiled: 117/117 passed'
}

validate_jvm() {
  test "$(find src/Enterprise/Java/patterns -maxdepth 1 -name '*.java' | wc -l)" -eq 39
  test "$(find src/Functional/Groovy/patterns -maxdepth 1 -name '*.groovy' | wc -l)" -eq 39
  mkdir -p .ci/java

  docker pull eclipse-temurin:25-jdk
  docker run --rm -v "$PWD:/work" -w /work eclipse-temurin:25-jdk bash -lc '
    set -euo pipefail
    java -version
    for f in src/Enterprise/Java/patterns/*.java; do
      rm -rf .ci/java/*
      cp "$f" .ci/java/PatternCell.java
      javac -Xlint:all -Werror .ci/java/PatternCell.java
      java -cp .ci/java PatternCell
    done
  '

  docker pull groovy:latest
  docker run --rm -v "$PWD:/work" -w /work groovy:latest bash -lc '
    set -euo pipefail
    groovy --version
    for f in src/Functional/Groovy/patterns/*.groovy; do groovy "$f"; done
  '
  echo 'jvm: 78/78 passed'
}

validate_functional() {
  local spec dir glob
  for spec in \
    'src/Functional/Elixir/patterns:*.exs' \
    'src/Functional/Erlang/patterns:*.erl' \
    'src/Functional/OCaml/patterns:*.ml' \
    'src/Functional/CommonLisp/patterns:*.lisp' \
    'src/Functional/Prolog/patterns:*.pl'; do
    dir=${spec%%:*}
    glob=${spec##*:}
    test "$(find "$dir" -maxdepth 1 -name "$glob" | wc -l)" -eq 39
  done
  mkdir -p .ci/erlang

  docker pull elixir:latest
  docker run --rm -v "$PWD:/work" -w /work elixir:latest bash -lc '
    set -euo pipefail
    elixir --version
    for f in src/Functional/Elixir/patterns/*.exs; do elixir "$f"; done
    for f in src/Functional/Erlang/patterns/*.erl; do
      mod=$(basename "$f" .erl)
      rm -f .ci/erlang/*.beam
      erlc -Werror -o .ci/erlang "$f"
      erl -noshell -pa /work/.ci/erlang -eval "$mod:main(), halt()."
    done
  '

  docker pull ocaml/opam:latest
  docker run --rm -v "$PWD:/work" -w /work ocaml/opam:latest sh -lc '
    set -eu
    ocamlc -version
    for f in src/Functional/OCaml/patterns/*.ml; do
      ocamlc -w +a -warn-error +a "$f" -o /tmp/cell
      /tmp/cell
    done
  '

  docker pull clfoundation/sbcl:latest
  docker run --rm -v "$PWD:/work" -w /work clfoundation/sbcl:latest sh -lc '
    set -eu
    sbcl --version
    for f in src/Functional/CommonLisp/patterns/*.lisp; do sbcl --script "$f"; done
  '

  docker pull swipl:latest
  docker run --rm -v "$PWD:/work" -w /work swipl:latest sh -lc '
    set -eu
    swipl --version
    for f in src/Functional/Prolog/patterns/*.pl; do swipl -q -f "$f"; done
  '
  echo 'functional-runtimes: 195/195 passed'
}

validate_data_shell() {
  test "$(find src/DataScience/R/patterns -maxdepth 1 -name '*.R' | wc -l)" -eq 39
  test "$(find src/DataScience/Octave/patterns -maxdepth 1 -name '*.m' | wc -l)" -eq 39
  test "$(find src/Scripting/PowerShell/patterns -maxdepth 1 -name '*.ps1' | wc -l)" -eq 39

  docker pull r-base:latest
  docker run --rm -v "$PWD:/work" -w /work r-base:latest bash -lc '
    set -euo pipefail
    R --version | head -n1
    for f in src/DataScience/R/patterns/*.R; do Rscript "$f"; done
  '

  docker pull gnuoctave/octave:latest
  docker run --rm -v "$PWD:/work" -w /work gnuoctave/octave:latest bash -lc '
    set -euo pipefail
    octave --version | head -n1
    for f in src/DataScience/Octave/patterns/*.m; do
      name=$(basename "$f" .m)
      octave --no-gui --quiet --eval "addpath(\"/work/src/DataScience/Octave/patterns\"); $name"
    done
  '

  docker pull mcr.microsoft.com/powershell:latest-ubuntu-24.04
  docker run --rm -v "$PWD:/work" -w /work mcr.microsoft.com/powershell:latest-ubuntu-24.04 pwsh -NoLogo -NoProfile -Command '
    $ErrorActionPreference = "Stop"
    $PSVersionTable.PSVersion
    Get-ChildItem /work/src/Scripting/PowerShell/patterns/*.ps1 | Sort-Object Name | ForEach-Object { & $_.FullName }
  '
  echo 'data-and-shell: 117/117 passed'
}

run_group native-compiled validate_native
run_group jvm validate_jvm
run_group functional-runtimes validate_functional
run_group data-and-shell validate_data_shell

if ((${#failures[@]})); then
  printf 'portable-functional cohort failed groups: %s\n' "${failures[*]}" >&2
  exit 1
fi

echo 'portable-functional cohort: 507/507 passed'
