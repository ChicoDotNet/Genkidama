#!/usr/bin/env bash
set -uo pipefail

failures=()

run_group() {
  local name="$1"
  shift
  local rc

  echo "::group::$name"
  # Do not invoke the validator from an `if` condition. Bash disables errexit
  # inside functions whose status is being tested, which can turn real runtime
  # failures into false positives. Run it as a plain command in a subshell and
  # capture the status afterwards instead.
  ( set -euo pipefail; "$@" )
  rc=$?

  if ((rc == 0)); then
    echo "$name: passed"
  else
    echo "$name: failed (exit $rc)" >&2
    failures+=("$name")
  fi
  echo "::endgroup::"
}

validate_native() {
  test "$(find src/Systems/Rust/patterns -maxdepth 1 -name '*.rs' | wc -l)" -eq 39
  test "$(find 'src/Systems/C++/patterns' -maxdepth 1 -name '*.cpp' | wc -l)" -eq 39
  test "$(find src/Systems/C/patterns -maxdepth 1 -name '*.c' | wc -l)" -eq 39

  docker pull rust:latest
  docker run --rm -v "$PWD:/work:ro" -w /work rust:latest bash -c '
    set -euo pipefail
    rustc --version
    work=$(mktemp -d)
    for f in src/Systems/Rust/patterns/*.rs; do
      echo "validating $f"
      { cat "$f"; printf "\nfn main(){assert!(run());}\n"; } > "$work/cell.rs"
      rustc --edition=2024 -D warnings "$work/cell.rs" -o "$work/cell"
      "$work/cell"
    done
  '

  docker pull gcc:latest
  docker run --rm -v "$PWD:/work:ro" -w /work gcc:latest bash -c '
    set -euo pipefail
    work=$(mktemp -d)

    gcc --version | head -n1
    for f in src/Systems/C/patterns/*.c; do
      echo "validating $f"
      { cat "$f"; printf "\nint main(void){return run()?0:1;}\n"; } > "$work/cell.c"
      gcc -std=c23 -Wall -Wextra -Werror "$work/cell.c" -o "$work/cell"
      "$work/cell"
    done

    g++ --version | head -n1
    for f in src/Systems/C++/patterns/*.cpp; do
      echo "validating $f"
      { cat "$f"; printf "\nint main(){return run()?0:1;}\n"; } > "$work/cell.cpp"
      g++ -std=c++23 -Wall -Wextra -Werror "$work/cell.cpp" -o "$work/cell"
      "$work/cell"
    done
  '

  echo 'native-compiled: 117/117 passed'
}

validate_jvm() {
  test "$(find src/Enterprise/Java/patterns -maxdepth 1 -name '*.java' | wc -l)" -eq 39
  test "$(find src/Functional/Groovy/patterns -maxdepth 1 -name '*.groovy' | wc -l)" -eq 39

  docker pull eclipse-temurin:25-jdk
  docker run --rm -v "$PWD:/work:ro" -w /work eclipse-temurin:25-jdk bash -c '
    set -euo pipefail
    java -version
    work=$(mktemp -d)
    for f in src/Enterprise/Java/patterns/*.java; do
      echo "validating $f"
      rm -rf "$work"/*
      cp "$f" "$work/PatternCell.java"
      javac -Xlint:all -Werror -d "$work" "$work/PatternCell.java"
      java -cp "$work" PatternCell
    done
  '

  docker pull groovy:latest
  docker run --rm -v "$PWD:/work:ro" -w /work groovy:latest bash -c '
    set -euo pipefail
    groovy --version
    for f in src/Functional/Groovy/patterns/*.groovy; do
      echo "validating $f"
      groovy "$f"
    done
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

  docker pull elixir:latest
  docker run --rm -v "$PWD:/work:ro" -w /work elixir:latest bash -c '
    set -euo pipefail
    elixir --version
    erlang_out=$(mktemp -d)

    for f in src/Functional/Elixir/patterns/*.exs; do
      echo "validating $f"
      elixir "$f"
    done

    for f in src/Functional/Erlang/patterns/*.erl; do
      echo "validating $f"
      mod=$(basename "$f" .erl)
      rm -f "$erlang_out"/*.beam
      erlc -Werror -o "$erlang_out" "$f"
      erl -noshell -pa "$erlang_out" -eval "$mod:main(), halt()."
    done
  '

  docker pull ocaml/opam:latest
  docker run --rm -v "$PWD:/work:ro" -w /work ocaml/opam:latest sh -c '
    set -eu
    ocamlc -version
    work=$(mktemp -d)
    for f in src/Functional/OCaml/patterns/*.ml; do
      echo "validating $f"
      cp "$f" "$work/cell.ml"
      (
        cd "$work"
        ocamlc -w +a-70 -warn-error +a-70 cell.ml -o cell
        ./cell
      )
    done
  '

  docker pull clfoundation/sbcl:latest
  docker run --rm -v "$PWD:/work:ro" -w /work clfoundation/sbcl:latest sh -c '
    set -eu
    sbcl --version
    for f in src/Functional/CommonLisp/patterns/*.lisp; do
      echo "validating $f"
      sbcl --script "$f"
    done
  '

  docker pull swipl:latest
  docker run --rm -v "$PWD:/work:ro" -w /work swipl:latest sh -c '
    set -eu
    swipl --version
    for f in src/Functional/Prolog/patterns/*.pl; do
      echo "validating $f"
      swipl -q -f "$f"
    done
  '

  echo 'functional-runtimes: 195/195 passed'
}

validate_data_shell() {
  test "$(find src/DataScience/R/patterns -maxdepth 1 -name '*.R' | wc -l)" -eq 39
  test "$(find src/DataScience/Octave/patterns -maxdepth 1 -name '*.m' | wc -l)" -eq 39
  test "$(find src/Scripting/PowerShell/patterns -maxdepth 1 -name '*.ps1' | wc -l)" -eq 39

  docker pull r-base:latest
  docker run --rm -v "$PWD:/work:ro" -w /work r-base:latest bash -c '
    set -euo pipefail
    R --version | head -n1
    for f in src/DataScience/R/patterns/*.R; do
      echo "validating $f"
      Rscript "$f"
    done
  '

  docker pull gnuoctave/octave:latest
  docker run --rm -v "$PWD:/work:ro" -w /work gnuoctave/octave:latest bash -c '
    set -euo pipefail
    octave --version | head -n1
    for f in src/DataScience/Octave/patterns/*.m; do
      echo "validating $f"
      name=$(basename "$f" .m)
      octave --no-gui --quiet --eval "addpath(\"/work/src/DataScience/Octave/patterns\"); $name"
    done
  '

  # Microsoft now recommends the .NET SDK image for supported PowerShell
  # container scenarios; the former powershell:* Ubuntu tag used here no longer
  # exists in MCR.
  docker pull mcr.microsoft.com/dotnet/sdk:9.0
  docker run --rm -v "$PWD:/work:ro" -w /work mcr.microsoft.com/dotnet/sdk:9.0 pwsh -NoLogo -NoProfile -Command '
    $ErrorActionPreference = "Stop"
    $PSVersionTable.PSVersion
    Get-ChildItem /work/src/Scripting/PowerShell/patterns/*.ps1 |
      Sort-Object Name |
      ForEach-Object {
        Write-Host "validating $($_.FullName)"
        & $_.FullName
      }
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
