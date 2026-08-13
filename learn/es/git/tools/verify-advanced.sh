#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
workspace="$(mktemp -d)"
trap 'rm -rf "$workspace"' EXIT
lab="$workspace/advanced"
mkdir -p "$lab"
cp -R "$repo_root/learn/es/git/app/." "$lab/"
cd "$lab"

git init -b main
git config user.name "Genkidama Advanced"
git config user.email "advanced@example.invalid"
git add README.md CHANGELOG.md .gitignore docs/plan.md
git commit -m "chore: iniciar laboratorio avanzado"

# Lesson 13: history reduction, blame provenance and deterministic bisect.
git switch -c diagnostic/bisect-demo main
printf 'status=healthy\n' > docs/health.txt
git add docs/health.txt
git commit -m "test: establecer salud conocida"
good_sha="$(git rev-parse HEAD)"
printf 'note=preparacion\n' >> docs/health.txt
git add docs/health.txt
git commit -m "docs: preparar diagnostico"
printf 'status=broken\nnote=preparacion\n' > docs/health.txt
git add docs/health.txt
git commit -m "test: introducir regresion controlada"
bad_intro_sha="$(git rev-parse HEAD)"
printf 'note=seguimiento-1\n' >> docs/health.txt
git add docs/health.txt
git commit -m "docs: agregar seguimiento uno"
printf 'note=seguimiento-2\n' >> docs/health.txt
git add docs/health.txt
git commit -m "docs: agregar seguimiento dos"

git log --oneline -- docs/health.txt > "$workspace/health-log.txt"
grep -q 'regresion controlada' "$workspace/health-log.txt"
git blame docs/health.txt > "$workspace/health-blame.txt"
grep -q 'status=broken' "$workspace/health-blame.txt"

git bisect start
git bisect bad HEAD
git bisect good "$good_sha"
git bisect run sh -c 'grep -q "^status=healthy$" docs/health.txt' > "$workspace/bisect.out"
first_bad="$(git rev-parse refs/bisect/bad)"
test "$first_bad" = "$bad_intro_sha"
git bisect reset

git switch main
git branch -D diagnostic/bisect-demo

# Lesson 14: recover a deleted branch from local reflog evidence.
git switch -c recovery/reflog-demo main
cat > docs/reflog-note.md <<'EOF'
# Nota recuperable

Este commit existe para demostrar reflog.
EOF
git add docs/reflog-note.md
git commit -m "docs: agregar nota recuperable"
lost_sha="$(git rev-parse HEAD)"
git switch main
git branch -D recovery/reflog-demo
if git show-ref --verify --quiet refs/heads/recovery/reflog-demo; then
  echo "Expected deleted recovery branch to be absent" >&2
  exit 1
fi
recovered_sha="$(git reflog --all --grep-reflog='docs: agregar nota recuperable' --format='%H' -n 1)"
test -n "$recovered_sha"
test "$recovered_sha" = "$lost_sha"
git branch recovered/reflog-demo "$recovered_sha"
git show recovered/reflog-demo:docs/reflog-note.md > "$workspace/reflog-note.txt"
grep -q 'demostrar reflog' "$workspace/reflog-note.txt"
git branch -D recovered/reflog-demo

# Lesson 15: local hook for fast feedback, without pretending it is shared governance.
git switch -c policy/hooks-demo main
mkdir -p .githooks
cat > .githooks/pre-commit <<'EOF'
#!/usr/bin/env sh
set -eu

git diff --cached --check

if git diff --cached --unified=0 | grep -q '^+.*FORBIDDEN-WIP'; then
  echo 'Commit rechazado: elimina FORBIDDEN-WIP del contenido staged.' >&2
  exit 1
fi
EOF
chmod +x .githooks/pre-commit
git add .githooks/pre-commit
git commit -m "chore: agregar hook de ejemplo"
git config core.hooksPath .githooks
printf '\nFORBIDDEN-WIP\n' >> docs/plan.md
git add docs/plan.md
if git commit -m "docs: commit que debe fallar" > "$workspace/hook.out" 2>&1; then
  echo "Expected pre-commit hook rejection" >&2
  exit 1
fi
git restore --staged docs/plan.md
git restore docs/plan.md
printf '\n- Validar checklist compartido.\n' >> docs/plan.md
git add docs/plan.md
git commit -m "docs: satisfacer hook local"
git config --unset core.hooksPath

git switch main
git branch -D policy/hooks-demo

# Lesson 16: use only a clearly fake secret and prove deletion is not history erasure.
git switch -c security/secret-demo main
cat > demo-secret.txt <<'EOF'
DEMO_TOKEN=example-only-not-a-secret
EOF
git add demo-secret.txt
git commit -m "test: agregar secreto falso controlado"
secret_sha="$(git rev-parse HEAD)"
git rm demo-secret.txt
git commit -m "test: retirar secreto falso del working tree"
test ! -e demo-secret.txt
git show "$secret_sha:demo-secret.txt" > "$workspace/secret-history.txt"
grep -q 'DEMO_TOKEN=example-only-not-a-secret' "$workspace/secret-history.txt"
git log --show-signature -1 > "$workspace/signature-view.txt"

git switch main
git branch -D security/secret-demo

test -z "$(git status --porcelain)"

# Course DoD: once course.yml says complete, the autonomous final assessment package must exist and be connected.
for required in \
  learn/es/git/lessons/17-evaluacion-final.md \
  learn/es/git/exercises/evaluacion-final.md \
  learn/es/git/exercises/rubrica-final.md \
  learn/es/git/solutions/evaluacion-final.md; do
  test -f "$repo_root/$required"
done

lesson_count="$(find "$repo_root/learn/es/git/lessons" -maxdepth 1 -type f -name '*.md' | wc -l | tr -d ' ')"
test "$lesson_count" -eq 17
grep -q '^status: complete$' "$repo_root/learn/es/git/course.yml"
grep -q '../exercises/evaluacion-final.md' "$repo_root/learn/es/git/lessons/17-evaluacion-final.md"
grep -q '../exercises/rubrica-final.md' "$repo_root/learn/es/git/lessons/17-evaluacion-final.md"
grep -q '../solutions/evaluacion-final.md' "$repo_root/learn/es/git/lessons/17-evaluacion-final.md"

printf 'Advanced Git lessons 13-16 and final Course DoD package passed.\n'
