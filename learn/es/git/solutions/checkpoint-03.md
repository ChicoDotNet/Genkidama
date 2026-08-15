# Solución de referencia — Checkpoint 03

Ésta es una referencia, no la única historia válida. Compara decisiones y evidencia, no sólo comandos.

## 1. Tag anotado

Desde `alpha`, con `main` sincronizada:

```text
git switch main
git fetch origin
git merge --ff-only origin/main
git tag -a v0.4.0-rc1 -m "ReleaseDesk 0.4.0 RC1"
git cat-file -t v0.4.0-rc1
git push origin v0.4.0-rc1
git ls-remote --tags origin
```

El tipo debe ser `tag` porque es anotado.

## 2. Ignore y atributos

```text
printf '\nartifacts/\n' >> .gitignore
cat > .gitattributes <<'EOF'
* text=auto
*.md text eol=lf
*.ps1 text eol=crlf
EOF
mkdir -p artifacts scripts
printf 'temporal\n' > artifacts/demo.txt
printf 'Write-Host "ReleaseDesk"\r\n' > scripts/release.ps1

git check-ignore -v artifacts/demo.txt
git check-attr text eol -- README.md scripts/release.ps1
git add .gitignore .gitattributes scripts/release.ps1
git commit -m "chore: definir política de archivos"
git push origin main
```

## 3. Non-fast-forward

Sincroniza `beta`, crea y publica un cambio:

```text
git -C ../beta pull --ff-only
printf '\nAporte beta.\n' >> ../beta/docs/plan.md
git -C ../beta add docs/plan.md
git -C ../beta commit -m "docs: agregar aporte beta"
git -C ../beta push origin main
```

En `alpha`, sin fetch previo, crea otro commit:

```text
printf '\nAporte alpha.\n' >> README.md
git add README.md
git commit -m "docs: agregar aporte alpha"
git push origin main
```

El push debe ser rechazado. Investiga:

```text
git fetch origin
git log --oneline --decorate --graph --all
git log --oneline main..origin/main
git log --oneline origin/main..main
```

Si los cambios no compiten y la historia local sigue siendo privada:

```text
git rebase origin/main
git push origin main
```

Ambos commits quedan alcanzables desde `origin/main`.

## 4. Stash nombrado

```text
printf '\nWIP: revisión pendiente.\n' >> README.md
git stash push -m "wip: revisión de README"
git status --short
git stash list
git stash show -p stash@{0}
git stash pop
```

Después decide si ese WIP merece commit o debe restaurarse. Para dejar el checkpoint limpio puedes convertirlo en un commit coherente:

```text
git add README.md
git commit -m "docs: registrar revisión pendiente"
git push origin main
```

## 5. Worktree adicional

```text
git worktree add -b feature/release-audit ../releasedesk-audit main
cat > ../releasedesk-audit/docs/release-audit.md <<'EOF'
# Auditoría de release

- Verificar diff.
- Verificar responsable.
- Verificar tag candidato.
EOF

git -C ../releasedesk-audit add docs/release-audit.md
git -C ../releasedesk-audit commit -m "docs: agregar auditoría de release"
```

Comprueba que el working tree principal sigue en `main`:

```text
git branch --show-current
git -C ../releasedesk-audit branch --show-current
git worktree list
git status --short
git -C ../releasedesk-audit status --short
```

## 6. Evidencia final

```text
git cat-file -t v0.4.0-rc1
git ls-remote --tags origin
git check-ignore -v artifacts/demo.txt
git check-attr text eol -- README.md scripts/release.ps1
git stash list
git worktree list
git log --oneline --decorate --graph --all
```

## Qué debes poder explicar

- el tag nombra un hito; `main` puede seguir avanzando;
- `.gitignore` decide sobre archivos no rastreados y `.gitattributes` define tratamiento compartido;
- el rechazo non-fast-forward evitó perder historia de `beta`;
- stash sirvió como almacenamiento temporal, no como historia permanente;
- el worktree adicional comparte el repositorio de objetos pero mantiene su propio working tree y branch activa.
