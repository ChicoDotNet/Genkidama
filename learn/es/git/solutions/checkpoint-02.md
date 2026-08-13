# Solución de referencia — Checkpoint 02

Esta solución muestra **una** secuencia válida. No la uses como receta antes de intentar el checkpoint.

## Preparar remoto y clones

Suponiendo que ya tienes un `origin` bare con ReleaseDesk publicado, crea o reutiliza dos clones independientes: `alpha` y `beta`. Configura identidad **local** en ambos.

## Parte A — fetch antes de integrar

En `beta` modifica `docs/plan.md`, registra y publica:

```text
git add docs/plan.md
git commit -m "docs: ampliar criterios de entrega"
git push origin main
```

En `alpha`:

```text
git fetch origin
git log --oneline --decorate --graph --all
git branch -vv
```

Aquí `origin/main` debe estar delante de `main`. Como `alpha/main` no tiene commits competidores:

```text
git merge --ff-only origin/main
```

## Parte B — conflicto

Con ambos clones sincronizados, en `alpha` cambia una línea existente de `CHANGELOG.md` y registra localmente:

```text
git add CHANGELOG.md
git commit -m "docs: ajustar nota de entrega desde alpha"
```

En `beta`, cambia la misma línea con otra intención válida, registra y publica:

```text
git add CHANGELOG.md
git commit -m "docs: ajustar nota de entrega desde beta"
git push origin main
```

En `alpha`:

```text
git fetch origin
git log --oneline --decorate --graph --all
git merge origin/main
```

El merge debe detenerse. Antes de resolver:

```text
git status
git diff
```

Edita `CHANGELOG.md` para producir el contenido final correcto, prepara y revisa:

```text
git add CHANGELOG.md
git diff --staged
git commit -m "merge: reconciliar notas de entrega"
git push origin main
```

La decisión importante no es el mensaje exacto del commit, sino que puedas explicar qué intención conservaste y por qué.

## Parte C — feature privada + rebase

En `alpha`:

```text
git switch main
git fetch origin
git merge --ff-only origin/main
git switch -c feature/checkpoint-02
```

Crea `docs/release-owner.md`, registra el cambio y no publiques todavía:

```text
git add docs/release-owner.md
git commit -m "docs: registrar responsable de entrega"
```

En `beta`, sincroniza `main`, modifica `README.md`, registra y publica:

```text
git switch main
git pull --ff-only
git add README.md
git commit -m "docs: aclarar propósito de ReleaseDesk"
git push origin main
```

Regresa a `alpha`, todavía en la feature:

```text
git fetch origin
git log --oneline --decorate --graph --all
git rebase origin/main
```

Como la feature era privada y su cambio no compite con el cambio de `README.md`, el rebase debe reaplicar el commit sobre la base nueva.

Comprueba:

```text
git log --oneline origin/main..HEAD
git diff --check origin/main...HEAD
git diff --stat origin/main...HEAD
```

Finalmente publica la branch por primera vez:

```text
git push -u origin feature/checkpoint-02
```

## Qué debes poder explicar

- `fetch` actualiza tu observación del remoto pero no integra automáticamente en `main`.
- el conflicto se resolvió usando intención + evidencia, no eligiendo un lado a ciegas;
- el merge de `main` preserva explícitamente la reconciliación de dos historias ya existentes;
- el rebase fue razonable porque la feature todavía era privada y controlada por ti;
- si otra persona dependiera de sus commits anteriores, reescribirlos podría romper su referencia de historia y exigir coordinación explícita.
