# Lección 11 — Sincronización colaborativa segura

## Qué vas a conseguir

Provocarás un rechazo `non-fast-forward`, demostrarás por qué protege historia remota y actualizarás tu trabajo mediante `fetch` + inspección + integración consciente antes de volver a publicar.

## Antes de empezar

Completa la [Lección 10](10-ignore-attributes-y-finales-de-linea.md). Conserva `alpha`, `beta` y el remoto bare de las lecciones anteriores.

## El problema

Dos personas parten del mismo `origin/main`. Ambas hacen commits. Una publica primero. Cuando la segunda intenta `push`, su historia ya no contiene la punta remota.

Forzar el push para “quitar el error” puede borrar historia ajena. El rechazo es una señal para investigar, no un obstáculo que Git te obliga a saltar.

## Concepto

Un push normal de una branch suele exigir que la referencia remota pueda avanzar sin perder commits alcanzables. Si el remoto avanzó por otro camino, Git rechaza una actualización `non-fast-forward`.

El flujo seguro inicial es:

```text
git fetch origin
git log --oneline --decorate --graph --all
git diff origin/main...HEAD
```

Después decides cómo integrar según la política del equipo: fast-forward cuando sea posible, merge para preservar topología o rebase si tu historia sigue siendo privada y esa práctica está acordada.

## Demostración

Desde `beta`, sincroniza, registra un cambio y publícalo:

```text
git pull --ff-only
# editar un archivo distinto
git add .
git commit -m "docs: actualizar criterio remoto"
git push origin main
```

Sin haber hecho fetch en `alpha`, crea otro commit local y prueba:

```text
git push origin main
```

Debe rechazarse. No uses `--force`.

## Código real

Investiga primero:

```text
git fetch origin
git log --oneline --decorate --graph --all
git log --oneline main..origin/main
git log --oneline origin/main..main
```

Si tu commit local es privado y no compite en contenido, puedes actualizarlo con:

```text
git rebase origin/main
git diff --check origin/main...HEAD
git push origin main
```

En otro equipo podría corresponder merge. La clave es elegir después de observar.

## `pull` no es magia

`git pull` combina obtención remota e integración. Para aprender y diagnosticar preferimos separar los pasos cuando hay riesgo:

```text
git fetch origin
# inspeccionar
git merge --ff-only origin/main
```

Cuando sabes qué política quieres, `pull --ff-only` puede ser una forma segura de expresar que sólo aceptarás un avance lineal.

## Errores comunes

- Responder a cualquier rechazo con `git push --force`.
- Hacer `pull` sin entender si hará merge, rebase o fallará según configuración.
- Confundir `origin/main` con la branch remota “en vivo”; es una referencia local actualizada por fetch.
- Integrar mientras el working tree contiene cambios que todavía no entiendes.

## Buenas prácticas

Antes de publicar:

```text
git status --short
git fetch origin
git log --oneline --decorate --graph --all
git diff --check origin/main...HEAD
```

Si alguna vez una política exige actualizar una branch remota reescrita, estudia `--force-with-lease` y el contrato del equipo antes de actuar. No lo uses como sustituto de comprensión.

## Tu turno

Provoca un rechazo non-fast-forward entre tus dos clones sin tocar el mismo archivo. Captura la evidencia del rechazo, sincroniza conscientemente la segunda historia y publica sin perder ninguno de los commits.

## Cómo comprobar

```text
git fetch origin
git merge-base --is-ancestor origin/main HEAD
git status --short
git log --oneline --decorate --graph --all
```

## Reto adicional

Explica con un dibujo de commits por qué un `push --force` podría hacer que una referencia remota deje de alcanzar un commit de otra persona, aunque el objeto Git todavía exista temporalmente en algún repositorio.

## Resumen

- un rechazo non-fast-forward protege historia remota;
- primero fetch e inspección, después integración;
- `pull --ff-only` expresa una restricción útil;
- fuerza no es una respuesta genérica a divergencia;
- sincronizar es reconciliar historias, no “quitar errores”.

## Siguiente paso

Continúa con [stash, worktree y checkpoint 03](12-stash-worktree-y-checkpoint-03.md).

## Referencias

- [`git-fetch`](https://git-scm.com/docs/git-fetch)
- [`git-pull`](https://git-scm.com/docs/git-pull)
- [`git-push`](https://git-scm.com/docs/git-push)
- [Distributed Git — Contributing to a Project](https://git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project)
