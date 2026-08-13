# Lección 12 — Stash, worktree y checkpoint 03

## Qué vas a conseguir

Separarás dos necesidades distintas: guardar temporalmente trabajo incompleto con `stash` y abrir un segundo working tree real con `worktree` para trabajar en otra branch sin clonar otra vez.

## Antes de empezar

Completa la [Lección 11](11-sincronizacion-colaborativa-segura.md) y deja tu `main` sincronizada.

## El problema

Estás modificando ReleaseDesk y aparece una tarea urgente. No quieres hacer un commit basura sólo para cambiar de contexto, pero tampoco quieres perder trabajo. Más tarde necesitas mantener dos branches activas al mismo tiempo.

Son problemas distintos.

## Concepto

`git stash` guarda temporalmente cambios del working tree/index y permite recuperarlos después. Es útil para WIP corto; no es una bandeja permanente de trabajo ni reemplaza commits significativos.

`git worktree` permite asociar working trees adicionales al mismo repositorio. Cada uno puede tener una branch distinta seleccionada sin duplicar todos los objetos Git.

## Demostración — stash

Haz un cambio no terminado:

```text
printf "\nWIP: revisar notas.\n" >> README.md
git status --short
git stash push -m "wip: revisar notas"
git status --short
git stash list
```

El working tree queda limpio. Inspecciona antes de recuperar:

```text
git stash show --stat stash@{0}
git stash show -p stash@{0}
git stash pop
```

`pop` aplica y elimina la entrada si puede completarse. `apply` permite conservarla para una recuperación más cauta.

## Demostración — worktree

Desde el repositorio principal:

```text
git worktree add -b feature/release-checklist ../releasedesk-checklist main
git worktree list
```

En `../releasedesk-checklist` puedes editar y hacer commits mientras tu working tree principal continúa en `main`.

Cuando hayas terminado e integrado la branch:

```text
git worktree remove ../releasedesk-checklist
git worktree prune
git branch -d feature/release-checklist
```

No elimines la carpeta a ciegas antes de comprobar qué worktree/branch contiene.

## Qué elegir

Usa stash para **interrumpir brevemente** trabajo local que todavía no merece commit.

Usa worktree cuando necesitas **dos contextos de branch activos a la vez**, por ejemplo atender un hotfix mientras una feature conserva herramientas, archivos generados o contexto propio.

Usa commits cuando el trabajo ya representa una unidad explicable y recuperable. No uses stash para evitar diseñar una historia razonable.

## Errores comunes

- Acumular stashes sin nombre durante semanas.
- Hacer `stash pop` sin revisar qué cambios locales podrían competir.
- Borrar manualmente un worktree y dejar metadata huérfana.
- Intentar seleccionar la misma branch simultáneamente en dos worktrees normales.
- Confundir un worktree adicional con un repositorio independiente.

## Buenas prácticas

```text
git stash list
git worktree list
git status --short
```

Nombra tus stashes y elimina worktrees deliberadamente cuando terminen.

## Tu turno — Checkpoint 03

Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

El checkpoint integra las lecciones 9–12: tag anotado, política de ignore/atributos, rechazo non-fast-forward, stash y worktree.

## Cómo comprobar

Debes poder mostrar:

```text
git tag --list
git check-attr -a -- README.md
git stash list
git worktree list
git log --oneline --decorate --graph --all
```

Y explicar qué evidencia demuestra cada requisito.

## Solución

Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de intentar el escenario completo.

## Reto adicional

Compara una segunda clonación con un worktree adicional usando `git rev-parse --git-dir` y `git rev-parse --git-common-dir`. Explica qué comparten y qué sigue siendo específico de cada working tree.

## Resumen

- stash guarda WIP temporal;
- un stash debe ser inspeccionable y de vida corta;
- worktree permite múltiples working trees sobre un mismo almacén de objetos;
- la branch activa y el estado siguen siendo contexto local de cada working tree;
- herramientas distintas resuelven problemas distintos.

## Siguiente paso

El siguiente incremento investigará regresiones con `log`, `blame` y `bisect`, y después profundizará en recuperación con `reflog`.

## Referencias

- [`git-stash`](https://git-scm.com/docs/git-stash)
- [`git-worktree`](https://git-scm.com/docs/git-worktree)
- [Stashing and Cleaning — Pro Git](https://git-scm.com/book/en/v2/Git-Tools-Stashing-and-Cleaning)
