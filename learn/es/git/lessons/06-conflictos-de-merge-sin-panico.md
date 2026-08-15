# Lección 06 — Conflictos de merge sin pánico

## Qué vas a conseguir

Provocarás un conflicto deliberadamente, leerás la evidencia que deja Git y lo resolverás sin borrar información por reflejo.

## Antes de empezar

Completa la [Lección 05](05-remotos-clone-fetch-pull-push.md). Debes tener una primera copia de ReleaseDesk, un remoto `origin` y una segunda copia que actúe como otro integrante del equipo.

## El problema

Dos personas pueden modificar correctamente la misma zona de un archivo y producir dos historias válidas que Git no puede reconciliar por sí solo. Eso no significa que el repositorio esté roto: hace falta una decisión humana.

## Concepto

Un conflicto aparece cuando una operación de integración necesita combinar cambios incompatibles. Git detiene la operación y marca el desacuerdo dentro del archivo.

Los marcadores `<<<<<<<`, `=======` y `>>>>>>>` separan temporalmente las versiones enfrentadas. No son la solución: son evidencia para construir el contenido final correcto.

## Demostración

En la primera copia, cambia una línea existente de `CHANGELOG.md` y registra el commit.

En la segunda copia, partiendo del mismo ancestro, cambia esa misma línea de otra manera, registra y publica a `origin/main`.

Regresa a la primera copia:

```text
git fetch origin
git log --oneline --decorate --graph --all
git merge origin/main
```

Git debe detenerse con conflicto.

## Código real

Antes de editar nada:

```text
git status
git diff
git log --oneline --decorate --graph --all
```

Abre el archivo y decide cuál debe ser el contenido final. Puede ser una de las versiones o una tercera versión que combine correctamente ambas intenciones.

Después:

```text
git add CHANGELOG.md
git status
git diff --staged
git commit
```

El commit resultante conserva la existencia de ambas historias y la decisión usada para reconciliarlas.

## Qué acaba de pasar

Git automatizó lo que podía determinar y se detuvo exactamente donde necesitaba criterio. Resolver un conflicto no consiste en quitar marcadores: consiste en producir el estado correcto del archivo, inspeccionarlo y registrarlo.

## Abortar también es una decisión válida

Si descubres que comenzaste el merge desde la branch equivocada o necesitas más contexto:

```text
git merge --abort
```

Volver al estado previo es preferible a improvisar una resolución que no entiendes.

## Errores comunes

- Elegir una versión sin leer la intención.
- Borrar marcadores y olvidar contenido importante.
- Hacer commit sin revisar `git diff --staged`.
- Tratar un conflicto de contenido como si fuera corrupción del repositorio.

## Buenas prácticas

Durante una integración conflictiva usa siempre `git status`, `git diff` y `git diff --staged`. Conserva una pregunta de negocio: **¿qué debe decir finalmente este archivo?** Git conoce historia; no conoce la intención de ReleaseDesk.

## Tu turno

Provoca un conflicto en `docs/plan.md` entre dos clones, resuélvelo conservando una frase útil de cada intención y publica la reconciliación.

## Cómo comprobar

```text
git status --short
git log --oneline --decorate --graph --all
git show --stat HEAD
```

El working tree debe estar limpio y la historia debe mostrar las dos líneas convergiendo.

## Reto adicional

Antes de resolver, copia en una nota qué intención aporta cada lado. Después de resolver compara esa nota contra el diff staged. Si una intención desapareció, explica por qué fue correcto descartarla.

## Resumen

- un conflicto es una solicitud de decisión, no corrupción;
- `status`, `diff` e historia explican el contexto;
- resolver significa construir el estado final correcto;
- `merge --abort` permite retirarse de una integración mal planteada;
- revisa el staging antes del commit de resolución.

## Siguiente paso

Continúa con la [Lección 07 — Pull Requests y revisión de cambios](07-pull-requests-y-revision.md).

## Referencias

- [`git-merge`](https://git-scm.com/docs/git-merge)
- [Basic Merge Conflicts — Pro Git](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging)
- [Advanced Merging — Pro Git](https://git-scm.com/book/en/v2/Git-Tools-Advanced-Merging)
