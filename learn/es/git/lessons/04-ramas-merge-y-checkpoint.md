# Lección 04 — Ramas, merge y checkpoint 01

## Qué vas a conseguir

Crearás una branch para una mejora de ReleaseDesk, registrarás cambios sin mover `main`, integrarás la branch y completarás el primer checkpoint recuperando lo aprendido en las cuatro lecciones.

## Antes de empezar

Completa la [Lección 03](03-historia-y-recuperacion-segura.md). Si dejaste un cambio útil sin commit, decide conscientemente si lo registrarás o restaurarás antes de cambiar de branch.

Comprueba:

```text
git status
git branch --show-current
```

Debes entender cualquier cambio pendiente antes de continuar.

## El problema

Quieres preparar notas de la próxima entrega sin convertir cada paso intermedio en el estado oficial de `main`.

Crear otra copia de la carpeta volvería al problema original. Una branch de Git permite que dos líneas de trabajo apunten a commits distintos dentro del mismo repositorio.

## Concepto

Una branch no es una carpeta completa duplicada. Es una referencia móvil a un commit.

Cuando ejecutas:

```text
git switch -c feature/release-notes
```

Git crea la referencia y cambia `HEAD` para trabajar sobre ella. Los nuevos commits avanzan esa branch; `main` sigue apuntando a donde estaba.

`git merge` integra la historia seleccionada en la branch actual. Dependiendo de cómo diverjan las historias, Git puede hacer fast-forward, crear un merge commit o pedirte resolver conflictos. Hoy trabajaremos un caso sin conflicto.

## Demostración

[EJECUTAR]

```text
git switch -c feature/release-notes
git branch --show-current
```

Agrega a `CHANGELOG.md`:

```markdown
## 0.2 — Preparación

- Se agregó una línea de trabajo independiente para las notas de entrega.
```

Inspecciona y registra:

```text
git diff
git add CHANGELOG.md
git diff --staged
git commit -m "docs: preparar notas de entrega 0.2"
git log --oneline --decorate --graph --all
```

## Código real

Regresa a `main` e integra:

```text
git switch main
git log --oneline --decorate --graph --all
git merge feature/release-notes
git log --oneline --decorate --graph --all
git status
```

Cuando hayas comprobado que la integración quedó bien:

```text
git branch -d feature/release-notes
```

Eliminar la referencia de una branch ya integrada no elimina los commits alcanzables desde `main`.

## Qué acaba de pasar

Mientras trabajabas en `feature/release-notes`, `main` no avanzó. El merge hizo que `main` incorporara la historia de la feature. En este escenario puede ocurrir un fast-forward porque `main` no recibió commits competidores durante la branch.

Más adelante provocarás divergencia y conflictos deliberadamente. Hoy basta con entender referencias, contexto actual e integración.

## Errores comunes

- Crear una branch y olvidar en cuál estás antes de hacer commit.
- Cambiar de branch con trabajo pendiente sin entender cómo afecta el working tree.
- Pensar que borrar una branch integrada borra automáticamente sus commits.
- Ejecutar merge desde la branch equivocada: la dirección importa.
- Crear branches para todo sin una intención concreta.

## Buenas prácticas

Antes de modificar:

```text
git status
git branch --show-current
```

Antes de integrar:

```text
git log --oneline --decorate --graph --all
git diff main...feature/release-notes
```

La segunda orden compara la intención de la branch contra su base común con `main`; la estudiaremos con más profundidad al llegar a Pull Requests.

## Tu turno — Checkpoint 01

Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) **sin abrir la solución**.

Vas a agregar una plantilla de incidentes en una branch propia, revisar working tree/staging, producir commits coherentes, integrarla en `main` y demostrar la historia final.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

Al terminar el checkpoint debes poder ejecutar:

```text
git status --short
git branch --show-current
git log --oneline --decorate --graph --all
```

El working tree debe estar limpio, la branch actual debe ser `main` y la historia debe contener tu trabajo de la plantilla.

## Solución

Sólo después de un intento consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional

Antes de borrar la branch del checkpoint, ejecuta:

```text
git branch --merged
```

Explica por qué esa salida es una mejor evidencia para decidir si una branch local puede eliminarse que confiar únicamente en tu memoria.

## Resumen

- una branch es una referencia móvil, no una copia completa del proyecto;
- `HEAD` indica el contexto actual de trabajo;
- los commits avanzan la branch seleccionada;
- merge integra historia hacia la branch actual;
- status, diff y log siguen siendo herramientas de orientación antes de actuar.

## Siguiente paso

Continúa con la [Lección 05 — Remotos: clone, fetch, pull y push](05-remotos-clone-fetch-pull-push.md).

## Referencias

- [`git-branch`](https://git-scm.com/docs/git-branch)
- [`git-switch`](https://git-scm.com/docs/git-switch)
- [`git-merge`](https://git-scm.com/docs/git-merge)
- [Branches in a Nutshell — Pro Git](https://git-scm.com/book/en/v2/Git-Branching-Branches-in-a-Nutshell)
- [Basic Branching and Merging — Pro Git](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging)
