# Lección 08 — Rebase consciente y checkpoint 02

## Qué vas a conseguir

Actualizarás una branch de trabajo sobre una `main` más reciente mediante rebase, observarás cómo cambia su historia y completarás un checkpoint que integra remotos, conflictos, revisión y sincronización.

## Antes de empezar

Completa la [Lección 07](07-pull-requests-y-revision.md). Conserva tu remoto local y dos clones de ReleaseDesk.

## El problema

Mientras trabajas en una feature, `main` sigue avanzando. Puedes integrar esos cambios con merge o reconstruir tus commits sobre la base nueva mediante rebase. Ambas opciones son válidas en contextos distintos; el problema aparece cuando reescribes historia compartida sin entender las consecuencias.

## Concepto

`git rebase <base>` toma los commits exclusivos de tu branch y los vuelve a aplicar sobre otra base. Los commits recreados reciben nuevos identificadores porque cambia su padre y, por tanto, su contenido histórico.

Eso produce una historia lineal útil para branches de trabajo **cuando todavía controlas esa historia**.

Regla práctica inicial:

> No rebases historia pública/compartida que otras personas ya están usando, salvo que el equipo haya acordado explícitamente ese flujo.

## Demostración

Parte de una `main` sincronizada:

```text
git switch main
git fetch origin
git merge --ff-only origin/main
git switch -c feature/rebase-demo
```

Agrega una mejora en `README.md`, registra el commit y **no la integres todavía**.

Desde el segundo clone, agrega un cambio distinto a `docs/plan.md` sobre `main` y publícalo.

Vuelve a tu feature:

```text
git fetch origin
git log --oneline --decorate --graph --all
git rebase origin/main
```

Si los archivos no compiten, Git reaplica tu commit sobre la nueva punta de `origin/main`.

## Código real

Compara antes y después con:

```text
git log --oneline --decorate --graph --all
git merge-base origin/main HEAD
git diff origin/main...HEAD
```

Tu feature debe contener sólo su intención, pero ahora parte de la `main` remota más reciente.

Si ocurre un conflicto durante el rebase, resuelve el archivo, agrega la resolución y continúa con:

```text
git rebase --continue
```

Si decides que el rebase fue una mala idea:

```text
git rebase --abort
```

## Merge vs. rebase

Usa merge cuando quieres preservar explícitamente la topología de dos líneas de historia o cuando la branch ya es compartida y no deseas reescribirla.

Usa rebase con criterio para actualizar una branch privada/de trabajo y presentar una serie coherente sobre una base reciente.

No conviertas “historia bonita” en un objetivo superior a trazabilidad, seguridad o acuerdos del equipo.

## Errores comunes

- Rebasar una branch compartida sin avisar.
- Creer que rebase “mueve” commits sin recrearlos.
- Resolver conflictos de rebase mecánicamente sin entender qué commit se está reaplicando.
- Forzar una actualización remota sin comprobar quién depende de esa historia.
- Usar rebase sólo porque una guía afirma que siempre es más limpio.

## Buenas prácticas

Antes de rebasar:

```text
git status --short
git fetch origin
git log --oneline --decorate --graph --all
```

Después:

```text
git diff --check origin/main...HEAD
git log --oneline origin/main..HEAD
```

Debes poder explicar por qué la reescritura es segura en ese contexto.

## Tu turno — Checkpoint 02

Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

El ejercicio combina dos clones y un remoto bare. Tendrás que observar cambios remotos sin integrarlos automáticamente, resolver una divergencia conflictiva y actualizar una feature privada sobre una `main` más reciente.

## Cómo comprobar

Al terminar:

```text
git status --short
git branch --show-current
git log --oneline --decorate --graph --all
git diff --check origin/main...HEAD
```

Debes poder justificar por qué usaste merge en una parte y rebase en otra.

## Solución

Consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) sólo después de un intento completo.

## Reto adicional

Dibuja en papel o texto la historia antes y después de un rebase con tres commits de feature. Luego compara tu dibujo con `git log --graph`. Si no puedes explicar por qué cambian los IDs, repite la demostración en una branch descartable.

## Resumen

- rebase reaplica commits sobre una nueva base y recrea sus IDs;
- merge y rebase resuelven problemas relacionados, pero preservan historias distintas;
- una branch privada es el contexto más seguro para aprender rebase;
- la historia compartida no se reescribe por capricho;
- revisar estado, base, log y diff precede a una operación de historia.

## Siguiente paso

El siguiente incremento empezará con tags y releases, y después cubrirá `.gitignore`, atributos/finales de línea, sincronización colaborativa y herramientas para trabajo paralelo.

## Referencias

- [`git-rebase`](https://git-scm.com/docs/git-rebase)
- [`git-merge-base`](https://git-scm.com/docs/git-merge-base)
- [Rebasing — Pro Git](https://git-scm.com/book/en/v2/Git-Branching-Rebasing)
