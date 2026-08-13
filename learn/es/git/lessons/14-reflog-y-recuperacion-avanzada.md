# Lección 14 — `reflog` y recuperación avanzada

## Qué vas a conseguir

Recuperarás un commit que dejó de ser alcanzable desde una branch visible después de eliminar esa branch, usando el reflog local para reconstruir una referencia segura.

## Antes de empezar

Completa la [Lección 13](13-log-blame-bisect.md) y deja el repositorio fuera de cualquier sesión de `bisect`.

## El problema

Eliminaste una branch, hiciste un reset equivocado o terminaste un rebase y después descubriste que necesitabas recuperar el estado anterior. `git log` ya no muestra fácilmente ese commit porque ninguna referencia normal apunta a él.

Eso no significa automáticamente que el objeto haya desaparecido.

## Concepto

El **reflog** registra movimientos recientes de referencias locales y de `HEAD`: cambios de branch, commits, resets, rebases y otras operaciones que mueven referencias.

```text
git reflog
git reflog --all
```

Es una bitácora local de navegación y movimiento de refs. No es una copia de seguridad remota, no se comparte mediante `push` y sus entradas pueden expirar.

## Demostración

Crea una branch controlada:

```text
git switch -c recovery/reflog-demo main
```

Agrega `docs/reflog-note.md`, haz un commit con mensaje reconocible y comprueba:

```text
git log -1 --oneline
```

Vuelve a `main` y elimina deliberadamente la branch de laboratorio:

```text
git switch main
git branch -D recovery/reflog-demo
```

Ahora:

```text
git branch --list
git log --all --oneline -- docs/reflog-note.md
```

La branch ya no existe. Busca el movimiento en el reflog:

```text
git reflog --all
```

Cuando identifiques el SHA correcto, inspecciónalo **antes de recrear nada**:

```text
git show <SHA_RECUPERADO>
```

Si realmente es el commit buscado, crea una referencia nueva:

```text
git branch recovered/reflog-demo <SHA_RECUPERADO>
git show recovered/reflog-demo:docs/reflog-note.md
```

## Qué acaba de pasar

Eliminar una branch eliminó una referencia, no necesariamente el objeto commit en ese instante. El reflog conservó evidencia local suficiente para volver a nombrarlo.

Esa ventana no es eterna. Los objetos inalcanzables pueden terminar siendo podados después de que expiren referencias/reflogs y opere la recolección de basura.

## Reflog después de reset o rebase

También puedes usar:

```text
git reflog show HEAD
git reflog show nombre-de-branch
```

para localizar el estado anterior a un `reset`, `rebase` o cambio de branch.

No ejecutes otro reset destructivo “para probar” antes de haber inspeccionado el commit candidato con `git show`.

## Errores comunes

- Confundir reflog con historial compartido del repositorio.
- Asumir que otro clone tendrá tu mismo reflog.
- Recuperar el primer SHA que parece plausible sin inspeccionarlo.
- Esperar que reflog rescate objetos eliminados para siempre.
- Usar `branch -D` en trabajo real sólo porque ahora sabes recuperarlo.

## Buenas prácticas

Cuando sospeches pérdida de una referencia:

```text
git status --short
git reflog --all
git show <sha-candidato>
```

Primero observa. Después crea una **nueva referencia** al commit correcto. Evita introducir más movimientos destructivos mientras diagnosticas.

## Tu turno

Crea una branch `recovery/practice`, agrega un archivo único, haz commit y anota únicamente el mensaje del commit. Elimina la branch de laboratorio. Sin usar el historial de tu terminal para copiar el SHA, localízalo mediante reflog, inspecciónalo y recupéralo como `recovered/practice`.

## Cómo comprobar

```text
git branch --list
git reflog --all
git show recovered/practice
git status --short
```

Explica por qué el commit seguía recuperable y por qué eso no convierte reflog en una estrategia de backup.

## Reto adicional

En una branch descartable, haz dos commits, ejecuta un `git reset --hard HEAD~1` **sólo dentro del laboratorio**, encuentra el estado anterior mediante reflog y crea una branch de rescate sin mover `main`.

## Resumen

- reflog registra movimientos recientes de refs locales y `HEAD`;
- una branch eliminada puede dejar commits todavía recuperables;
- observa e inspecciona antes de recrear referencias;
- reflog no es compartido ni permanente;
- saber recuperar no justifica operar destructivamente sin necesidad.

## Siguiente paso

Continúa con [automatización, hooks y políticas](15-hooks-automatizacion-y-politicas.md).

## Referencias

- [`git-reflog`](https://git-scm.com/docs/git-reflog)
- [`git-branch`](https://git-scm.com/docs/git-branch)
- [Data Recovery — Pro Git](https://git-scm.com/book/en/v2/Git-Internals-Maintenance-and-Data-Recovery)
