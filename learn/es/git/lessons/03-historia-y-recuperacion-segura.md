# Lección 03 — Historia y recuperación segura

## Qué vas a conseguir

Leerás la historia de ReleaseDesk, inspeccionarás un commit concreto y practicarás dos recuperaciones frecuentes: descartar un cambio no preparado y sacar un archivo del staging **sin reescribir la historia**.

## Antes de empezar

Completa la [Lección 02](02-working-tree-staging-y-diff.md). Procura comenzar con:

```text
git status --short
```

sin salida.

## El problema

Cuando algo sale mal, un principiante suele buscar “el comando para deshacer” y termina copiando una orden destructiva de Internet. Git tiene varias formas de recuperar estados porque **no todos los errores son el mismo error**.

Hoy sólo resolveremos situaciones locales y todavía no reescribiremos commits.

## Concepto

Primero orienta el problema:

- `git log` responde **qué historia tengo**;
- `git show` responde **qué contiene un commit**;
- `git diff` responde **qué cambié y aún no preparé**;
- `git diff --staged` responde **qué preparé**;
- `git restore <archivo>` puede devolver el working tree al contenido del índice;
- `git restore --staged <archivo>` quita contenido del staging conservándolo en tu working tree.

La regla práctica es: **inspecciona antes de restaurar**.

## Demostración

[EJECUTAR]

```text
git log --oneline --decorate --graph
git show --stat HEAD
git show HEAD
```

Ahora abre `README.md` y cambia accidentalmente `Versión inicial: 0.1` por:

```text
Versión inicial: BORRAR TODO
```

Comprueba:

```text
git diff -- README.md
git status --short
```

Como no quieres conservar ese cambio:

```text
git restore README.md
git diff -- README.md
```

## Código real

Ahora simula otro error: agrega a `docs/plan.md` una línea válida, prepárala y después decide que **sí quieres conservarla**, pero no en el próximo commit:

```text
git add docs/plan.md
git diff --staged -- docs/plan.md
git restore --staged docs/plan.md
git status --short
git diff -- docs/plan.md
```

La modificación sigue en tu archivo; solamente dejó de estar preparada.

## Qué acaba de pasar

En el primer caso descartaste una modificación del working tree. En el segundo cambiaste la selección del próximo commit sin perder el trabajo local.

Ésa es una diferencia fundamental: **restaurar contenido y cambiar staging no son la misma operación**.

## Errores comunes

- Ejecutar `reset --hard` por costumbre cuando el problema sólo era un archivo no preparado.
- Restaurar antes de mirar `git diff`.
- Confundir “sacar de staging” con “borrar el cambio”.
- Usar hashes de ejemplos sin comprobar qué commit representan en tu repositorio.
- Pensar que `git log` es sólo una lista decorativa en vez de una herramienta de diagnóstico.

## Buenas prácticas

Antes de cualquier recuperación pregunta:

1. ¿el cambio está sin preparar, staged o ya committed?;
2. ¿quiero conservar el contenido y sólo cambiar su estado?;
3. ¿hay trabajo de otra persona involucrado?;
4. ¿puedo demostrar qué voy a perder antes de ejecutar la orden?

En esta etapa priorizamos comandos que no reescriben historia publicada.

## Tu turno

1. Usa `git log --oneline` para elegir el commit anterior a `HEAD`.
2. Inspecciónalo con `git show <hash>`.
3. Modifica dos líneas de `CHANGELOG.md` sin preparar.
4. Ejecuta `git diff -- CHANGELOG.md`.
5. Restaura el archivo.
6. Haz un cambio útil en `docs/plan.md`.
7. Agrégalo a staging.
8. Sácalo de staging sin perderlo.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

Al terminar el paso 8:

```text
git status --short
git diff -- docs/plan.md
git diff --staged -- docs/plan.md
```

Esperas ver el cambio del plan en working tree y no en staged changes.

## Solución

La secuencia de referencia es:

```text
git diff -- CHANGELOG.md
git restore CHANGELOG.md
git add docs/plan.md
git restore --staged docs/plan.md
```

No continúes si no puedes explicar qué conserva y qué descarta cada restauración.

## Reto adicional

Ejecuta:

```text
git log --format="%h %ad %an %s" --date=short
```

Identifica qué metadatos pertenecen al autor y cuáles al mensaje del commit. No necesitas memorizar el formato todavía.

## Resumen

- historia, working tree y staging requieren herramientas distintas;
- `log` y `show` ayudan a orientarte antes de actuar;
- `restore` puede descartar cambios no preparados;
- `restore --staged` puede conservar el trabajo y sólo modificar la selección del commit;
- no necesitas empezar con comandos destructivos para corregir errores cotidianos.

## Siguiente paso

Continúa con [Lección 04 — Ramas, merge y checkpoint 01](04-ramas-merge-y-checkpoint.md). Separarás una mejora de `main`, la integrarás y demostrarás el flujo completo sin receta línea por línea.

## Referencias

- [`git-log`](https://git-scm.com/docs/git-log)
- [`git-show`](https://git-scm.com/docs/git-show)
- [`git-restore`](https://git-scm.com/docs/git-restore)
- [Viewing the Commit History — Pro Git](https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History)
