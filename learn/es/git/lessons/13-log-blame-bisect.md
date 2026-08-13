# Lección 13 — Diagnosticar regresiones con `log`, `blame` y `bisect`

## Qué vas a conseguir

Localizarás el primer commit que introdujo una regresión en ReleaseDesk usando evidencia de historia, procedencia de líneas y búsqueda binaria, en lugar de revisar commits al azar.

## Antes de empezar

Completa la [Lección 12](12-stash-worktree-y-checkpoint-03.md) y deja tu working tree limpio.

## El problema

Ayer ReleaseDesk estaba bien. Hoy una comprobación falla, pero entre ambos puntos existen varios commits. Leer toda la historia manualmente puede funcionar con cinco commits; no escala cuando hay decenas o cientos.

Necesitas responder tres preguntas distintas:

1. ¿qué commits tocaron esta zona?;
2. ¿de qué commit proviene una línea concreta?;
3. ¿cuál fue el primer commit en el que una comprobación pasó de buena a mala?

## Concepto

`git log` reduce la historia a un alcance relevante. `git blame` muestra qué commit introdujo cada línea del estado actual. `git bisect` hace búsqueda binaria entre un punto conocido como bueno y otro conocido como malo.

`blame` significa procedencia, no culpabilidad. Un commit puede haber introducido una línea correcta en su contexto original y quedar expuesto por un cambio posterior.

## Demostración — reducir el espacio de búsqueda

Crea una branch descartable:

```text
git switch -c diagnostic/bisect-demo main
```

Agrega `docs/health.txt` con:

```text
status=healthy
```

Haz commit y guarda su SHA como tu punto bueno conocido. Después crea varios commits; en uno de ellos cambia la primera línea a:

```text
status=broken
```

Agrega dos commits posteriores que no reparen esa línea.

Ahora inspecciona sólo el archivo relevante:

```text
git log --oneline -- docs/health.txt
git log -p -- docs/health.txt
git blame -L 1,1 docs/health.txt
```

`log` responde qué pasó en el tiempo. `blame` responde de dónde viene la línea actual.

## Demostración — búsqueda binaria

Inicia bisect con un commit actual malo y el SHA conocido como bueno:

```text
git bisect start
git bisect bad HEAD
git bisect good <SHA_BUENO>
```

Git seleccionará un commit intermedio. Ejecuta la comprobación:

```text
grep -q "^status=healthy$" docs/health.txt
```

Si pasa:

```text
git bisect good
```

Si falla:

```text
git bisect bad
```

Repite hasta que Git identifique el primer commit malo.

Al terminar siempre regresa al contexto original:

```text
git bisect reset
```

## Automatizar un bisect

Si tienes una comprobación determinista cuyo código de salida es `0` cuando el commit es bueno y distinto de cero cuando es malo, Git puede repetirla:

```text
git bisect start
git bisect bad HEAD
git bisect good <SHA_BUENO>
git bisect run sh -c 'grep -q "^status=healthy$" docs/health.txt'
git bisect reset
```

No automatices con una prueba destructiva, no determinista o dependiente de datos que cambian fuera del repositorio sin controlar esas variables.

## Qué acaba de pasar

No “adivinaste” el commit culpable. Reduciste el problema con historia, inspeccionaste procedencia y luego dejaste que Git descartara aproximadamente la mitad del intervalo en cada paso.

## Errores comunes

- Usar `blame` como mecanismo social para señalar personas.
- Ejecutar `bisect` sin un punto bueno que realmente hayas validado.
- Marcar un commit como malo por una prueba intermitente.
- Olvidar `git bisect reset` y seguir trabajando en un HEAD temporal.
- Buscar en toda la historia cuando el problema está limitado a un archivo o ruta.

## Buenas prácticas

Empieza con la menor pregunta verificable:

```text
git status --short
git log --oneline -- ruta/relevante
git show <sha> -- ruta/relevante
git blame ruta/relevante
```

Usa `bisect` cuando puedas definir objetivamente “bueno” y “malo”.

## Tu turno

Construye una historia de al menos cinco commits sobre `docs/health.txt`: uno bueno conocido, uno que introduzca la regresión y al menos dos posteriores. Encuentra el primer commit malo sin mirar de antemano el SHA que lo introdujo.

## Cómo comprobar

Debes poder mostrar:

```text
git log --oneline -- docs/health.txt
git blame -L 1,1 docs/health.txt
git bisect log
```

Antes de salir de bisect, explica por qué el commit identificado es el **primero** malo y no simplemente un commit malo.

## Reto adicional

Repite el ejercicio con diez commits. Compara cuántos commits inspeccionaste manualmente frente a cuántos pasos necesitó `bisect`.

## Resumen

- `log` reduce la historia relevante;
- `blame` muestra procedencia de líneas actuales;
- `bisect` encuentra una transición buena→mala mediante búsqueda binaria;
- una prueba determinista permite automatizar el diagnóstico;
- diagnóstico no significa culpabilidad humana.

## Siguiente paso

Continúa con [reflog y recuperación avanzada](14-reflog-y-recuperacion-avanzada.md).

## Referencias

- [`git-log`](https://git-scm.com/docs/git-log)
- [`git-blame`](https://git-scm.com/docs/git-blame)
- [`git-bisect`](https://git-scm.com/docs/git-bisect)
- [Debugging with Git — Pro Git](https://git-scm.com/book/en/v2/Git-Tools-Debugging-with-Git)
