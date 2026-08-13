# Lección 15 — Automatización, hooks y políticas que sí se pueden gobernar

## Qué vas a conseguir

Crearás un hook local que evita un tipo de commit defectuoso, observarás su alcance real y distinguirás automatización de conveniencia local frente a políticas que deben vivir en CI o en la plataforma del repositorio.

## Antes de empezar

Completa la [Lección 14](14-reflog-y-recuperacion-avanzada.md).

## El problema

Tu equipo acuerda que ningún cambio staged debe contener la marca `FORBIDDEN-WIP`. Puedes recordar revisarlo manualmente, automatizarlo localmente o exigirlo en un control compartido.

Esas tres cosas no ofrecen la misma garantía.

## Concepto

Git puede ejecutar programas llamados **hooks** en momentos específicos. Un `pre-commit` corre antes de crear un commit local y puede rechazarlo con un código de salida distinto de cero.

Los hooks son excelentes para feedback rápido, pero un hook del cliente **no es una frontera de gobierno suficiente**: cada clone tiene configuración local, el usuario controla su entorno y algunos hooks pueden omitirse deliberadamente.

Por eso distinguimos:

- **conveniencia local:** hooks;
- **regla compartida verificable:** CI/checks;
- **gobierno de integración:** protección de branch, revisiones requeridas, permisos y políticas de la plataforma.

## Demostración — un hook versionable pero opt-in

Crea una branch de laboratorio:

```text
git switch -c policy/hooks-demo main
mkdir -p .githooks
```

Crea `.githooks/pre-commit`:

```sh
#!/usr/bin/env sh
set -eu

git diff --cached --check

if git diff --cached --unified=0 | grep -q '^+.*FORBIDDEN-WIP'; then
  echo 'Commit rechazado: elimina FORBIDDEN-WIP del contenido staged.' >&2
  exit 1
fi
```

Hazlo ejecutable en sistemas POSIX/Git Bash y registra el archivo. Después configura **este clone** para usar esa carpeta:

```text
git config core.hooksPath .githooks
```

La configuración `core.hooksPath` no se comparte automáticamente con otros clones sólo por haber versionado `.githooks/`.

## Probar la defensa local

Agrega deliberadamente a `docs/plan.md` una línea:

```text
FORBIDDEN-WIP
```

Haz stage e intenta commit. El hook debe rechazarlo.

Repara el archivo, vuelve a hacer stage y confirma que un cambio válido sí puede registrarse.

## Qué acaba de pasar

El hook mejoró tu ciclo local: fallaste antes de enviar nada. Pero no demostraste que todos los colaboradores tengan el hook activado ni que una plataforma remota lo vaya a ejecutar.

Una política importante debe tener una segunda defensa compartida, por ejemplo un check de CI que analice el repositorio o el diff de la PR.

## Hooks no son seguridad perimetral

Git ofrece mecanismos para omitir ciertos hooks locales. Eso es útil en escenarios controlados, pero demuestra por qué un hook del cliente no debe ser tu única protección para secretos, pruebas, formato o cumplimiento.

No diseñes una política crítica suponiendo que “nadie podrá saltársela”. Diseña una defensa repetible en el punto donde el equipo integra cambios.

## Errores comunes

- Creer que versionar `.githooks/` activa hooks automáticamente en todos los clones.
- Convertir un hook lento o frágil en un castigo para cada commit.
- Poner reglas críticas únicamente del lado del desarrollador.
- Modificar archivos desde un hook sin que el usuario entienda qué cambió.
- Usar hooks para ocultar que el pipeline compartido carece de validaciones.

## Buenas prácticas

Un buen hook local:

- es rápido;
- es determinista;
- explica por qué rechaza;
- no destruye trabajo;
- replica, cuando corresponde, una regla que también existe en CI.

Comprueba qué carpeta usa tu clone:

```text
git config --get core.hooksPath
```

Y recuerda que una configuración local no equivale a política organizacional.

## Tu turno

Crea un hook que ejecute `git diff --cached --check` y además rechace una marca de prueba elegida por ti. Demuestra un commit rechazado y uno aceptado. Después explica dónde pondrías la misma regla para que una PR no pueda integrarse sin cumplirla.

## Cómo comprobar

Entrega evidencia de:

```text
git config --get core.hooksPath
git diff --cached --check
git status --short
```

Y la salida del intento rechazado.

## Reto adicional

Diseña una matriz de tres columnas: “hook local”, “CI” y “protección de branch”. Coloca al menos cinco reglas posibles y justifica dónde debe vivir cada una.

## Resumen

- hooks dan feedback local temprano;
- `core.hooksPath` puede apuntar a una carpeta versionada, pero su activación sigue siendo local;
- una política crítica necesita validación compartida;
- CI y protección de integración resuelven problemas que un hook de cliente no puede gobernar;
- automatizar no significa automáticamente asegurar.

## Siguiente paso

Continúa con [secretos, firma, hardening y checkpoint 04](16-secretos-firma-hardening-y-checkpoint-04.md).

## Referencias

- [`githooks`](https://git-scm.com/docs/githooks)
- [`git-config`](https://git-scm.com/docs/git-config)
- [Customizing Git — Git Hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
