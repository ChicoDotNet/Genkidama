# Lección 01 — Primer repositorio y primer commit

## Qué vas a conseguir

Convertirás una carpeta normal en un repositorio Git, configurarás una identidad **local al laboratorio**, prepararás los archivos de ReleaseDesk y crearás tu primer commit verificable.

## Antes de empezar

Necesitas Git instalado y una copia independiente de [`../app/`](../app/) como explica el [README del curso](../README.md#preparar-releasedesk).

Comprueba:

```text
git --version
```

No trabajes dentro de `learn/es/git/app/`: inicializar un repositorio anidado dentro del checkout de Genkidama confunde el ejercicio.

## El problema

Sin control de versiones, un proyecto suele terminar con copias como:

```text
proyecto/
proyecto-final/
proyecto-final-2/
proyecto-ahora-si/
```

Esas copias no explican bien **qué cambió**, **por qué cambió** ni **qué versión produjo un resultado**.

## Concepto

Git mantiene una base de datos local de objetos y referencias dentro de `.git/`. Cuando haces un commit no “guardas un archivo suelto”: registras un snapshot identificable del proyecto y su relación con la historia anterior.

Tres ideas bastan hoy:

1. tu **working tree** son los archivos visibles que editas;
2. `git add` selecciona contenido para el siguiente commit;
3. `git commit` registra esa selección en la historia.

## Demostración

[EN PANTALLA] Entra a tu copia de ReleaseDesk y ejecuta:

```text
git init -b main
git status
```

Git crea `.git/` y `main` como branch inicial. Todavía no existe ningún commit.

Para no cambiar la configuración global de tu computadora durante el laboratorio, define identidad sólo en este repositorio:

```text
git config user.name "Estudiante Genkidama"
git config user.email "estudiante@example.invalid"
```

Comprueba de dónde vienen los valores:

```text
git config --list --show-origin
```

## Código real

[EJECUTAR]

```text
git status
git add README.md CHANGELOG.md .gitignore docs/plan.md
git status
git commit -m "chore: iniciar ReleaseDesk"
git log --oneline --decorate
```

No copies el hash que aparezca en la demostración de otra persona. Tu commit tendrá su propio identificador porque incorpora contenido y metadatos de tu ejecución.

## Qué acaba de pasar

Antes del `add`, Git veía archivos no rastreados. Después del `add`, esos contenidos quedaron preparados en el **staging area**. `commit` creó el primer snapshot y movió `main` para apuntar a él.

`git status` después del commit debería indicar que no tienes cambios pendientes.

## Errores comunes

- Ejecutar `git init` en la carpeta equivocada.
- Configurar una identidad global sólo para superar el ejercicio sin entender el alcance.
- Usar `git add .` automáticamente sin mirar qué estás seleccionando.
- Creer que guardar un archivo en el editor equivale a crear un commit.
- Escribir mensajes como `cambios`, `asdf` o `final` que no explican intención.

## Buenas prácticas

Antes de cada commit pregunta:

- ¿qué problema resuelve este cambio?;
- ¿qué archivos estoy preparando?;
- ¿el commit representa una unidad coherente?;
- ¿podría explicar su intención leyendo el mensaje dentro de seis meses?

## Tu turno

1. Ejecuta `git status`.
2. Comprueba que el working tree está limpio.
3. Ejecuta `git log --oneline --decorate`.
4. Identifica el hash abreviado, el branch y el mensaje de tu primer commit.
5. Abre `.git/config` **sólo para leerlo** y localiza `user.name` y `user.email`.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

Estas órdenes deben terminar sin error:

```text
git rev-parse --is-inside-work-tree
git branch --show-current
git log -1 --oneline
git status --short
```

Esperas `true`, `main`, un commit y ninguna línea de estado pendiente.

## Solución

No necesitas copiar una solución si las cuatro comprobaciones anteriores coinciden. Si algo no coincide, usa `git status` y `git config --list --show-origin` antes de borrar y empezar de nuevo.

## Reto adicional

Ejecuta:

```text
git show --stat HEAD
```

Sin memorizar opciones todavía, identifica qué archivos pertenecen al primer commit.

## Resumen

- un repositorio Git local vive en `.git/`;
- working tree, staging y commit son estados distintos;
- `git status` es la primera herramienta de orientación;
- la identidad de autor forma parte de los commits;
- `main` apunta ahora a tu primer snapshot.

## Siguiente paso

Continúa con [Lección 02 — Working tree, staging y diff](02-working-tree-staging-y-diff.md). Ahí dejarás de tratar `git add` como una caja negra y aprenderás a revisar exactamente qué entra al siguiente commit.

## Referencias

- [Getting a Git Repository — Pro Git](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository)
- [First-Time Git Setup — Pro Git](https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup)
- [`git-init`](https://git-scm.com/docs/git-init)
- [`git-status`](https://git-scm.com/docs/git-status)
- [`git-commit`](https://git-scm.com/docs/git-commit)
