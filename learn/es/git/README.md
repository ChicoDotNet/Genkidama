# Curso de Git desde cero — Domina cambios, ramas y recuperación con ReleaseDesk

Este curso enseña **Git como herramienta profesional transversal**, no como una colección de comandos memorizados. Desde la primera lección trabajas sobre **ReleaseDesk**, un pequeño repositorio real que evoluciona mediante commits, staging, recuperación, ramas, remotos, versiones y trabajo paralelo.

No necesitas saber programar. Los archivos del laboratorio son Markdown y texto para que Git sea el problema que estás aprendiendo a resolver.

## ¿Qué es Git y para qué se utiliza?

Git es un sistema distribuido de control de versiones. Permite registrar cambios, comparar estados, recuperar versiones anteriores, trabajar en líneas paralelas y colaborar con trazabilidad.

Los cursos de lenguajes de Genkidama Learn **no vuelven a enseñar Git sustancialmente**: cuando necesites esa habilidad, te referirán a este curso.

## ¿Puedo aprenderlo desde cero?

Sí. Necesitas una terminal, editor y Git instalado. Las primeras lecciones son locales; desde la 5 incorporamos remotos reproducibles sin exigir GitHub ni Internet.

## ¿Qué vas a construir?

**ReleaseDesk** es una bitácora de entregas con README, plan y changelog. Copiarás [`app/`](app/) fuera del checkout de Genkidama y convertirás esa copia en tu repositorio de práctica.

El curso crea después un remoto bare, un segundo clone y worktrees adicionales para practicar colaboración distribuida y cambio de contexto sin depender de una cuenta externa.

## Tooling verificado

- Git **2.54.0** probado en Ubuntu 24.04 mediante CI.
- Objetivo principal: Windows 11 + PowerShell + VS Code.
- Alternativa soportada: Linux actual + bash + VS Code.
- El curso usa comportamiento estable y ampliamente soportado; `course.yml` conserva la versión realmente probada.

## Instalar

```text
git --version
```

Windows:

```powershell
winget install --id Git.Git -e --source winget
```

Debian/Ubuntu:

```bash
sudo apt install git
```

## Preparar ReleaseDesk

Copia `app/` fuera del repositorio Genkidama para evitar un repositorio anidado accidental.

PowerShell:

```powershell
$lab = Join-Path $HOME "genkidama-git-lab"
Remove-Item $lab -Recurse -Force -ErrorAction SilentlyContinue
Copy-Item -Recurse .\app $lab
Set-Location $lab
```

bash:

```bash
lab="$HOME/genkidama-git-lab"
rm -rf "$lab"
cp -R ./app "$lab"
cd "$lab"
```

## Ruta — 12/17 implementadas

1. [Primer repositorio y primer commit](lessons/01-primer-repositorio-y-primer-commit.md)
2. [Working tree, staging y diff](lessons/02-working-tree-staging-y-diff.md)
3. [Historia y recuperación segura](lessons/03-historia-y-recuperacion-segura.md)
4. [Ramas, merge y checkpoint 01](lessons/04-ramas-merge-y-checkpoint.md)
5. [Remotos: clone, fetch, pull y push](lessons/05-remotos-clone-fetch-pull-push.md)
6. [Conflictos de merge sin pánico](lessons/06-conflictos-de-merge-sin-panico.md)
7. [Pull Requests y revisión de cambios](lessons/07-pull-requests-y-revision.md)
8. [Rebase consciente y checkpoint 02](lessons/08-rebase-consciente-y-checkpoint.md)
9. [Tags y releases](lessons/09-tags-y-releases.md)
10. [`.gitignore`, `.gitattributes` y finales de línea](lessons/10-ignore-attributes-y-finales-de-linea.md)
11. [Sincronización colaborativa segura](lessons/11-sincronizacion-colaborativa-segura.md)
12. [Stash, worktree y checkpoint 03](lessons/12-stash-worktree-y-checkpoint-03.md)
13. Buscar regresiones con log, blame y bisect — planeada.
14. Reflog y recuperación avanzada — planeada.
15. Automatización, hooks y políticas — planeada.
16. Secretos, firma, hardening y checkpoint 04 — planeada.
17. Evaluación final sin receta — planeada.

## Checkpoints

- [Checkpoint 01](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)

## Qué sabrás hacer al terminar

El objetivo completo es que puedas explicar el modelo de Git; preparar cambios conscientemente; leer diffs e historia; usar ramas; integrar y resolver conflictos; trabajar con remotos y Pull Requests; marcar versiones; normalizar archivos; sincronizar sin destruir historia; cambiar de contexto; recuperar errores; investigar regresiones; proteger secretos; y desenvolverte en un repositorio existente sin depender de recetas.

## Build, Test y Run

Git no compila ReleaseDesk. El equivalente operativo es demostrar que historia, referencias, tags, atributos y working trees tienen el estado esperado.

```text
git status
git log --oneline --decorate --graph --all
git tag --list
git worktree list
```

El workflow [`learn-git.yml`](../../../.github/workflows/learn-git.yml) crea repositorios temporales y ejecuta de forma reproducible el arco acumulado: init, staging, recuperación, branches, remotos, conflictos, rebase, tags, políticas de archivos, rechazo non-fast-forward, stash y worktree.

## Trabajo y alcance

Git aparece transversalmente en desarrollo, mantenimiento, automatización, datos, infraestructura y colaboración técnica. Este curso entrega práctica demostrable; **no garantiza contratación** y tampoco sustituye las políticas concretas de un equipo.

## FAQ

### ¿Git y GitHub son lo mismo?
No. Git es el sistema de control de versiones. GitHub es una plataforma que hospeda repositorios y agrega Pull Requests, permisos, CI y conversación de revisión.

### ¿Por qué practicamos remotos sin GitHub?
Porque `clone`, `fetch`, `merge`, `pull` y `push` pertenecen a Git. Un remoto bare local permite entenderlos de forma determinista antes de agregar autenticación, red y UX de una plataforma.

### ¿Rebase es mejor que merge?
No universalmente. Rebase puede ser apropiado para una branch privada que quieres actualizar; merge conserva explícitamente topología. El contexto y las reglas del equipo mandan.

### ¿Por qué no usar `push --force` cuando el remoto rechaza mi cambio?
Porque el rechazo puede estar protegiendo commits de otra persona. Primero debes obtener e inspeccionar la historia remota y decidir cómo reconciliarla.

### ¿Stash y worktree resuelven lo mismo?
No. Stash conserva WIP temporal para limpiar un working tree; worktree permite mantener varios working trees/branches activos sobre el mismo repositorio de objetos.

## Glosario

- **working tree:** archivos que ves y editas;
- **staging area / index:** selección exacta del próximo commit;
- **commit:** snapshot identificado y enlazado con su historia;
- **HEAD:** contexto actualmente seleccionado;
- **branch:** referencia móvil;
- **remote:** otro repositorio conocido por un nombre local;
- **remote-tracking branch:** observación local de una referencia remota, por ejemplo `origin/main`;
- **merge:** reconciliación de historias;
- **rebase:** reaplicación de commits sobre una base distinta;
- **tag:** referencia usada para nombrar un hito estable;
- **`.gitignore`:** reglas para rutas no rastreadas que no deberían entrar al índice;
- **`.gitattributes`:** política versionada sobre tratamiento de archivos;
- **stash:** almacenamiento temporal de cambios no comprometidos;
- **worktree:** working tree adicional asociado al mismo repositorio;
- **Pull Request:** conversación/proceso de revisión de una branch ofrecido por una plataforma.

## Cómo hablar de este proyecto en una entrevista

Explica ReleaseDesk como un laboratorio distribuido. Describe working tree/staging/commit; cómo inspeccionas antes de registrar; por qué `fetch` no mueve automáticamente `main`; cómo resuelves divergencia sin fuerza bruta; cómo anclas una versión con tag; cómo evitas diffs de EOL; y cuándo eliges stash, worktree, merge o rebase.

## Referencias oficiales

- [Git Reference](https://git-scm.com/docs)
- [Pro Git](https://git-scm.com/book/en/v2)
- [Working with Remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)
- [Tagging](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
- [`gitattributes`](https://git-scm.com/docs/gitattributes)
- [`git-worktree`](https://git-scm.com/docs/git-worktree)
- [`git-stash`](https://git-scm.com/docs/git-stash)

## Siguiente paso

Si empiezas desde cero, comienza con la [Lección 1](lessons/01-primer-repositorio-y-primer-commit.md). Si ya completaste los tres checkpoints, el próximo bloque empezará en la lección 13 con diagnóstico de regresiones.
