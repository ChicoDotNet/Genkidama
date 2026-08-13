# Curso de Git desde cero — Domina cambios, ramas y recuperación con ReleaseDesk

Este curso enseña **Git como herramienta profesional transversal**, no como una colección de comandos memorizados. Desde la primera lección trabajas sobre **ReleaseDesk**, un pequeño repositorio real que evoluciona mediante commits, staging, recuperación, ramas, remotos, versiones, diagnóstico y trabajo paralelo.

No necesitas saber programar. Los archivos del laboratorio son Markdown y texto para que Git sea el problema que estás aprendiendo a resolver.

## ¿Qué es Git y para qué se utiliza?

Git es un sistema distribuido de control de versiones. Permite registrar cambios, comparar estados, recuperar versiones anteriores, trabajar en líneas paralelas, investigar regresiones y colaborar con trazabilidad.

Los cursos de lenguajes de Genkidama Learn **no vuelven a enseñar Git sustancialmente**: cuando necesites esa habilidad, te referirán a este curso.

## ¿Puedo aprenderlo desde cero?

Sí. Necesitas una terminal, editor y Git instalado. Las primeras lecciones son locales; desde la 5 incorporamos remotos reproducibles sin exigir GitHub ni Internet.

## ¿Qué vas a construir?

**ReleaseDesk** es una bitácora de entregas con README, plan y changelog. Copiarás [`app/`](app/) fuera del checkout de Genkidama y convertirás esa copia en tu repositorio de práctica.

El curso crea después un remoto bare, un segundo clone y worktrees adicionales para practicar colaboración distribuida, diagnóstico y recuperación sin depender de una cuenta externa.

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

## Ruta — 16/17 implementadas

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
13. [Diagnosticar regresiones con `log`, `blame` y `bisect`](lessons/13-log-blame-bisect.md)
14. [`reflog` y recuperación avanzada](lessons/14-reflog-y-recuperacion-avanzada.md)
15. [Automatización, hooks y políticas](lessons/15-hooks-automatizacion-y-politicas.md)
16. [Secretos, firma, hardening y checkpoint 04](lessons/16-secretos-firma-hardening-y-checkpoint-04.md)
17. Evaluación final sin receta — planeada.

## Checkpoints

- [Checkpoint 01](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)
- [Checkpoint 04](exercises/checkpoint-04.md) · [solución](solutions/checkpoint-04.md)

## Qué sabrás hacer al terminar

El objetivo completo es que puedas explicar el modelo de Git; preparar cambios conscientemente; leer diffs e historia; usar ramas; integrar y resolver conflictos; trabajar con remotos y Pull Requests; marcar versiones; normalizar archivos; sincronizar sin destruir historia; cambiar de contexto; investigar regresiones; recuperar referencias perdidas; distinguir hooks locales de políticas compartidas; responder correctamente ante secretos versionados; interpretar firmas; y desenvolverte en un repositorio existente sin depender de recetas.

## Build, Test y Run

Git no compila ReleaseDesk. El equivalente operativo es demostrar que historia, referencias, tags, atributos y working trees tienen el estado esperado.

```text
git status
git log --oneline --decorate --graph --all
git tag --list
git worktree list
git reflog --all
```

El workflow [`learn-git.yml`](../../../.github/workflows/learn-git.yml) ejecuta dos capas reproducibles:

1. el arco acumulado 1–12: init, staging, recuperación, branches, remotos, conflictos, rebase, tags, políticas de archivos, rechazo non-fast-forward, stash y worktree;
2. el incidente avanzado 13–16 mediante [`tools/verify-advanced.sh`](tools/verify-advanced.sh): `log`/`blame`/`bisect`, recuperación por reflog, hook local con rechazo verificable y persistencia histórica de un secreto **falso**.

El workflow además falla si queda un marcador `PLACEHOLDER` sin resolver dentro del contenido Markdown del curso.

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

### ¿`git blame` dice quién tuvo la culpa?
No. Muestra procedencia de líneas del estado actual. Es evidencia histórica, no un juicio sobre intención, contexto o responsabilidad humana.

### ¿Reflog es un backup?
No. Es una bitácora local y temporal de movimientos de referencias. Puede ayudarte a recuperar estados recientes, pero no sustituye remotos, backups ni una estrategia de continuidad.

### ¿Un pre-commit hook puede imponer una política de empresa?
No por sí solo. Da feedback local temprano, pero la integración compartida debe gobernarse con CI, protección de branches, revisiones y permisos.

### ¿Borrar un secreto del último commit resuelve el incidente?
No. Si una credencial real fue versionada, debes tratarla como comprometida y rotarla/revocarla. La limpieza de historia atiende exposición residual; no vuelve confiable la credencial original.

### ¿Un commit firmado significa que es seguro?
No. Una firma verificable aporta procedencia criptográfica. Revisión, calidad, seguridad y autorización de integración son controles distintos.

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
- **bisect:** búsqueda binaria del punto donde una condición cambia de buena a mala;
- **blame:** procedencia por línea del estado actual;
- **reflog:** registro local reciente de movimientos de refs y `HEAD`;
- **hook:** programa local invocado por Git en un evento concreto;
- **firma:** evidencia criptográfica asociada a un commit o tag cuando se configura y verifica correctamente;
- **Pull Request:** conversación/proceso de revisión de una branch ofrecido por una plataforma.

## Cómo hablar de este proyecto en una entrevista

Explica ReleaseDesk como un laboratorio distribuido y de incidentes. Describe working tree/staging/commit; por qué `fetch` no mueve automáticamente `main`; cómo resuelves divergencia sin fuerza bruta; cómo anclas una versión con tag; cómo evitas diffs de EOL; cuándo eliges stash, worktree, merge o rebase; cómo usarías `bisect` para una regresión; qué puede recuperar reflog; por qué un hook no sustituye CI; y qué harías primero si una credencial real entrara a historia.

## Referencias oficiales

- [Git Reference](https://git-scm.com/docs)
- [Pro Git](https://git-scm.com/book/en/v2)
- [Working with Remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)
- [Tagging](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
- [`gitattributes`](https://git-scm.com/docs/gitattributes)
- [`git-worktree`](https://git-scm.com/docs/git-worktree)
- [`git-stash`](https://git-scm.com/docs/git-stash)
- [`git-bisect`](https://git-scm.com/docs/git-bisect)
- [`git-reflog`](https://git-scm.com/docs/git-reflog)
- [`githooks`](https://git-scm.com/docs/githooks)
- [Signing Your Work](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work)

## Siguiente paso

Si empiezas desde cero, comienza con la [Lección 1](lessons/01-primer-repositorio-y-primer-commit.md). Si ya completaste los cuatro checkpoints, el siguiente y último incremento será la evaluación final autónoma de Git Junior.
