# Curso de Git desde cero — Domina cambios, ramas y recuperación con ReleaseDesk

Este curso enseña **Git como herramienta profesional transversal**, no como una colección de comandos memorizados. Desde la primera lección trabajas sobre **ReleaseDesk**, un pequeño repositorio real que evoluciona mediante commits, staging, recuperación, ramas, remotos e integración.

No necesitas saber programar. Los archivos del laboratorio son Markdown y texto para que Git sea el problema que estás aprendiendo a resolver.

## ¿Qué es Git y para qué se utiliza?

Git es un sistema distribuido de control de versiones. Permite registrar cambios, comparar estados, recuperar versiones anteriores, trabajar en líneas paralelas y colaborar con trazabilidad.

Los cursos de lenguajes de Genkidama Learn **no vuelven a enseñar Git sustancialmente**: cuando necesites esa habilidad, te referirán a este curso.

## ¿Puedo aprenderlo desde cero?

Sí. Necesitas una terminal, editor y Git instalado. Las primeras lecciones son locales; desde la 5 incorporamos remotos reproducibles sin exigir GitHub ni Internet.

## ¿Qué vas a construir?

**ReleaseDesk** es una bitácora de entregas con README, plan y changelog. Copiarás [`app/`](app/) fuera del checkout de Genkidama y convertirás esa copia en tu repositorio de práctica.

El curso después crea un remoto bare y una segunda copia para practicar colaboración distribuida sin depender de una cuenta externa.

## Tooling verificado

- Git **2.54.0** probado en Ubuntu 24.04 mediante CI.
- Objetivo principal: Windows 11 + PowerShell + VS Code.
- Alternativa soportada: Linux actual + bash + VS Code.
- El curso usa comportamiento estable y ampliamente soportado; `course.yml` conserva la versión realmente probada.

## Instalar

Comprueba:

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

## Ruta — 8/17 implementadas

1. [Primer repositorio y primer commit](lessons/01-primer-repositorio-y-primer-commit.md)
2. [Working tree, staging y diff](lessons/02-working-tree-staging-y-diff.md)
3. [Historia y recuperación segura](lessons/03-historia-y-recuperacion-segura.md)
4. [Ramas, merge y checkpoint 01](lessons/04-ramas-merge-y-checkpoint.md)
5. [Remotos: clone, fetch, pull y push](lessons/05-remotos-clone-fetch-pull-push.md)
6. [Conflictos de merge sin pánico](lessons/06-conflictos-de-merge-sin-panico.md)
7. [Pull Requests y revisión de cambios](lessons/07-pull-requests-y-revision.md)
8. [Rebase consciente y checkpoint 02](lessons/08-rebase-consciente-y-checkpoint.md)
9. Tags y releases — planeada.
10. `.gitignore`, atributos y finales de línea — planeada.
11. Colaboración segura y sincronización — planeada.
12. Stash, worktree y checkpoint 03 — planeada.
13. Buscar regresiones con log, blame y bisect — planeada.
14. Reflog y recuperación avanzada — planeada.
15. Automatización, hooks y políticas — planeada.
16. Secretos, firma, hardening y checkpoint 04 — planeada.
17. Evaluación final sin receta — planeada.

## Checkpoints

- [Checkpoint 01](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)

## Qué sabrás hacer al terminar

El objetivo completo es que puedas explicar el modelo de Git; preparar cambios conscientemente; leer diffs e historia; usar ramas; integrar y resolver conflictos; trabajar con remotos y Pull Requests; recuperar errores; investigar regresiones; proteger secretos; y desenvolverte en un repositorio existente sin depender de recetas.

## Build, Test y Run

Git no compila ReleaseDesk. El equivalente operativo es demostrar que la historia, las referencias y los working trees tienen el estado esperado.

```text
git status
git log --oneline --decorate --graph --all
```

El workflow [`learn-git.yml`](../../../.github/workflows/learn-git.yml) crea una copia temporal, un remoto bare y un segundo clone. Ejecuta de forma reproducible el flujo de las lecciones 1–8: init, staging, restore, branches, merge, fetch/push, conflicto, revisión de feature y rebase privado.

## Trabajo y alcance

Git aparece transversalmente en desarrollo, mantenimiento, automatización, datos, infraestructura y colaboración técnica. Este curso entrega práctica demostrable; **no garantiza contratación** y tampoco sustituye las políticas concretas de un equipo.

## FAQ

### ¿Git y GitHub son lo mismo?
No. Git es el sistema de control de versiones. GitHub es una plataforma que hospeda repositorios y agrega Pull Requests, permisos, CI y conversación de revisión.

### ¿Por qué practicamos remotos sin GitHub?
Porque `clone`, `fetch`, `merge`, `pull` y `push` pertenecen a Git. Un remoto bare local permite entenderlos de forma determinista antes de agregar autenticación, red y UX de una plataforma.

### ¿Pull Request es un comando de Git?
No. La PR es una capacidad de plataformas de colaboración. Git aporta las branches, commits, diffs y referencias que la PR compara.

### ¿Rebase es mejor que merge?
No universalmente. Rebase puede ser apropiado para una branch privada que quieres actualizar sobre una base nueva; merge conserva explícitamente la topología de historias. El contexto y las reglas del equipo mandan.

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
- **Pull Request:** conversación y proceso de revisión de una branch propuesto por una plataforma, no por Git core.

## Cómo hablar de este proyecto en una entrevista

Explica ReleaseDesk como un laboratorio distribuido. Describe working tree/staging/commit; cómo inspeccionas antes de registrar; por qué `fetch` no mueve automáticamente `main`; cómo distingues un conflicto de corrupción; qué revisas antes de una PR; y cuándo una branch privada puede actualizarse con rebase sin reescribir historia compartida.

## Referencias oficiales

- [Git Reference](https://git-scm.com/docs)
- [Pro Git](https://git-scm.com/book/en/v2)
- [Working with Remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)
- [Basic Branching and Merging](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging)
- [Rebasing](https://git-scm.com/book/en/v2/Git-Branching-Rebasing)
- [GitHub Docs — About pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests)

## Siguiente paso

Si empiezas desde cero, comienza con la [Lección 1](lessons/01-primer-repositorio-y-primer-commit.md). Si terminaste el primer bloque, continúa con la [Lección 5](lessons/05-remotos-clone-fetch-pull-push.md).
