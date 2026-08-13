# Curso de Git desde cero — Domina cambios, ramas y recuperación con ReleaseDesk

Este curso enseña **Git como herramienta profesional transversal**, no como una colección de comandos memorizados. Desde la primera lección trabajas sobre **ReleaseDesk**, un pequeño repositorio real que evoluciona mediante commits, staging, recuperación, ramas e integración.

No necesitas saber programar. Los archivos del laboratorio son Markdown y texto para que Git sea el problema que estás aprendiendo a resolver.

## ¿Qué es Git y para qué se utiliza?

Git es un sistema distribuido de control de versiones. Permite registrar cambios, comparar estados, recuperar versiones anteriores, trabajar en líneas paralelas y combinar trabajo con trazabilidad.

Los cursos de lenguajes de Genkidama Learn **no vuelven a enseñar Git sustancialmente**: cuando necesites esa habilidad, te referirán a este curso.

## ¿Puedo aprenderlo desde cero?

Sí. Necesitas una terminal, editor y Git instalado. No necesitas GitHub para comenzar: primero aprenderás Git localmente y después incorporaremos remotos y colaboración.

## ¿Qué vas a construir?

**ReleaseDesk** es una bitácora de entregas con README, plan y changelog. Copiarás [`app/`](app/) fuera del checkout de Genkidama y convertirás esa copia en tu repositorio de práctica.

Esto evita crear un repositorio Git anidado accidentalmente dentro de Genkidama.

## Tooling verificado

- Git **2.54.0** probado en el runner Ubuntu 24.04 usado por CI.
- La verificación de fuentes oficiales del **13 de agosto de 2026** encontró Git upstream 2.55.0 disponible para Linux y Git for Windows 2.54.0 como build mantenido publicado.
- Objetivo principal: Windows 11 + PowerShell + VS Code.
- Alternativa soportada: Linux actual + bash + VS Code.

Git preserva compatibilidad hacia atrás de forma amplia; el curso evita depender de novedades marginales y documentará cualquier requisito de versión cuando aparezca.

## Instalar

Comprueba primero:

```text
git --version
```

En Windows puedes instalar Git for Windows desde el sitio oficial o mediante `winget`:

```powershell
winget install --id Git.Git -e --source winget
```

En Debian/Ubuntu:

```bash
sudo apt install git
```

Usa el mecanismo oficial o del sistema que corresponda a tu plataforma; no necesitas compilar Git para este curso.

## Preparar ReleaseDesk

Desde `learn/es/git/`, copia `app/` **fuera** del repositorio Genkidama.

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

A partir de ahí las lecciones trabajan dentro de esa copia.

## Ruta — 4/17 implementadas

1. [Primer repositorio y primer commit](lessons/01-primer-repositorio-y-primer-commit.md)
2. [Working tree, staging y diff](lessons/02-working-tree-staging-y-diff.md)
3. [Historia y recuperación segura](lessons/03-historia-y-recuperacion-segura.md)
4. [Ramas, merge y checkpoint 01](lessons/04-ramas-merge-y-checkpoint.md)
5. Remotos: clone, fetch, pull y push — planeada.
6. Conflictos de merge sin pánico — planeada.
7. Pull Requests y revisión de cambios — planeada.
8. Rebase consciente y checkpoint 02 — planeada.
9. Tags y releases — planeada.
10. `.gitignore`, atributos y finales de línea — planeada.
11. Colaboración segura y sincronización — planeada.
12. Stash, worktree y checkpoint 03 — planeada.
13. Buscar regresiones con log, blame y bisect — planeada.
14. Reflog y recuperación avanzada — planeada.
15. Automatización, hooks y políticas — planeada.
16. Secretos, firma, hardening y checkpoint 04 — planeada.
17. Evaluación final sin receta — planeada.

## Checkpoint actual

- [Checkpoint 01](exercises/checkpoint-01.md)
- [Solución de referencia](solutions/checkpoint-01.md) — consulta sólo después de intentarlo.

## Qué sabrás hacer al terminar

El objetivo completo es que puedas explicar el modelo de Git; preparar cambios conscientemente; leer diffs e historia; usar ramas; integrar y resolver conflictos; trabajar con remotos y Pull Requests; recuperar errores con herramientas apropiadas; investigar regresiones; proteger secretos; y desenvolverte en un repositorio existente sin depender de recetas.

## Build, Test y Run

Git no compila ReleaseDesk. Aquí el equivalente operativo es comprobar que el repositorio tiene el estado esperado:

```text
git status
git log --oneline --decorate --graph --all
```

El workflow [`learn-git.yml`](../../../.github/workflows/learn-git.yml) crea una copia temporal de ReleaseDesk y ejecuta un flujo reproducible de init → commit → branch → merge como smoke test del material.

## Trabajo y alcance

Git aparece transversalmente en desarrollo, mantenimiento, automatización, datos, infraestructura y colaboración técnica. Este curso entrega práctica demostrable; **no garantiza contratación** y tampoco sustituye las políticas concretas de un equipo.

## FAQ

### ¿Git y GitHub son lo mismo?
No. Git es el sistema de control de versiones. GitHub es una plataforma que aloja repositorios Git y agrega colaboración, Pull Requests, permisos, CI y otros servicios. Aprendemos primero la herramienta y luego la plataforma.

### ¿Voy a memorizar todos los comandos?
No. Aprenderás un modelo mental y comandos frecuentes. Parte de trabajar profesionalmente con Git consiste en consultar ayuda antes de ejecutar operaciones que no dominas.

### ¿Por qué no empezamos clonando Genkidama?
Porque ya estás dentro de un repositorio existente. El primer laboratorio debe permitirte observar con claridad qué crea `git init`, qué cambia `git add` y qué registra un commit.

### ¿Por qué usar identidad local en el laboratorio?
Para no cambiar accidentalmente la configuración global de tu computadora. Más adelante aprenderás los alcances system/global/local y decidirás conscientemente qué configuración quieres conservar.

## Glosario inicial

- **working tree:** archivos que ves y editas;
- **staging area / index:** selección exacta que entrará al siguiente commit;
- **commit:** snapshot identificado y enlazado con su historia;
- **HEAD:** referencia al commit/branch actualmente seleccionado;
- **branch:** referencia móvil a una línea de trabajo;
- **merge:** integración de historias compatibles o reconciliables.

## Cómo hablar de este proyecto en una entrevista

Explica ReleaseDesk como un laboratorio de control de versiones, no como una aplicación de negocio. Describe la diferencia entre working tree, staging y commit; cómo inspeccionas antes de registrar; cómo recuperas un cambio no deseado; y por qué una branch permite experimentar sin mover inmediatamente `main`.

Cuando el curso avance, añade conflictos, remotos, PRs, rebase, bisect, reflog y seguridad sólo si puedes explicar qué problema resuelve cada herramienta.

## Referencias oficiales

- [Git Reference](https://git-scm.com/docs)
- [Pro Git — Getting Started](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)
- [Pro Git — Getting a Git Repository](https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository)
- [Pro Git — First-Time Git Setup](https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup)
- [Instalar Git en Windows](https://git-scm.com/install/windows)
- [Instalar Git en Linux](https://git-scm.com/install/linux)

## Siguiente paso

Prepara una copia limpia de ReleaseDesk y comienza con la [Lección 1](lessons/01-primer-repositorio-y-primer-commit.md).
