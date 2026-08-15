# Lección 09 — Tags y releases: nombrar una versión sin moverla

## Qué vas a conseguir

Marcarás un commit estable de ReleaseDesk con un tag anotado, comprobarás qué objeto creó Git y publicarás esa referencia en el remoto sin confundir un tag con una branch o con una release de plataforma.

## Antes de empezar

Completa la [Lección 08](08-rebase-consciente-y-checkpoint.md) y deja `main` sincronizada con `origin/main`.

```text
git switch main
git fetch origin
git merge --ff-only origin/main
git status --short
```

## El problema

`main` continúa avanzando. Decir “la versión buena era más o menos el commit de ayer” no es trazabilidad suficiente. Necesitas un nombre estable para un punto concreto de la historia.

## Concepto

Una branch es una referencia móvil: nuevos commits pueden hacerla avanzar. Un tag normalmente nombra un commit que quieres conservar como hito.

Git soporta tags ligeros y anotados. Para una versión publicada preferiremos un **tag anotado** porque almacena autor del tag, fecha y mensaje además de la referencia.

```text
git tag -a v0.3.0 -m "ReleaseDesk 0.3.0"
```

Crear el tag es local. Igual que una branch, no aparece mágicamente en otro repositorio.

## Demostración

[EJECUTAR]

```text
git tag -a v0.3.0 -m "ReleaseDesk 0.3.0"
git tag --list
git show v0.3.0
git cat-file -t v0.3.0
```

Para un tag anotado, el último comando debe reportar `tag`.

Publica sólo ese tag:

```text
git push origin v0.3.0
```

## Código real

Comprueba qué commit representa:

```text
git rev-list -n 1 v0.3.0
git rev-parse main
git ls-remote --tags origin
```

Si creaste el tag sobre la punta actual de `main`, ambos SHA de commit deben corresponder al mismo estado aunque el objeto tag anotado tenga su propio identificador.

## Tag no es release

Git entiende tags. Plataformas como GitHub pueden construir encima una **release** con notas, archivos y UX de publicación. Una release puede usar un tag como ancla, pero no son el mismo concepto.

## Errores comunes

- Creer que `git push` siempre publica todos los tags.
- Mover o recrear silenciosamente un tag que ya fue consumido por otras personas.
- Usar un tag para señalar una línea de trabajo que en realidad debería seguir avanzando como branch.
- Confundir la versión de un archivo con una versión reproducible del repositorio completo.

## Buenas prácticas

Antes de publicar un tag:

```text
git status --short
git log -1 --oneline
git diff --check HEAD^
```

Comprueba que estás nombrando el commit correcto. Si una versión publicada necesita corrección, normalmente crea una versión nueva en lugar de desplazar silenciosamente la anterior.

## Tu turno

Crea un tag anotado `v0.3.0-lab` sobre un commit que puedas explicar. Inspecciona su tipo y el commit al que apunta. No lo publiques hasta poder describir la diferencia entre ambos objetos.

## Cómo comprobar

```text
git cat-file -t v0.3.0-lab
git rev-list -n 1 v0.3.0-lab
git show v0.3.0-lab
```

## Reto adicional

Crea una branch descartable y un tag sobre el mismo commit. Haz avanzar la branch un commit y demuestra con `git rev-parse` cuál referencia se movió y cuál permaneció estable.

## Resumen

- una branch suele moverse; un tag identifica un hito;
- los tags anotados tienen metadata propia;
- los tags locales se publican explícitamente;
- una release de plataforma es una capa adicional sobre las referencias Git;
- una versión publicada no debería moverse por sorpresa.

## Siguiente paso

Continúa con [`.gitignore`, `.gitattributes` y finales de línea](10-ignore-attributes-y-finales-de-linea.md).

## Referencias

- [`git-tag`](https://git-scm.com/docs/git-tag)
- [`git-push`](https://git-scm.com/docs/git-push)
- [Tagging — Pro Git](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
