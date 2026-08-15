# Checkpoint 03 — Publicar, normalizar y cambiar de contexto sin perder historia

Resuelve este checkpoint sin abrir la solución. Trabaja con tu repositorio `alpha`, un remoto bare `origin` y un segundo clone `beta`.

## Escenario

ReleaseDesk está preparando una versión y dos personas siguen colaborando. Además aparece una tarea urgente mientras tienes trabajo incompleto.

Debes demostrar que puedes:

1. crear un **tag anotado** `v0.4.0-rc1` sobre un commit de `main` que puedas identificar y publicarlo en `origin`;
2. agregar una política explícita que ignore `artifacts/` y defina LF para `*.md` y CRLF para `*.ps1` mediante `.gitattributes`;
3. demostrar con `git check-ignore` y `git check-attr` qué regla aplica;
4. provocar intencionalmente un rechazo `non-fast-forward` entre `alpha` y `beta` y conservar ambos cambios sin usar `git push --force`;
5. crear un cambio WIP en `README.md`, guardarlo con un stash **nombrado**, demostrar que el working tree queda limpio y recuperarlo;
6. crear un segundo worktree en una branch `feature/release-audit`, agregar `docs/release-audit.md` y hacer ahí un commit sin cambiar la branch seleccionada en el working tree principal;
7. dejar ambos working trees limpios y mostrar la historia final.

## Restricciones

- No uses `git reset --hard` para “limpiar” el escenario.
- No uses `git push --force` ni `--force-with-lease`.
- No recrees desde cero un clone cuando aparezca divergencia.
- No resuelvas el conflicto borrando uno de los cambios: ambos aportes deben sobrevivir.
- El stash debe tener un mensaje reconocible.

## Evidencia mínima

Entrega la salida o una explicación verificable de:

```text
git cat-file -t v0.4.0-rc1
git ls-remote --tags origin
git check-ignore -v artifacts/demo.txt
git check-attr text eol -- README.md
git stash list
git worktree list
git log --oneline --decorate --graph --all
git status --short
```

Incluye también la salida del push rechazado y explica **por qué era correcto que Git lo rechazara**.

## Preguntas de reflexión

1. ¿Qué diferencia hay entre publicar una branch y publicar un tag?
2. ¿Por qué `.gitignore` no deja de rastrear automáticamente un archivo ya versionado?
3. ¿Qué información necesitabas antes de decidir cómo resolver el non-fast-forward?
4. ¿Por qué stash no sería una buena ubicación permanente para trabajo importante?
5. ¿Qué comparte un worktree adicional con el repositorio principal y qué conserva como contexto propio?

## Criterio de éxito

No basta con llegar a un estado limpio. Debes poder reconstruir verbalmente qué ocurrió con las referencias, qué historia estaba local/remota en cada momento y por qué ninguna operación eliminó trabajo ajeno.
