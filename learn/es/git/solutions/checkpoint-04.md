# Solución de referencia — Checkpoint 04

Esta solución muestra **una** secuencia válida. No la uses como receta antes de haber intentado el checkpoint. Los SHAs serán distintos en tu repositorio.

## Parte A — Diagnóstico con `log`, `blame` y `bisect`

Crea la historia controlada:

```text
git switch main
git switch -c incident/bisect
```

Crea `docs/health.txt`:

```text
status=healthy
```

Registra el punto bueno:

```text
git add docs/health.txt
git commit -m "test: establecer salud conocida"
git rev-parse HEAD
```

Guarda ese SHA como `SHA_BUENO`.

Agrega varios commits. En uno cambia la primera línea a:

```text
status=broken
```

y deja que commits posteriores mantengan ese estado.

Reduce la historia relevante:

```text
git log --oneline -- docs/health.txt
git blame -L 1,1 docs/health.txt
```

Inicia la búsqueda binaria:

```text
git bisect start
git bisect bad HEAD
git bisect good SHA_BUENO
```

En cada commit elegido por Git ejecuta:

```text
grep -q "^status=healthy$" docs/health.txt
```

Si devuelve `0`:

```text
git bisect good
```

Si falla:

```text
git bisect bad
```

También puedes automatizar el criterio en un entorno donde `sh` esté disponible:

```text
git bisect run sh -c 'grep -q "^status=healthy$" docs/health.txt'
```

Antes de salir puedes inspeccionar:

```text
git bisect log
git rev-parse refs/bisect/bad
```

`refs/bisect/bad` conserva el commit que bisect identificó como primer malo durante la sesión.

Después:

```text
git bisect reset
git switch main
```

La branch `incident/bisect` era de laboratorio; elimínala sólo cuando ya no necesites la evidencia.

## Parte B — Recuperar una branch eliminada

```text
git switch -c recovery/checkpoint-04 main
```

Crea `docs/recovery-note.md`, agrega contenido reconocible y haz commit:

```text
git add docs/recovery-note.md
git commit -m "docs: agregar nota recuperable checkpoint 04"
git switch main
git branch -D recovery/checkpoint-04
```

Confirma que la referencia normal desapareció:

```text
git branch --list
```

Busca la operación:

```text
git reflog --all
```

También puedes filtrar por el mensaje de reflog:

```text
git reflog --all --grep-reflog="nota recuperable checkpoint 04" --format="%H %gs"
```

Inspecciona el SHA candidato:

```text
git show <SHA_RECUPERADO>
```

Sólo después crea una referencia nueva:

```text
git branch recovered/checkpoint-04 <SHA_RECUPERADO>
git show recovered/checkpoint-04:docs/recovery-note.md
```

La recuperación funciona porque el commit todavía existe en el repositorio local y el reflog conserva una referencia temporal a su movimiento. No es una garantía permanente ni un backup remoto.

## Parte C — Hook local

Crea una branch y una carpeta de hooks:

```text
git switch -c policy/checkpoint-04 main
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

Haz ejecutable el archivo donde aplique y configura este clone:

```text
git config core.hooksPath .githooks
```

Agrega deliberadamente `FORBIDDEN-WIP` a un archivo, haz stage e intenta commit. Debe fallar.

Después repara el archivo:

```text
git restore --staged docs/plan.md
git restore docs/plan.md
```

Agrega un cambio válido, haz stage y commit. Debe pasar.

Una política equivalente compartida puede ejecutarse en CI sobre el diff o el árbol completo y configurarse como check requerido antes de integrar a `main`. Ésa es la diferencia de gobierno: el hook acelera feedback local; CI/protección de branch controla integración compartida.

## Parte D — Demostrar persistencia histórica con un valor falso

Crea una branch descartable:

```text
git switch -c security/checkpoint-04 main
```

Crea `demo-secret.txt` usando **sólo**:

```text
DEMO_TOKEN=example-only-not-a-secret
```

Haz commit:

```text
git add demo-secret.txt
git commit -m "test: agregar secreto falso de laboratorio"
git rev-parse HEAD
```

Guarda el SHA como `SHA_SECRETO_FALSO`.

Elimina el archivo y registra la eliminación:

```text
git rm demo-secret.txt
git commit -m "test: retirar secreto falso del estado actual"
```

El archivo ya no existe en el working tree, pero:

```text
git show SHA_SECRETO_FALSO:demo-secret.txt
```

sigue mostrando el valor falso porque forma parte de ese snapshot histórico.

Si hubiera sido una credencial real, la respuesta primaria sería **revocarla o rotarla inmediatamente**, detener propagación y coordinar la limpieza de historia/caches/clones según la política. Reescribir historia por sí solo no vuelve confiable una credencial ya expuesta.

## Cierre y limpieza del laboratorio

Vuelve a `main` y elimina únicamente branches descartables que ya no necesites:

```text
git switch main
git status --short
```

No uses limpieza agresiva de objetos: precisamente quieres conservar evidencia suficiente para comprender reflog e historia.

## Respuestas esperadas de reflexión

1. `log` reconstruye cambios a través del tiempo; `blame` atribuye líneas del estado actual a commits de procedencia.
2. `bisect` sólo puede delimitar correctamente la transición si las etiquetas “good” y “bad” representan observaciones verdaderas y reproducibles.
3. reflog conserva movimientos locales recientes de referencias/HEAD incluso cuando una branch visible desaparece.
4. un hook corre en el cliente y depende de configuración local; CI/protección de branch opera en el punto compartido de integración.
5. un secreto copiado deja de ser confiable; rotación/revocación atiende la credencial, mientras la limpieza de Git atiende exposición residual.
6. una firma prueba procedencia criptográfica bajo ciertas condiciones; una revisión expresa evaluación humana/proceso; autorización de integración depende de permisos y políticas del repositorio.
