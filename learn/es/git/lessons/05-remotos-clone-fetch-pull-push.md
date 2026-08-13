# Lección 05 — Remotos: clone, fetch, pull y push

## Qué vas a conseguir

Convertirás ReleaseDesk de un repositorio aislado en un repositorio distribuido. Crearás un remoto local, publicarás `main`, clonarás una segunda copia y distinguirás con evidencia qué hacen `fetch`, `pull` y `push`.

## Antes de empezar

Completa la [Lección 04](04-ramas-merge-y-checkpoint.md) y deja `main` limpio.

```text
git status --short
git branch --show-current
```

## El problema

Hasta ahora todo ocurrió en una sola carpeta. En un equipo, otra persona necesita obtener la historia, publicar cambios y sincronizarse sin compartir físicamente tu directorio de trabajo.

## Concepto

Un remoto es un nombre local que apunta a otro repositorio Git. `origin` es una convención, no una palabra mágica.

- `git clone` crea una nueva copia con historia y configura un remoto.
- `git fetch` descarga referencias y objetos sin integrar cambios en tu branch actual.
- `git pull` combina `fetch` con una estrategia de integración; por eso no conviene ejecutarlo sin saber qué historia esperas.
- `git push` intenta actualizar referencias del remoto con commits que ya existen localmente.

Una referencia como `origin/main` es una **remote-tracking branch**: tu última observación local de la rama `main` del remoto.

## Demostración

Para que el ejercicio sea reproducible sin GitHub ni Internet, crea un remoto bare fuera de ReleaseDesk.

bash:

```bash
remote="$HOME/releasedesk-origin.git"
rm -rf "$remote"
git init --bare "$remote"
git remote add origin "$remote"
git push -u origin main
```

PowerShell:

```powershell
$remote = Join-Path $HOME "releasedesk-origin.git"
Remove-Item $remote -Recurse -Force -ErrorAction SilentlyContinue
git init --bare $remote
git remote add origin $remote
git push -u origin main
```

Inspecciona:

```text
git remote -v
git branch -vv
git log --oneline --decorate --graph --all
```

## Código real

Crea una segunda copia en otra ruta:

```text
git clone <ruta-del-remoto> releasedesk-peer
```

Dentro de `releasedesk-peer`, configura identidad local, edita `docs/plan.md`, registra y publica:

```text
git config user.name "Peer ReleaseDesk"
git config user.email "peer@example.invalid"
git add docs/plan.md
git commit -m "docs: agregar criterio de revisión"
git push origin main
```

Regresa a tu primera copia y ejecuta primero:

```text
git fetch origin
git status
git log --oneline --decorate --graph --all
```

Observa que `origin/main` avanzó pero tu `main` todavía no. Si tu branch local no tiene commits competidores, integra explícitamente:

```text
git merge --ff-only origin/main
```

También podrías usar `git pull --ff-only`; aquí separamos fetch e integración para ver qué ocurre.

## Qué acaba de pasar

Git transfirió objetos y actualizó referencias. La red —o en este laboratorio, el filesystem— transportó historia; no sustituyó tu working tree ni decidió automáticamente cómo reconciliar divergencias.

## Errores comunes

- Confundir `origin/main` con una branch editable local.
- Ejecutar `pull` por reflejo sin inspeccionar primero.
- Creer que `push` “sube archivos”: publica objetos y actualiza referencias.
- Forzar un push ante un rechazo sin entender qué commits existen en el remoto.
- Modificar configuración global sólo para completar un laboratorio.

## Buenas prácticas

Antes de sincronizar:

```text
git status --short
git fetch origin
git log --oneline --decorate --graph --all
```

Usa `--ff-only` cuando esperas una integración sin divergencia; si falla, la falla es información útil.

## Tu turno

Desde la segunda copia agrega a `CHANGELOG.md` una entrada breve, publica a `origin/main`, vuelve a la primera copia, ejecuta `fetch`, demuestra que `origin/main` avanzó antes de mover `main` y luego sincroniza con fast-forward.

## Cómo comprobar

```text
git status --short
git branch -vv
git log --oneline --decorate --graph --all
```

La primera copia debe terminar limpia y `main` debe coincidir con `origin/main`.

## Reto adicional

Ejecuta `git remote show origin` y localiza qué branches rastreas y qué operación de push está configurada.

## Resumen

- un remoto es otro repositorio, no “la nube”;
- `fetch` observa sin integrar;
- `pull` observa e integra según una estrategia;
- `push` publica historia local si el remoto puede avanzar de forma válida;
- las remote-tracking branches permiten razonar antes de modificar tu branch.

## Siguiente paso

Continúa con la [Lección 06 — Conflictos de merge sin pánico](06-conflictos-de-merge-sin-panico.md).

## Referencias

- [`git-clone`](https://git-scm.com/docs/git-clone)
- [`git-fetch`](https://git-scm.com/docs/git-fetch)
- [`git-pull`](https://git-scm.com/docs/git-pull)
- [`git-push`](https://git-scm.com/docs/git-push)
- [Working with Remotes — Pro Git](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)
