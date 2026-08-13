# Solución de referencia — Checkpoint 01

Ésta es una secuencia posible. No necesitas reproducir hashes ni mensajes literalmente si tu historia expresa la misma intención.

## 1. Confirmar punto de partida

```text
git switch main
git status --short
```

No debe haber salida pendiente.

## 2. Crear la branch

```text
git switch -c feature/incident-template
```

Crea `docs/incident-template.md`:

```markdown
# Incidente

## Impacto

## Evidencia

## Acción tomada

## Seguimiento
```

Revisa antes de preparar:

```text
git status --short
git diff -- docs/incident-template.md
```

Un archivo nuevo no rastreado puede no aparecer en `git diff` hasta estar preparado; `git status` te dice que existe. Prepáralo explícitamente y revisa:

```text
git add docs/incident-template.md
git diff --staged -- docs/incident-template.md
git commit -m "docs: agregar plantilla de incidentes"
```

## 3. Registrar la actualización del changelog

Agrega a `CHANGELOG.md` una línea como:

```markdown
- Se agregó una plantilla para documentar incidentes.
```

Después:

```text
git diff -- CHANGELOG.md
git add CHANGELOG.md
git diff --staged -- CHANGELOG.md
git commit -m "docs: registrar plantilla de incidentes"
```

Dos commits mantienen separadas la capacidad nueva y la actualización de su registro. Un único commit también podría ser defendible en un equipo con otra política; aquí la separación permite practicar staging e historia.

## 4. Revisar e integrar

```text
git log --oneline --decorate --graph --all
git diff main...feature/incident-template
git switch main
git merge feature/incident-template
```

Comprueba:

```text
git status --short
git log --oneline --decorate --graph --all -8
git branch --merged
```

Si `feature/incident-template` aparece como integrada y el estado es correcto:

```text
git branch -d feature/incident-template
```

## 5. Qué debes poder explicar

- `status` orientó el estado de archivos y branch.
- `diff` mostró cambios antes de staging; `diff --staged` mostró el próximo commit.
- La branch permitió avanzar la feature sin mover inicialmente `main`.
- El merge se ejecutó **desde `main`** porque queríamos integrar la feature hacia `main`.
- `branch --merged` aportó evidencia de que la referencia local podía eliminarse sin perder commits alcanzables desde `main`.

Si tu resultado es verde pero no puedes explicar estas cinco ideas, repite el checkpoint cambiando el nombre de la feature y el contenido de la plantilla.
