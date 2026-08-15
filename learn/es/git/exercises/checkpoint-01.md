# Checkpoint 01 — Integra una plantilla de incidentes

Trabaja sobre tu repositorio local de **ReleaseDesk**. No abras la solución hasta completar un intento.

## Encargo

ReleaseDesk necesita una plantilla breve para documentar incidentes de una entrega.

Debes:

1. comenzar desde `main` con working tree limpio;
2. crear una branch llamada `feature/incident-template`;
3. crear `docs/incident-template.md` con al menos estos encabezados:
   - `# Incidente`
   - `## Impacto`
   - `## Evidencia`
   - `## Acción tomada`
   - `## Seguimiento`;
4. inspeccionar el cambio antes de prepararlo;
5. agregar el archivo al staging y revisar el staged diff;
6. crear un commit cuyo mensaje describa la intención;
7. agregar a `CHANGELOG.md` una línea que anuncie la plantilla;
8. registrar ese cambio en un segundo commit;
9. regresar a `main`;
10. revisar la diferencia de la branch antes de integrarla;
11. hacer merge de `feature/incident-template` en `main`;
12. comprobar historia y estado final;
13. eliminar la branch local únicamente después de confirmar que quedó integrada.

## Restricciones

- No uses `git add .` en este checkpoint: selecciona archivos explícitamente.
- No uses `reset --hard`, `clean -fd` ni otros comandos destructivos para “arreglar” el ejercicio.
- No edites dentro de `.git/`.
- No necesitas GitHub, remoto ni conexión a Internet.

## Evidencia de entrega

Conserva la salida de:

```text
git status --short
git branch --show-current
git log --oneline --decorate --graph --all -8
git show --stat HEAD
```

Debes poder explicar:

- qué cambio estaba en working tree;
- qué preparaste en staging antes de cada commit;
- por qué hiciste dos commits;
- desde qué branch ejecutaste el merge;
- por qué fue seguro borrar la branch local al final.

## Criterio de éxito

El checkpoint está completo cuando `main` contiene `docs/incident-template.md` y la actualización de `CHANGELOG.md`, el working tree está limpio y puedes reconstruir verbalmente el camino desde la branch hasta la integración.

Después de intentarlo, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).
