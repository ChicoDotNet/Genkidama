# Rúbrica final — Git / ReleaseDesk

Puntaje: **100**.

| Área | Puntos |
|---|---:|
| Lectura del repositorio y modelo mental de referencias | 10 |
| Staging, commits coherentes y branch de entrega | 10 |
| Colaboración remota, divergencia y resolución de conflictos | 15 |
| Diagnóstico reproducible con `log` / `blame` / `bisect` | 15 |
| Recuperación con reflog y explicación de límites | 10 |
| Tags, release candidate y trazabilidad | 5 |
| Hooks, políticas compartidas y gobierno de integración | 10 |
| Seguridad de secretos, firma y hardening | 10 |
| Verificación final, limpieza y evidencia | 10 |
| Consulta de documentación oficial y explicación técnica | 5 |

- **90–100:** evidencia sólida para operar Git en tareas Junior guiadas y explicar decisiones.
- **75–89:** fundamentos funcionales; corrige las áreas débiles antes de presentar el proyecto.
- **60–74:** existen huecos de recuperación, colaboración o comprensión que conviene practicar.
- **<60:** repasa los bloques débiles y reintenta la evaluación completa.

No predice contratación; mide evidencia dentro de este laboratorio.

## Condiciones que impiden considerar completa la evaluación

Aunque el puntaje aritmético fuera suficiente, la evaluación no se considera completa si ocurre cualquiera de estos casos sin una justificación y reparación explícitas:

- se usa un secreto o credencial real en el laboratorio;
- se usa force push para borrar trabajo compartido en vez de diagnosticar la divergencia;
- se elimina trabajo de otra persona para “resolver” un conflicto;
- el commit de regresión se selecciona manualmente y `bisect` sólo se usa como decoración;
- no se demuestra recuperación real por reflog;
- el hook nunca demuestra un rechazo real;
- se afirma que borrar un secreto de `HEAD` elimina su exposición histórica;
- se confunde una firma verificable con aprobación, calidad o autorización;
- el repositorio final queda en un estado no comprendido.

La calidad de la explicación importa: un comando correcto usado por accidente no demuestra el mismo nivel que una operación elegida a partir de evidencia.
