# Rúbrica final — TypeScript / FreelanceDesk

Puntaje total: **100**.

| Área | Puntos |
|---|---:|
| Lectura y modificación de la base existente | 10 |
| Fecha objetivo + compatibilidad legacy | 15 |
| Consulta de proyectos vencidos determinista | 10 |
| Bugfix de IDs duplicados sin estado parcial | 15 |
| Manejo de errores y validación runtime | 10 |
| Pruebas y regresiones | 15 |
| Persistencia durable y contratos HTTP existentes | 10 |
| Tooling, diagnóstico y verificación | 5 |
| Diseño, explicación y tradeoffs | 5 |
| Consulta de documentación oficial | 5 |

Interpretación:

- **90–100:** evidencia sólida para tareas Junior guiadas sobre una base TypeScript existente.
- **75–89:** fundamentos funcionales; conviene reforzar las áreas débiles antes de presentar el proyecto.
- **60–74:** existen huecos de confiabilidad o comprensión que requieren práctica adicional.
- **<60:** repasa los bloques débiles y reintenta la evaluación.

Esta rúbrica no predice contratación. Mide evidencia dentro de FreelanceDesk.

La evaluación **no se considera completa** si ocurre cualquiera de estas condiciones:

- `npm run verify` queda rojo;
- una fecha inválida entra al estado como si fuera válida;
- la regla de vencimiento usa un reloj global no controlable;
- se aceptan dos proyectos con el mismo ID;
- una falla de persistencia deja memoria adelantada;
- se debilita `strict` o se introduce `any` sólo para silenciar errores;
- el bugfix no tiene prueba de regresión;
- el diagnóstico nuevo registra payloads, correos u otra PII.
