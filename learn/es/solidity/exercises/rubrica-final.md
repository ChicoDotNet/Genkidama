# Rúbrica final — Solidity / FreelanceEscrow

Puntaje total: **100**.

| Área | Puntos |
|---|---:|
| Lectura y modificación de contrato existente | 10 |
| Referencia de proyecto, ABI y evento de creación | 15 |
| Bugfix de identidad de participantes | 10 |
| Conservación de máquina de estados y autorización | 15 |
| Transferencias, atomicidad y regresión de reentrada | 15 |
| Pruebas deterministas, fuzzing y manejo de errores | 15 |
| Tooling, traces y verificación reproducible | 8 |
| Diseño, claridad y explicación de riesgos | 7 |
| Consulta y aplicación de documentación oficial | 5 |

- **90–100:** evidencia sólida para tareas Junior guiadas sobre contratos pequeños y pruebas locales.
- **75–89:** fundamentos funcionales; corrige puntos débiles antes de presentar el proyecto.
- **60–74:** existen huecos de seguridad, tooling o comprensión que conviene practicar.
- **<60:** repasa los bloques débiles y reintenta la evaluación.

No predice contratación ni equivale a auditoría de seguridad.

La evaluación no se considera completa si `bash tools/verify.sh` queda rojo, se debilitan regresiones existentes, el bug de identidad carece de prueba, la referencia puede ser vacía, una transferencia fallida deja estado parcial o la reentrada consigue un segundo pago.
