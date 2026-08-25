# Checkpoint 02 — Una regla configurable y testeable

Sin editar los providers reales:

1. agrega una configuración con `StorageWarningPercent = 35`, `StorageCriticalPercent = 15` y `MemoryWarningPercent = 25`;
2. crea un fixture de unidad con 20% libre y demuestra que queda `Warning`;
3. crea un fixture de memoria con 20% libre y demuestra que queda `Warning`;
4. ejecuta `Get-WorkstationAudit` con providers artificiales y confirma que el reporte conserva la configuración resuelta;
5. agrega al menos una aserción Pester que fallaría si el auditor volviera a ignorar la configuración.

No uses el disco ni la memoria real de tu equipo como precondición del checkpoint.

Ver solución después de intentarlo: `../solutions/checkpoint-02.md`.
