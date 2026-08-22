# Checkpoint 04 — Opera WorkstationAudit con límites profesionales

Sin copiar la solución:

1. Resuelve `localhost` y un nombre remoto. El remoto debe exigir opt-in explícito.
2. Genera tres reportes fixture con al menos un `Info`, un `Warning` y un `Critical`.
3. Obtén el resumen con `ThrottleLimit 1` y luego con `2`; los totales deben coincidir.
4. Mide ambas ejecuciones sin convertir el tiempo observado en una promesa de rendimiento.
5. Valida el contrato de los tres reportes.
6. Rompe deliberadamente una severidad y demuestra que la validación la detecta.
7. Calcula el código de salida para un resumen crítico.

## Evidencia esperada
- ninguna función cambia WinRM, firewall, privilegios o configuración del equipo;
- fan-out acotado y resultado determinista;
- errores observables;
- pruebas Pester verdes.

Cuando termines, compara tu enfoque con [la solución de referencia](../solutions/checkpoint-04.md).
