# Lección 08 — Compón un diagnóstico reproducible

## Qué vas a conseguir
Integrarás snapshots, configuración y reglas sin hacer que las pruebas dependan del equipo donde corren.

## El problema
Un auditor real toca el sistema; una prueba estable no debe exigir que el runner tenga poca memoria o disco lleno.

## Concepto
`Get-WorkstationAudit` acepta proveedores inyectables para plataforma y sistema. Producción usa los proveedores reales; Pester usa fixtures.

[DEMO] Revisa el test que compone unidades y un snapshot de memoria artificial. La salida y el resumen son predecibles.

## Qué acaba de pasar
La frontera no existe para presumir arquitectura. Existe porque permite probar reglas, simular fallos y conservar I/O en el borde.

## Errores comunes
- Mockear todo el lenguaje en vez de inyectar una dependencia pequeña.
- Afirmar que un test Linux valida CIM de Windows.
- Mezclar remediación con auditoría read-only.

## Tu turno
Completa Checkpoint 02 antes de mirar la solución.

## Siguiente paso
Continúa con [Lección 09 — Inventario acotado de software y servicios](09-inventario-acotado.md).

## Referencias
- https://pester.dev/docs/usage/mocking
