# Lección 04 — Maneja errores y prueba comportamiento

## Qué vas a conseguir

Harás explícitos los fallos de recolección y ejecutarás pruebas Pester que protegen reglas sin depender del hardware real.

## El problema

Una consulta del sistema puede fallar por plataforma, permisos, proveedor o estado transitorio. Ocultar el error y entregar un reporte incompleto como si fuera correcto sería peor que fallar claramente.

## Concepto

PowerShell tiene errores terminantes y no terminantes. En fronteras donde la operación completa requiere una señal, `try/catch` permite agregar contexto y conservar la excepción original como causa.

Pester ejecuta escenarios automáticos. Una buena prueba protege comportamiento que puede romperse; no existe para inflar un porcentaje.

## Demostración

```powershell
Get-WorkstationAudit -SnapshotProvider { throw 'fallo simulado' }
```

Debe producir un error contextual: `No se pudo recopilar el snapshot...`.

[EJECUTAR]

```powershell
Invoke-Pester ./app/tests -Output Detailed
```

## Código real

`Get-WorkstationAudit` acepta un `SnapshotProvider` inyectable. En producción usa `Get-PlatformSnapshot`; en pruebas recibe fixtures deterministas.

Esto no se presenta como un Design Pattern: es una frontera pequeña para poder probar reglas sin hardware específico.

## Qué acaba de pasar

La suite protege clasificación `Info/Warning/Critical`, capacidad desconocida, composición de auditoría, failure mode del proveedor y JSON exportable.

## Errores comunes

- `catch {}` vacío.
- Convertir todos los errores en `$null`.
- Hacer pruebas que sólo verifican que una función existe.
- Depender del porcentaje libre del runner para decidir si una prueba pasa.

## Buenas prácticas

Prueba reglas con datos controlados y reserva el smoke test para demostrar que la integración con el sistema real funciona.

## Tu turno

Cambia temporalmente un fixture de 5% a 25% y predice qué prueba debería fallar antes de ejecutarla. Revierte el cambio después del ejercicio.

## Cómo comprobar tu solución

Una prueba útil debe fallar cuando rompes deliberadamente el contrato y volver a verde cuando restauras la regla.

## Solución

La solución de referencia del checkpoint está separada en `../solutions/checkpoint-01.md`; no la abras antes de completar el ejercicio.

## Reto adicional

Añade una prueba para exactamente 10% libre y decide, leyendo la implementación, si el contrato actual produce `Warning` o `Critical`.

## Resumen

WorkstationAudit ya tiene errores explícitos, pruebas deterministas y un smoke path contra el sistema real.

## Siguiente paso

Completa [Checkpoint 01 — Añade una regla diagnóstica](../exercises/checkpoint-01.md). Después iniciaremos consultas de sistema y fronteras Windows.

## Referencias

- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_try_catch_finally
- https://pester.dev/docs/quick-start
