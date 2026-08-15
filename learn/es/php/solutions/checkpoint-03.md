# Solución de referencia — Checkpoint 03

> Consulta esta referencia sólo después de intentar el checkpoint. No existe una única solución correcta.

## Proyección única

La referencia mantiene `appointments.json` como única fuente durable. `Schedule::between()` produce el subconjunto temporal y `matchingService()` aplica el segundo filtro. Ninguno modifica el calendario original.

El rango diario usa medianoche local inclusiva y la medianoche siguiente exclusiva. Así una cita exactamente a las 00:00 del día siguiente no se cuenta dos veces.

## Resumen

El conteo usa el mismo `Schedule` filtrado que alimenta la tabla. `bookedMinutes()` suma `durationMinutes` sobre esa proyección. No se persiste ninguno de los dos valores porque pueden reconstruirse de forma barata y determinista.

## CSV

`AppointmentCsvExporter` recibe la proyección ya resuelta. Su trabajo es representación: encabezados, timezone, orden y escaping mediante `fputcsv`. El exporter no conoce query strings ni vuelve a decidir qué citas pertenecen al reporte.

Una prueba útil incluye un servicio como `Consulta, fiscal` y espera una celda entrecomillada correctamente. Otra verifica que el orden del CSV sea el de `Schedule::all()`.

## Errores

Una fecha sintácticamente válida pero imposible debe rechazarse explícitamente. La referencia verifica `DateTimeImmutable::getLastErrors()` y también que el valor formateado de vuelta coincida con la entrada.

Si `JsonAppointmentStore` encuentra JSON inválido, lanza `RuntimeException`. La frontera web responde 503 y muestra un mensaje genérico; no expone la ruta del filesystem ni reemplaza el estado con una agenda vacía.

## Qué no resolvimos

JSON sigue siendo adecuado para este laboratorio single-process, pero no implementa coordinación entre varios escritores. Dos procesos podrían leer la misma versión y publicar estados incompatibles. SQLite sería razonable cuando necesitemos transacciones, consultas selectivas, mayor volumen o coordinación de escrituras; el puerto `AppointmentStore` existe precisamente para sustituir la implementación sin mover reglas al dominio.

## Verificación

```bash
cd app
composer test
bash tools/smoke.sh
```

El smoke de referencia verifica create → update, consulta por fecha/servicio, minutos derivados, CSV, fecha inválida 422, cancelación y finalmente JSON corrupto → 503.

Vuelve a la [Lección 12](../lessons/12-fallos-operativos-y-checkpoint-03.md) y explica el diseño con tus propias palabras antes de continuar.
