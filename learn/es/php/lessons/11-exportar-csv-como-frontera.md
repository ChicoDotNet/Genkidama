# Lección 11 — Exportar CSV como frontera

## Qué vas a conseguir

Vas a descargar la consulta visible como CSV sin enseñar al dominio a escribir archivos ni duplicar reglas de filtrado.

## Antes de empezar

Completa la [Lección 10](10-resumen-derivado-y-capacidad.md).

## El problema

Un usuario puede necesitar abrir su agenda en una hoja de cálculo o enviarla a otra herramienta. Copiar HTML no es un contrato de intercambio.

## Concepto

CSV es una frontera de representación. `AppointmentCsvExporter` recibe un `Schedule` ya filtrado y decide columnas, escapado y zona horaria. No decide qué citas pertenecen al reporte.

PHP incluye `fputcsv`; usarla evita implementar manualmente comillas, comas y escapes.

## Demostración

[DEMO] Crea una cita cuyo servicio contenga una coma. Descarga CSV y comprueba que la celda esté entrecomillada correctamente. Aplica un filtro por fecha y confirma que el archivo contiene exactamente la misma proyección que la tabla.

## Código real

Revisa [`AppointmentCsvExporter.php`](../app/src/Infrastructure/AppointmentCsvExporter.php). El exporter usa `php://temp`, escribe encabezados y recorre `Schedule::all()` para conservar orden determinista.

[`public/index.php`](../app/public/index.php) sólo añade headers HTTP de descarga después de que la consulta se cargó y validó con éxito.

## Qué acaba de pasar

La lógica de negocio sigue sin conocer CSV. Puedes sustituir esta representación por JSON, iCalendar o una API sin mover reglas de horarios.

## Errores comunes

- Construir CSV concatenando strings con comas.
- Volver a implementar los filtros dentro del exporter.
- Exportar todas las citas cuando la UI muestra una consulta filtrada.
- Enviar headers de descarga antes de saber si el almacenamiento puede leerse.

## Buenas prácticas

Mantén cada frontera pequeña y testeable. Usa funciones estándar para formatos con reglas de escape. Documenta timezone y orden de salida.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega una cita con coma en el servicio y otra fuera del rango. Escribe una prueba que verifique encabezado, escaping y orden del CSV filtrado.

## Cómo comprobar

```bash
composer test
bash tools/smoke.sh
```

El smoke solicita `?date=2026-08-20&export=csv` y comprueba contenido real.

## Solución enlazada

Consulta [`AppointmentCsvExporterTest.php`](../app/tests/AppointmentCsvExporterTest.php) sólo después de tu intento.

## Reto adicional

¿Qué cambiarías para exportar RFC 5545/iCalendar? Identifica qué pertenece al formato y qué debe seguir en dominio.

## Resumen

- CSV es una frontera, no una regla de agenda.
- El exporter recibe una proyección ya decidida.
- `fputcsv` resuelve escaping estándar.
- La exportación conserva orden y zona horaria explícitos.

## Siguiente paso

La [Lección 12](12-fallos-operativos-y-checkpoint-03.md) tratará el caso en que el archivo durable no puede leerse correctamente.

## Referencias

- [fputcsv — PHP](https://www.php.net/manual/en/function.fputcsv.php)
- [Stream wrappers — PHP](https://www.php.net/manual/en/wrappers.php.php)
- [Content-Disposition — MDN](https://developer.mozilla.org/docs/Web/HTTP/Headers/Content-Disposition)
