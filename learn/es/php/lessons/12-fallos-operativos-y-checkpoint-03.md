# Lección 12 — Fallos operativos + Checkpoint 03

## Qué vas a conseguir

Vas a distinguir un error de entrada de una indisponibilidad del almacenamiento y cerrar el tercer checkpoint sin ocultar corrupción durable.

## Antes de empezar

Completa la [Lección 11](11-exportar-csv-como-frontera.md).

## El problema

Si `appointments.json` está corrupto o no puede leerse, responder “500” sin contexto mezcla un defecto inesperado con una dependencia local temporalmente no disponible. Peor aún sería fingir una agenda vacía y permitir nuevas escrituras sobre evidencia dañada.

## Concepto

AgendaPHP trata `DomainException` como entrada/regla inválida y `RuntimeException` del store como fallo operativo. La frontera HTTP responde **503 Service Unavailable** ante lectura/escritura durable fallida y no sustituye el estado por una colección vacía silenciosa.

## Demostración

[DEMO] Corrompe deliberadamente una copia de laboratorio de `appointments.json` con JSON inválido y recarga. Debes obtener 503 y un mensaje accionable. Repara/restaura el archivo antes de continuar.

## Código real

[`JsonAppointmentStore`](../app/src/Infrastructure/JsonAppointmentStore.php) ya valida JSON y estructura. [`public/index.php`](../app/public/index.php) traduce ese fallo de infraestructura a 503, mientras fechas de filtro inválidas siguen siendo 422.

El smoke automatiza ambos casos: fecha imposible → 422; estado durable corrupto → 503.

## Qué acaba de pasar

La aplicación ya comunica tres categorías distintas: petición inválida, recurso inexistente y dependencia de almacenamiento no disponible. Ninguna exige contaminar el dominio con HTTP.

## Errores comunes

- Convertir JSON corrupto en calendario vacío.
- Mostrar detalles internos o rutas del filesystem al usuario.
- Devolver 200 con un mensaje de error.
- Reintentar escrituras automáticamente sobre un estado cuya integridad es desconocida.

## Buenas prácticas

Falla de forma visible y conservadora. Los mensajes externos deben ayudar sin filtrar datos internos; los diagnósticos técnicos detallados pueden pertenecer a logs seguros en un bloque posterior.

## Tu turno — Checkpoint 03

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar

```bash
cd app
bash tools/verify.sh
bash tools/smoke.sh
```

## Solución enlazada

Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de completar un intento.

## Reto adicional

Diseña qué tendría que cambiar para dos procesos escritores. Explica por qué un 503 correcto no resuelve lost updates ni locking multi-proceso.

## Resumen

- 422 representa entrada inválida; 503 representa almacenamiento indisponible.
- La corrupción no se convierte silenciosamente en estado vacío.
- Filtros, resumen y CSV usan la misma proyección.
- JSON sigue siendo válido para el alcance single-process actual, no para coordinación multiwriter.

## Siguiente paso

Continúa con [Lección 13 — Gate profesional y contratos HTTP](13-gate-profesional-y-contratos-http.md).

## Referencias

- [503 Service Unavailable — HTTP Semantics](https://www.rfc-editor.org/rfc/rfc9110.html#name-503-service-unavailable)
- [Exceptions — PHP](https://www.php.net/manual/en/language.exceptions.php)
- [JSON_THROW_ON_ERROR — PHP](https://www.php.net/manual/en/json.constants.php)
