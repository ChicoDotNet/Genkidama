# Lección 14 — Debugging desde evidencia

## Qué vas a conseguir

Vas a diagnosticar fallos de AgendaPHP separando petición, dominio, persistencia y representación antes de tocar código.

## Antes de empezar

Completa la [Lección 13](13-gate-profesional-y-contratos-http.md).

## El problema

Cuando un formulario “no funciona”, cambiar varias capas a la vez puede esconder la causa. Un 422, 403, 415 o 503 cuenta historias distintas y debe dirigir la investigación.

## Concepto

Debugging profesional empieza por evidencia observable: status HTTP, input reproducible, estado durable antes/después y test mínimo que falla. Después ubicas la frontera responsable.

Usa este orden:

1. reproduce con la menor petición posible;
2. identifica status y mensaje;
3. confirma si el archivo durable cambió;
4. ejecuta la prueba más cercana a la frontera sospechosa;
5. corrige una causa y vuelve a correr el gate completo.

## Demostración

[DEMO] Envía un POST sin token CSRF. Debe responder 403 y el archivo de citas no debe aparecer. Luego envía el mismo formulario con una fecha inválida y token válido: ahora el resultado es 422. La diferencia demuestra que la primera petición fue rechazada en HTTP y la segunda sí alcanzó validación de dominio.

## Código real

[`public/index.php`](../app/public/index.php) ordena los checks de la frontera antes de llamar a [`AppointmentService`](../app/src/Application/AppointmentService.php). [`JsonAppointmentStore`](../app/src/Infrastructure/JsonAppointmentStore.php) reserva `RuntimeException` para fallos de almacenamiento.

## Qué acaba de pasar

El status no es decoración: reduce el espacio de búsqueda. También evitamos imprimir stack traces o rutas internas a una persona usuaria.

## Errores comunes

- Agregar `var_dump` permanentes con datos personales.
- Corregir simultáneamente HTTP, dominio y persistencia.
- Confundir un 503 con “agenda vacía”.
- Rerunear CI esperando que un fallo determinista desaparezca.

## Buenas prácticas

Captura sólo la evidencia necesaria. En aplicaciones con datos reales, nombres, citas y payloads pueden ser sensibles; no deben terminar en logs diagnósticos por comodidad.

## Tu turno

Provoca un 415 y un 503 en una copia de laboratorio. Para cada uno escribe: frontera responsable, evidencia, dato que no debes registrar y prueba que protege la corrección.

## Cómo comprobar

```bash
bash tools/smoke.sh
composer test
```

## Solución enlazada

Compara tu diagnóstico con los escenarios automatizados en [`tools/smoke.sh`](../app/tools/smoke.sh).

## Reto adicional

Diseña un identificador de correlación que ayude a soporte sin almacenar contenido del formulario.

## Resumen

- Status + estado durable orientan el diagnóstico.
- Una frontera debe fallar antes de delegar a la siguiente.
- Los datos personales no son material de logging por defecto.
- Una regresión reproducible vale más que una corrección intuitiva.

## Siguiente paso

Continúa con [Lección 15 — Medir antes de optimizar](15-medir-antes-de-optimizar.md).

## Referencias

- [PHP — Error handling](https://www.php.net/manual/en/book.errorfunc.php)
- [HTTP Semantics](https://www.rfc-editor.org/rfc/rfc9110.html)
