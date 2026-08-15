# Lección 03 — Formulario, POST y persistencia JSON

## Qué vas a conseguir

Vas a convertir entrada HTTP en una cita, guardar un snapshot durable y volver a mostrarlo después de otra petición.

## Antes de empezar

Completa la [Lección 02](02-tipos-clases-y-citas-validas.md).

## El problema

HTTP no conserva automáticamente el estado de la agenda. Si una cita desaparece al recargar, la aplicación todavía no resuelve el trabajo real.

## Concepto

Separamos tres responsabilidades:

- [`AppointmentService`](../app/src/Application/AppointmentService.php) orquesta caso de uso y reglas;
- [`AppointmentStore`](../app/src/Application/AppointmentStore.php) define qué necesita la aplicación para cargar/guardar;
- [`JsonAppointmentStore`](../app/src/Infrastructure/JsonAppointmentStore.php) implementa esa frontera con filesystem.

El servicio no recibe `$_POST`. Recibe scalars explícitos y convierte el `datetime-local` usando la zona horaria configurada.

## Demostración

[DEMO] Ejecuta AgendaPHP, registra una cita, detén el servidor y vuelve a iniciarlo. La cita sigue allí porque `data/appointments.json` conserva el estado.

Después abre el archivo. Es datos, no código PHP ejecutable.

## Código real

`JsonAppointmentStore::save()` serializa un candidato completo, escribe primero un archivo temporal y finalmente lo publica con `rename`. Si falla, lanza `RuntimeException`; no presenta la operación como exitosa.

`load()` distingue: archivo inexistente → primera ejecución; JSON válido → rehidrata el dominio; JSON corrupto → error explícito. No convierte corrupción en “no hay citas”.

## Qué acaba de pasar

La persistencia quedó detrás de una interfaz. Hoy es JSON; mañana puede ser SQLite sin que `Appointment` aprenda SQL.

## Errores comunes

- Confundir archivo inexistente con archivo corrupto.
- Guardar directamente sobre el archivo final y dejarlo truncado ante una falla.
- Aceptar cualquier array decodificado como dato válido.
- Mostrar al usuario el mensaje interno completo de una excepción de filesystem.

## Buenas prácticas

I/O en los bordes, errores accionables, datos externos revalidados al cargarlos y salida HTML escapada. Después de un POST exitoso usamos redirect 303 para evitar reenvíos accidentales al refrescar.

## Tu turno

[PAUSA PARA EJERCICIO] Cambia `AGENDA_DATA_FILE` a una ruta temporal, registra una cita y demuestra que el archivo por defecto no se modifica.

## Cómo comprobar

```bash
composer test
AGENDA_DATA_FILE=/tmp/agenda-php.json composer serve
```

Además revisa [`../app/tests/JsonAppointmentStoreTest.php`](../app/tests/JsonAppointmentStoreTest.php).

## Solución enlazada

No requiere cambiar código: la frontera ya lee la variable de entorno en `public/index.php`.

## Reto adicional

Describe por qué este store no garantiza escrituras coordinadas entre dos procesos concurrentes. No agregues locks distribuidos todavía.

## Resumen

- HTTP y persistencia son fronteras diferentes.
- El dominio no conoce archivos.
- Corrupción se reporta, no se oculta.
- Un snapshot temporal reduce el riesgo de publicación parcial.

## Siguiente paso

Continúa con [Lección 04 — Evitar cruces de horario + Checkpoint 01](04-evitar-cruces-y-checkpoint-01.md).

## Referencias

- [json_decode — PHP manual](https://www.php.net/manual/en/function.json-decode.php)
- [json_encode — PHP manual](https://www.php.net/manual/en/function.json-encode.php)
- [header — PHP manual](https://www.php.net/manual/en/function.header.php)
