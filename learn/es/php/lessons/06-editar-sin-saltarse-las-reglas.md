# Lección 06 — Editar sin saltarse las reglas

## Qué vas a conseguir

Vas a editar cliente, servicio, horario y duración conservando el ID y revalidando cruces como si la cita fuera nueva.

## Antes de empezar

Completa la [Lección 05](05-encontrar-y-cancelar-citas.md).

## El problema

Editar “en sitio” parece fácil, pero puede introducir un cruce que `Schedule::add()` jamás permitiría. Tampoco queremos generar un ID distinto y perder identidad.

## Concepto

`Schedule::replacing()` elimina conceptualmente la versión anterior, intenta agregar la nueva y devuelve un calendario candidato. Si el nuevo intervalo choca, el candidato nunca se persiste.

## Demostración

[DEMO] Mueve una cita 10:00–11:00 a 11:00–12:00: debe conservar su ID. Después intenta moverla encima de otra cita 12:00–13:00: debe fallar y conservar la versión anterior.

## Código real

`AppointmentService::update()` crea un `Appointment` con el mismo ID y datos nuevos. Luego:

```php
$candidate = $schedule->replacing($replacement);
$store->save($candidate);
```

El formulario cambia a modo edición mediante `?edit=<id>` y POST `action=update`; la regla sigue fuera de la plantilla.

## Qué acaba de pasar

Separaste identidad de atributos editables y reutilizaste la misma invariante de solapamiento para create y update.

## Errores comunes

- Generar un ID nuevo al editar.
- Excluir la validación de cruces para “facilitar” updates.
- Mutar la cita anterior antes de saber si el reemplazo es válido.
- Confiar en campos ocultos como autoridad de negocio.

## Buenas prácticas

La aplicación conserva la identidad y el dominio vuelve a validar el estado candidato completo. Un fallo conocido regresa 422 y deja al usuario en modo edición con sus valores.

## Tu turno

[PAUSA PARA EJERCICIO] Escribe una regresión: una edición conflictiva debe lanzar `DomainException` y el store debe conservar exactamente el calendario previo.

## Cómo comprobar

```bash
cd app
vendor/bin/phpunit
```

Además edita una cita desde el navegador y verifica el mismo ID en el archivo JSON.

## Solución enlazada

Revisa `AppointmentServiceTest.php` después de completar tu prueba.

## Reto adicional

¿Qué cambiaría si quisiéramos mantener historial de versiones en vez de reemplazar la cita? Distingue auditoría de estado actual.

## Resumen

- Update conserva identidad.
- El reemplazo vuelve a validar todas las reglas.
- Persistir sólo el candidato válido evita estado fantasma.

## Siguiente paso

Continúa con [Lección 07 — Consultas derivadas sin segunda fuente de verdad](07-consultas-derivadas.md).

## Referencias

- [readonly classes/properties — PHP](https://www.php.net/manual/en/language.oop5.basic.php#language.oop5.basic.class.readonly)
- [Exceptions — PHP](https://www.php.net/manual/en/language.exceptions.php)
