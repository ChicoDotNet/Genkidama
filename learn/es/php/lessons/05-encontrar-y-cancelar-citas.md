# Lección 05 — Encontrar y cancelar citas

## Qué vas a conseguir

Vas a incorporar el primer cambio de ciclo de vida de AgendaPHP: localizar una cita por ID y cancelarla sin alterar las demás.

## Antes de empezar

Completa la [Lección 04](04-evitar-cruces-y-checkpoint-01.md).

## El problema

Una agenda real no sólo crea citas. El cliente puede cancelar. Borrar por posición de arreglo o desde la plantilla haría depender la regla del orden visual.

## Concepto

`Schedule::find()` convierte el ID en identidad explícita. `Schedule::without()` construye un calendario candidato nuevo y falla si la cita no existe. El servicio persiste ese candidato sólo cuando la operación completa fue válida.

## Demostración

[DEMO] Registra dos citas, cancela la primera y comprueba que la segunda conserva ID, horario y datos.

## Código real

Revisa [`Schedule.php`](../app/src/Domain/Schedule.php) y `AppointmentService::cancel()` en [`AppointmentService.php`](../app/src/Application/AppointmentService.php).

```php
$candidate = $schedule->without($id);
$store->save($candidate);
```

La UI envía el ID mediante POST. Una cancelación modifica estado y por eso no se implementa como enlace GET.

## Qué acaba de pasar

La identidad dejó de ser un detalle de persistencia: ahora sirve para dirigir cambios explícitos sin acoplar dominio a HTML.

## Errores comunes

- Cancelar por índice de la tabla.
- Tratar una cita inexistente como éxito silencioso.
- Usar GET para una acción destructiva.
- Modificar el store antes de validar la identidad.

## Buenas prácticas

Los comandos deben ser explícitos, recuperables y testeables fuera de HTTP. La UI describe la acción como “Cancelar cita”, no como un icono ambiguo.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba que intente cancelar un ID inexistente y demuestre que el calendario durable permanece igual.

## Cómo comprobar

```bash
cd app
bash tools/verify.sh
```

Prueba también dos citas desde el navegador y cancela sólo una.

## Solución enlazada

Compara con las pruebas del repositorio después de tu intento; el checkpoint formal llegará en la lección 08.

## Reto adicional

¿Qué debería ocurrir si dos pestañas intentan cancelar la misma cita casi al mismo tiempo? No lo resuelvas aún: identifica qué limitación tiene el store JSON actual.

## Resumen

- Los IDs dirigen cambios de ciclo de vida.
- Las acciones destructivas usan POST.
- El estado candidato se valida antes de persistirse.

## Siguiente paso

Continúa con [Lección 06 — Editar sin saltarse las reglas](06-editar-sin-saltarse-las-reglas.md).

## Referencias

- [DomainException — PHP](https://www.php.net/manual/en/class.domainexception.php)
- [HTTP Semantics — safe methods](https://www.rfc-editor.org/rfc/rfc9110.html#name-safe-methods)
