# Lección 02 — Tipos, clases y una cita válida

## Qué vas a conseguir

Vas a modelar una cita con tipos explícitos, invariantes y tiempo inmutable, y protegerla con pruebas.

## Antes de empezar

Completa la [Lección 01](01-tu-primera-agenda-web.md) y ejecuta desde `app/`:

```bash
composer test
```

## El problema

Un formulario sólo produce texto. La agenda, en cambio, necesita saber que una duración es un entero válido, que cliente/servicio no están vacíos y que un intervalo tiene inicio y fin coherentes.

## Concepto

[`Appointment`](../app/src/Domain/Appointment.php) usa una `readonly class` y propiedades tipadas. Una instancia válida no cambia silenciosamente después de construirse.

Su constructor protege reglas pequeñas y locales; `endsAt()` deriva el final y `overlaps()` expresa la matemática del traslape: A empieza antes de que B termine y B empieza antes de que A termine. Por eso dos citas adyacentes —10:00–11:00 y 11:00–11:30— no se cruzan.

## Demostración

[DEMO] Abre [`../app/tests/AppointmentTest.php`](../app/tests/AppointmentTest.php) y ejecuta PHPUnit. Compara el caso adyacente con 10:00–11:00 vs 10:30–11:30.

## Código real

Observa estas decisiones:

- `DateTimeImmutable` evita mutaciones accidentales del inicio;
- `DomainException` comunica entrada que viola una regla;
- `toArray()`/`fromArray()` hacen explícita la forma persistida;
- PHPDoc documenta la superficie pública y la forma de los arrays.

No hay HTML ni filesystem dentro de `Appointment`.

## Qué acaba de pasar

Convertimos valores externos en un objeto con significado. A partir de aquí las capas exteriores pueden cambiar sin reescribir la regla temporal.

## Errores comunes

- Representar todo como strings “porque viene del formulario”.
- Guardar fecha y hora en variables separadas sin una razón.
- Usar `DateTime` mutable y modificar accidentalmente el inicio al calcular el fin.
- Validar únicamente en el navegador.

## Buenas prácticas

Haz imposible representar ciertos estados inválidos. Usa excepciones de dominio para errores esperables de reglas y reserva errores de infraestructura para I/O.

## Tu turno

[PAUSA PARA EJERCICIO] Añade una prueba que demuestre que un nombre de cliente formado sólo por espacios se rechaza.

## Cómo comprobar

```bash
composer test
```

La prueba nueva debe fallar si eliminas la validación correspondiente del constructor.

## Solución enlazada

Una solución posible usa `expectException(DomainException::class)` y construye `Appointment` con `'   '` como cliente. Intenta primero escribirla sin copiar.

## Reto adicional

Explica qué cambiaría si una cita pudiera ocupar varios recursos en paralelo. No implementes todavía salas, empleados ni calendarios múltiples.

## Resumen

- Los tipos dan forma; las invariantes dan significado.
- `readonly` y `DateTimeImmutable` reducen mutación accidental.
- La regla de traslape puede probarse sin HTTP.

## Siguiente paso

Continúa con [Lección 03 — Formulario, POST y persistencia JSON](03-formulario-post-y-persistencia.md).

## Referencias

- [Classes and Objects — PHP manual](https://www.php.net/manual/en/language.oop5.php)
- [Readonly classes — PHP manual](https://www.php.net/manual/en/language.oop5.basic.php#language.oop5.basic.class.readonly)
- [DateTimeImmutable — PHP manual](https://www.php.net/manual/en/class.datetimeimmutable.php)
