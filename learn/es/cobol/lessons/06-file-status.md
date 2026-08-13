# Lección 06 — FILE STATUS y fallos explícitos

## Qué vas a conseguir

Harás que NominaBatch distinga un fin de archivo normal de un fallo real de entrada o salida y devuelva códigos operativos que un script o scheduler pueda interpretar.

## Antes de empezar

Completa la [Lección 05](05-copybooks.md) y ejecuta `bash tests/smoke.sh` desde `app/`.

## El problema

Un batch que sólo imprime “algo salió mal” no es operable. En producción, abrir un archivo inexistente, perder acceso durante una lectura o fallar al escribir el reporte son incidentes distintos. Si todos terminan igual, quien opera el proceso no sabe qué corregir ni qué automatización debe reaccionar.

## Concepto

COBOL permite asociar un campo de dos caracteres con `FILE STATUS`. Después de `OPEN`, `READ`, `WRITE` o `CLOSE`, ese campo expresa el resultado de la operación. En NominaBatch usamos un status separado para entrada y otro para salida.

El código `00` representa éxito. En una lectura secuencial, `10` representa fin de archivo; no es un error de negocio. Otros valores deben tratarse como fallo explícito en vez de continuar como si nada hubiera ocurrido.

## Demostración

[EN PANTALLA] Localiza en [`../app/src/nomina.cob`](../app/src/nomina.cob):

```text
FILE STATUS IS WS-EMPLOYEE-STATUS
FILE STATUS IS WS-REPORT-STATUS
```

Después sigue los párrafos `FAIL-INPUT-OPEN`, `FAIL-REPORT-OPEN`, `FAIL-INPUT-READ` y `ENSURE-REPORT-WRITE`.

## Código real

NominaBatch asigna códigos de retorno diferentes:

- `2`: no pudo abrir la entrada;
- `3`: no pudo abrir el reporte;
- `4`: falló una escritura del reporte;
- `5`: falló una lectura que no era EOF.

Esto mantiene el mensaje para una persona y, al mismo tiempo, entrega una señal numérica para automatización.

## Qué acaba de pasar

El I/O dejó de ser una suposición implícita. Cada frontera puede fallar y el programa conserva suficiente contexto para saber cuál falló.

## Errores comunes

- tratar `10` como error cuando sólo significa EOF;
- reutilizar un único status para varios archivos y perder contexto;
- comprobar el status sólo al abrir, pero ignorarlo durante lectura/escritura;
- imprimir un error y continuar generando resultados parciales como si fueran completos;
- devolver siempre cero aunque el batch no haya producido una salida confiable.

## Buenas prácticas

Mantén los fallos de I/O en párrafos de borde y las reglas de nómina en párrafos deterministas. Un error operativo no debe convertirse en una condición de negocio falsa.

Para investigar un status que no conoces, consulta el manual de tu compilador y registra el valor exacto observado; no inventes su significado.

## Tu turno

Describe qué debería hacer un scheduler ante cada `RETURN-CODE` actual. ¿Qué códigos ameritan reintento, corrección de datos o intervención de infraestructura?

## Cómo comprobar

Desde `app/`:

```text
cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob
bash tests/smoke.sh
```

El smoke debe terminar en `NominaBatch smoke: OK` y el proceso normal debe devolver cero.

## Solución enlazada

Esta lección modifica directamente la aplicación canónica; usa el código actual como referencia y conserva el checkpoint para después de la lección 08.

## Reto adicional

Diseña, sin implementarlo todavía, un mensaje de error estructurado que incluya operación, nombre lógico del archivo y status sin exponer rutas sensibles de un ambiente real.

## Resumen

`FILE STATUS` convierte fallos de archivos en estados observables. EOF es parte del flujo normal; otros fallos deben detener un batch que ya no puede garantizar su salida.

## Siguiente paso

Continúa con la [Lección 07 — Totales de control y reconciliación](07-totales-control.md).

## Referencias

- [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/guides.html)
- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
