# Lección 4 — Archivos, procedimientos y checkpoint 01

## Qué vas a conseguir
Entenderás cómo NominaBatch modela archivos secuenciales, recorre registros con `READ` y organiza responsabilidades mediante párrafos ejecutados con `PERFORM`. Después resolverás el primer checkpoint.

## Antes de empezar
Ubica `FILE-CONTROL`, `FILE SECTION` y los párrafos de `PROCEDURE DIVISION` en [`../app/src/nomina.cob`](../app/src/nomina.cob).

## El problema
Un batch real necesita controlar ciclo de vida de archivos, fin de archivo y responsabilidades. Si parsing, cálculo, reporting y apertura/cierre viven mezclados en un bloque enorme, cualquier nueva regla aumenta el riesgo.

## Concepto
`SELECT` conecta un nombre lógico con un archivo. `FD` describe el registro físico. `OPEN`, `READ`, `WRITE` y `CLOSE` forman la frontera de I/O.

NominaBatch usa una condición `88 END-OF-FILE` para expresar el estado del ciclo y párrafos como `PROCESS-RECORD`, `VALIDATE-AND-CALCULATE` y `WRITE-REJECTION` para separar intención.

## Demostración
[EN PANTALLA] Sigue una sola línea del fixture desde `READ` hasta una línea del reporte. Identifica exactamente dónde cambia de texto crudo a dato validado y dónde ocurre I/O.

## Código real
El `PERFORM UNTIL END-OF-FILE` contiene el ciclo, pero delega el contenido del registro. Esa separación permitirá incorporar nuevas reglas sin convertir `MAIN` en una lista interminable de condiciones.

## Qué acaba de pasar
Ya tienes cuatro piezas transferibles de COBOL profesional: estructura, datos explícitos, reglas deterministas y archivos batch.

## Errores comunes
- olvidar cerrar archivos;
- usar un párrafo como contenedor de responsabilidades no relacionadas;
- depender de efectos laterales sin nombres que los hagan visibles;
- modificar `MAIN` para cada nueva regla de nómina.

## Buenas prácticas
Mantén el flujo principal legible como historia de alto nivel y mueve decisiones detalladas a párrafos con una responsabilidad clara.

## Tu turno — Checkpoint 01
Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución. Agregarás pago de horas extra conservando validación y reporte.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Compila con `-Wall`, ejecuta el smoke existente y agrega al menos una comprobación nueva para un empleado ficticio con horas extra.

## Solución enlazada
Sólo después de tu intento consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).

## Reto adicional
¿Qué cambiarías si el archivo de entrada tuviera millones de registros? Explica por qué el enfoque secuencial actual ya tiene una ventaja importante.

## Resumen
- `FILE-CONTROL` y `FD` describen el contrato de archivo;
- `READ`/`WRITE` son fronteras de I/O;
- `PERFORM` permite mantener un flujo principal legible;
- checkpoint 01 modifica una regla real sin reescribir el programa.

## Siguiente paso
Continúa con la [Lección 05 — Copybooks y contratos de datos](05-copybooks.md).

## Referencias
- [Manual de GnuCOBOL](https://gnucobol.sourceforge.io/doc/gnucobol.html)
