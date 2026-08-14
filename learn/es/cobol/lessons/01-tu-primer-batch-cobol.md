# Lección 1 — Tu primer batch COBOL

## Qué vas a conseguir
Compilarás y ejecutarás NominaBatch, recorrerás la estructura mínima de un programa COBOL y verás un flujo completo de entrada → proceso → reporte.

## Antes de empezar
Confirma `cobc --version` y sitúate en `learn/es/cobol/app/`.

## El problema
Una nómina batch recibe registros, aplica reglas y produce un resultado auditable. Antes de estudiar sintaxis aislada necesitamos ver ese flujo funcionando de extremo a extremo.

## Concepto
COBOL organiza el programa en divisiones. En NominaBatch verás `IDENTIFICATION`, `ENVIRONMENT`, `DATA` y `PROCEDURE DIVISION`. No memorices todavía cada cláusula: identifica qué describe el programa, qué conecta archivos, qué modela datos y qué ejecuta reglas.

## Demostración
[EJECUTAR]

```text
cobc -x -free -Wall -o nomina src/nomina.cob
./nomina
```

Abre después `report.txt`.

## Código real
En [`../app/src/nomina.cob`](../app/src/nomina.cob), `MAIN` abre el archivo de entrada y el reporte, recorre registros hasta fin de archivo y delega cada registro a `PROCESS-RECORD`.

## Qué acaba de pasar
`cobc` tradujo y compiló el fuente. El ejecutable leyó `data/employees.dat`, produjo líneas válidas, rechazó un registro fuera de política y cerró con un resumen.

## Errores comunes
- ejecutar desde otra carpeta y no encontrar `data/employees.dat`;
- confundir el punto final de COBOL con puntuación decorativa;
- modificar muchos párrafos a la vez antes de volver a compilar;
- asumir que un batch sin interfaz es un programa de juguete.

## Buenas prácticas
Compila con advertencias, trabaja con entradas reproducibles y conserva una salida que pueda compararse.

## Tu turno
Cambia temporalmente el nombre `Empleado Uno` por otro texto ficticio, recompila y confirma que el reporte refleja sólo ese cambio. Revierte después el fixture.

## Cómo comprobar
El ejecutable termina sin error y `report.txt` contiene dos procesados, un rechazado y un resumen.

## Solución enlazada
No hay código nuevo obligatorio en esta lección; el objetivo es ejecutar y leer la aplicación canónica.

## Reto adicional
Localiza qué párrafo escribe el encabezado y explica por qué no debería encargarse también del cálculo.

## Resumen
- COBOL separa descripción, datos y procedimiento;
- NominaBatch ya es un flujo batch completo;
- compilar frecuentemente reduce el espacio de búsqueda de errores.

## Siguiente paso
Continúa con [Lección 2 — Datos `PIC` y registros](02-datos-pic-y-registros.md).

## Referencias
- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [Manual de GnuCOBOL](https://gnucobol.sourceforge.io/doc/gnucobol.html)
