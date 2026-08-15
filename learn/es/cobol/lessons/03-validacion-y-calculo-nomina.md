# Lección 3 — Validación y aritmética decimal

## Qué vas a conseguir
Seguirás la frontera de validación de NominaBatch y entenderás cómo se calculan bruto, deducciones y neto sin aceptar silenciosamente entradas inválidas.

## Antes de empezar
Abre `VALIDATE-AND-CALCULATE` en [`../app/src/nomina.cob`](../app/src/nomina.cob).

## El problema
Una nómina no puede convertir cualquier texto y esperar que el cálculo “salga”. Un dato no numérico, horas imposibles o una deducción mayor a 100% deben detener ese registro y quedar visibles.

## Concepto
NominaBatch usa `FUNCTION TEST-NUMVAL` antes de `FUNCTION NUMVAL`. Después aplica invariantes de negocio: horas entre 1 y 80, tarifa mayor que cero y deducción entre 0 y 100.

Sólo entonces ejecuta:

```text
bruto = horas × tarifa
deducción = bruto × porcentaje / 100
neto = bruto - deducción
```

`ROUNDED` hace explícita la política de redondeo en la deducción.

## Demostración
[DEMO] Observa el registro `E003`: tiene 95 horas. El programa no lo incorpora al total; genera una línea `RECHAZADO` con la razón.

## Código real
Cada error ejecuta `WRITE-REJECTION` y `EXIT PARAGRAPH`. Eso evita continuar hacia conversiones o cálculos posteriores cuando una precondición ya falló.

## Qué acaba de pasar
La validación no es un `IF` decorativo: define qué datos pueden entrar al núcleo de cálculo. El reporte conserva evidencia de los descartes.

## Errores comunes
- llamar `NUMVAL` antes de comprobar que el texto es convertible;
- aceptar valores fuera de política porque “son numéricos”;
- esconder rechazos;
- redondear en lugares distintos y obtener centavos inconsistentes.

## Buenas prácticas
Valida formato antes que rango, falla el registro completo de forma determinista y define el punto exacto de redondeo.

## Tu turno
Cambia temporalmente la deducción de un registro ficticio a `101.00`, ejecuta el batch y verifica que se rechaza. Revierte el fixture al terminar.

## Cómo comprobar
La línea rechazada debe indicar que la deducción debe estar entre 0 y 100, y el resumen debe cambiar de forma coherente.

## Solución enlazada
La implementación canónica ya contiene la regla; tu trabajo es observar y razonar su comportamiento.

## Reto adicional
Explica por qué validar sólo `NUMERIC` sobre una cadena con `250.00` no es equivalente a `TEST-NUMVAL`.

## Resumen
- formato y rango son validaciones distintas;
- convertir después de validar reduce errores;
- el cálculo decimal debe tener política de redondeo;
- los rechazos forman parte del resultado operativo.

## Siguiente paso
Continúa con [Lección 4 — Archivos, procedimientos y checkpoint 01](04-archivos-procedimientos-y-checkpoint.md).

## Referencias
- [TEST-NUMVAL y funciones de GnuCOBOL](https://gnucobol.sourceforge.io/HTML/gnucobqr.html)
- [Manual de GnuCOBOL](https://gnucobol.sourceforge.io/doc/gnucobol.html)
