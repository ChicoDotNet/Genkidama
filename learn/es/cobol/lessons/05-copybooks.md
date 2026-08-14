# Lección 05 — Copybooks y contratos de datos

## Qué vas a conseguir
Separarás el layout reutilizable de NominaBatch en un copybook y conservarás las reglas e I/O en el programa principal.

## Antes de empezar
Completa la [Lección 04](04-archivos-procedimientos-y-checkpoint.md) y ejecuta `bash tests/smoke.sh`.

## El problema
Cuando varios programas comparten la misma forma de datos, duplicar campos `PIC` crea contratos que pueden divergir.

## Concepto
`COPY` incorpora un copybook durante la compilación. Es apropiado para layouts estables y no necesita contener comportamiento ni acceso a archivos.

## Demostración
[EN PANTALLA] Revisa [`../app/copybooks/payroll-data.cpy`](../app/copybooks/payroll-data.cpy) y después el `COPY` de [`../app/src/nomina.cob`](../app/src/nomina.cob).

## Código real
El copybook contiene texto parseado, valores numéricos y formatos de salida. `nomina.cob` conserva validación, cálculo, archivos y reporte.

## Qué acaba de pasar
El contrato reutilizable quedó separado del flujo batch sin cambiar el resultado funcional.

## Errores comunes
- usar copybooks como contenedores de responsabilidades no relacionadas;
- cambiar una `PIC` sin revisar rango y precisión;
- duplicar layouts que deberían tener una fuente común.

## Buenas prácticas
Mantén los contratos pequeños, explícitos y sin I/O oculto.

## Tu turno
Explica en tu copia por qué `WS-DEDUCTION-PCT` reserva posiciones enteras y decimales distintas.

## Cómo comprobar
Compila con `-Wall` y ejecuta el smoke existente.

## Reto adicional
Identifica otro layout que sólo extraerías si apareciera un segundo consumidor real.

## Resumen
`COPY` permite reutilizar contratos; la reutilización no debe esconder comportamiento.

## Siguiente paso
Continúa con la [Lección 06 — FILE STATUS y fallos explícitos](06-file-status.md).

## Referencias
- [Guías de GnuCOBOL](https://gnucobol.sourceforge.io/guides.html)
