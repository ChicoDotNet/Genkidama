# Lección 17 — Evaluación final COBOL sin receta

## Qué vas a conseguir

Demostrarás que puedes leer, modificar, probar y explicar NominaBatch sin seguir un tutorial paso a paso. Esta lección integra el curso: no introduce una sintaxis principal nueva.

## Antes de empezar

Completa la [Lección 16](16-operacion-confiable-y-checkpoint-04.md) y ejecuta desde `app/`:

```bash
bash tools/verify.sh
```

Confirma que entiendes el flujo completo: archivo de entrada → parsing → validación → control de duplicados → cálculo decimal → acumuladores → bandas → reporte → códigos de retorno.

## El problema

Un equipo mantiene un procesador batch de nómina y recibe una solicitud pequeña de negocio junto con un defecto de integridad. El cambio debe conservar los contratos existentes, producir evidencia reproducible y poder explicarse a otra persona. No recibirás una lista de párrafos o líneas que debas editar.

## Concepto

Una tarea junior profesional no se reduce a escribir sintaxis. El ciclo esperado es **leer → formular una hipótesis → reproducir → probar → implementar → verificar → explicar**.

En COBOL esto incluye comprender contratos de datos, precisión numérica, orden de actualización de acumuladores, `FILE STATUS`, códigos de retorno y límites explícitos de estructuras `OCCURS`.

## Demostración

[DEMO] Antes de cambiar nada, recorre `src/nomina.cob`, `copybooks/payroll-data.cpy`, `tests/smoke.sh` y `tests/operational.sh`. Explica en voz alta qué partes son I/O, qué partes representan reglas deterministas y qué invariantes protegen los tests.

No escribas código todavía.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve las historias sobre la misma aplicación canónica.

Puedes consultar el manual oficial de GnuCOBOL y las lecciones anteriores. No abras la solución de referencia antes de completar un intento.

## Qué acaba de pasar

Ya no estás reproduciendo una receta. Estás modificando una base existente bajo contratos de negocio y operación, que es una habilidad más representativa del mantenimiento real que memorizar cláusulas aisladas.

## Errores comunes

- agregar una regla después de actualizar totales y contaminar el lote con un registro que debía rechazarse;
- usar `PIC` con precisión insuficiente para la nueva regla;
- corregir el síntoma sin una regresión que falle antes del arreglo;
- romper el contrato de retorno o `FILE STATUS` para conseguir un caso feliz;
- optimizar la búsqueda de IDs sin medir ni respetar el límite de 100 registros;
- afirmar experiencia mainframe que NominaBatch no demuestra.

## Buenas prácticas

Haz cambios pequeños y comprobables. Mantén parsing, validación, cálculo y I/O reconocibles. Los errores deben ser explícitos y los acumuladores sólo deben modificarse después de que un registro sea aceptado completamente.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–E de la evaluación final. Entrega código, pruebas, evidencia del gate, una nota de documentación oficial y una explicación de diseño.

## Cómo comprobar

Como mínimo:

```bash
bash tools/verify.sh
```

Después ejecuta manualmente un lote válido y uno que contenga el caso de regresión solicitado. Verifica reporte, conteos, totales y código de retorno.

Evalúate con la [`rúbrica final`](../exercises/rubrica-final.md).

## Solución enlazada

Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia muestra una dirección posible; no exige que tus párrafos ni nombres sean idénticos.

## Reto adicional

Describe qué cambiaría si el archivo de entrada creciera de decenas a millones de registros. Separa qué medirías primero, qué estructuras dejarían de ser razonables y qué contratos conservarías.

## Cómo hablar de este proyecto en una entrevista

Empieza por el problema: procesamiento batch reproducible con reglas monetarias y datos potencialmente inválidos. Después explica decisiones: `PIC` decimal explícito, `FILE STATUS`, copybook como contrato, rechazo antes de acumuladores, tablas `OCCURS` acotadas, reconciliación de totales y tests de camino feliz/fallos operativos.

Preguntas probables:

- ¿Por qué un registro inválido no debe tocar ningún acumulador?
- ¿Qué aporta `FILE STATUS` frente a asumir que `OPEN` funcionó?
- ¿Qué diferencia existe entre `PIC 9(5)V99` y un decimal binario típico?
- ¿Por qué la tabla de IDs tiene un límite y cuándo la cambiarías?
- ¿Cómo diagnosticarías un lote cuyo total por bandas no coincide con el total global?
- ¿Qué demuestra este proyecto y qué no demuestra sobre un entorno mainframe real?

## Resumen

Completar el curso significa poder evolucionar NominaBatch con evidencia y explicar por qué el cambio es correcto. La evaluación aporta señal de preparación inicial; no predice contratación ni sustituye experiencia productiva.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva el proyecto como evidencia y crea una variante propia del lote sin copiar la solución.

## Referencias

- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [GnuCOBOL Guides](https://gnucobol.sourceforge.io/guides.html)
