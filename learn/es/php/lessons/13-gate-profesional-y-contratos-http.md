# Lección 13 — Gate profesional y contratos HTTP

## Qué vas a conseguir

Vas a convertir los comandos que ya usabas por separado en un gate repetible y a tratar el formato de una mutación HTTP como un contrato explícito.

## Antes de empezar

Completa la [Lección 12](12-fallos-operativos-y-checkpoint-03.md).

## El problema

Una aplicación puede “funcionar en mi máquina” y aun así romperse por sintaxis, dependencias, pruebas o un smoke HTTP que nadie volvió a ejecutar. También puede aceptar accidentalmente formatos de request que nunca diseñaste.

## Concepto

Un gate profesional pequeño responde siempre las mismas preguntas: ¿Composer está sano?, ¿el PHP parsea?, ¿las pruebas pasan?, ¿la aplicación real arranca y recorre su flujo crítico? AgendaPHP concentra esa respuesta en [`tools/verify.sh`](../app/tools/verify.sh) y mantiene el smoke HTTP como evidencia de integración.

En la frontera web hacemos además explícito que las mutaciones de este curso usan `application/x-www-form-urlencoded`. Un POST con otro media type no se interpreta “como se pueda”: responde 415 antes de modificar estado.

## Demostración

[EJECUTAR]

```bash
cd app
bash tools/verify.sh
bash tools/smoke.sh
```

Después envía un POST con `Content-Type: application/json`. El smoke espera **415 Unsupported Media Type** y comprueba que no aparece una cita nueva.

## Código real

[`public/index.php`](../app/public/index.php) normaliza `CONTENT_TYPE` y valida el contrato antes de llamar a `AppointmentService`. El dominio no sabe nada de media types.

El workflow [`learn-php.yml`](../../../../.github/workflows/learn-php.yml) instala PHP/Composer y ejecuta los mismos gates en un runner limpio.

## Qué acaba de pasar

No agregamos un framework para conseguir disciplina. Hicimos visible el contrato real que ya tenía la interfaz y evitamos que entradas no diseñadas lleguen al dominio.

## Errores comunes

- Ejecutar sólo PHPUnit y olvidar que el proceso HTTP puede fallar.
- Aceptar cualquier `Content-Type` y confiar en coerciones implícitas.
- Introducir un linter pesado sin una señal concreta de riesgo.
- Cambiar el gate para que el rojo desaparezca en lugar de corregir la causa.

## Buenas prácticas

Un gate debe ser corto, determinista y útil para una persona y para CI. Agrega herramientas sólo si detectan una clase real de defectos que hoy no cubres.

## Tu turno

Provoca un fallo de sintaxis en una copia local y observa qué paso del gate falla primero. Revierte el cambio y confirma verde nuevamente.

## Cómo comprobar

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

## Solución enlazada

No hay una solución única: el objetivo es interpretar el gate y reparar la causa sin desactivarlo.

## Reto adicional

Explica cuándo tendría sentido aceptar `application/json` además de formularios y qué tests agregarías antes de ampliar ese contrato.

## Resumen

- El gate profesional reúne comprobaciones repetibles.
- El smoke prueba el proceso HTTP real, no sólo clases aisladas.
- Los formatos de mutación son contratos, no sugerencias.
- Un 415 temprano evita ambigüedad y mutaciones accidentales.

## Siguiente paso

Continúa con [Lección 14 — Debugging desde evidencia](14-debugging-desde-evidencia.md).

## Referencias

- [Composer — validate](https://getcomposer.org/doc/03-cli.md#validate)
- [PHPUnit](https://phpunit.de/)
- [415 Unsupported Media Type — HTTP Semantics](https://www.rfc-editor.org/rfc/rfc9110.html#name-415-unsupported-media-type)
