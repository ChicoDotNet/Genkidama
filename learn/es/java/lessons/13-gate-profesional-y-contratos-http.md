# Lección 13 — Gate profesional y contratos HTTP

## Qué vas a conseguir

Vas a convertir `mvn verify` en un contrato profesional de entrega y endurecer la frontera HTTP para que las mutaciones acepten únicamente JSON explícito y acotado.

## Antes de empezar

Completa la [Lección 12](12-operacion-confiable-y-checkpoint.md) y confirma `mvn verify` verde.

## El problema

Aceptar cualquier `Content-Type` o un body sin límite hace que una API pequeña dependa de supuestos invisibles. Además, “compila en mi máquina” no sirve como criterio de entrega.

## Concepto

Un gate profesional debe ser repetible. En HelpDesk la unidad mínima sigue siendo:

```bash
mvn verify
```

Ese comando compila, ejecuta JUnit y falla si un contrato protegido deja de cumplirse. El servidor complementa ese gate validando el transporte antes de invocar el dominio.

## Demostración

[DEMO] Envía un `POST /api/tickets` con `Content-Type: text/plain`: debe responder `415`. Envía después JSON mayor a 64 KiB: debe responder `413`.

## Código real

`HelpDeskHttpServer` lee como máximo `MAX_JSON_BODY_BYTES + 1`. Si aparece el byte adicional, rechaza la petición antes de crear o persistir un ticket.

## Qué acaba de pasar

La regla de negocio no cambió. Cambió la calidad de la frontera: entradas incompatibles dejan de llegar accidentalmente al dominio.

## Errores comunes

- Usar el parser JSON como sustituto de validar el media type.
- Leer un body ilimitado y validar tamaño después.
- Llamar “CI” a un comando que nadie puede ejecutar localmente.
- Desactivar una prueba porque el gate detectó un contrato roto.

## Buenas prácticas

Haz que el comando local y CI ejecuten la misma intención. Rechaza temprano y conserva mensajes accionables.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega una regresión para `PUT /priority` con media type incorrecto y demuestra que la prioridad original no cambia.

## Cómo comprobar

```bash
mvn verify
```

## Solución enlazada

Compara tu resultado con la suite HTTP sólo después de intentarlo.

## Reto adicional

Explica cuándo un límite global de body sería insuficiente y por qué distintos endpoints pueden requerir límites distintos.

## Resumen

- `mvn verify` es el gate local reproducible.
- El transporte valida antes del dominio.
- `415` expresa formato no soportado.
- `413` limita consumo accidental o abusivo.

## Siguiente paso

Continúa con [Lección 14 — Debugging desde evidencia](14-debugging-desde-evidencia.md).

## Referencias

- [Apache Maven Lifecycle](https://maven.apache.org/guides/introduction/introduction-to-the-lifecycle.html)
- [HttpExchange — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/com/sun/net/httpserver/HttpExchange.html)
