# Lección 16 — Hardening y Checkpoint 04

## Qué vas a conseguir

Vas a cerrar el bloque profesional protegiendo límites HTTP y respuestas defensivas sin fingir que unos headers convierten por sí solos una API en “segura”.

## Antes de empezar

Completa la [Lección 15](15-medir-antes-de-optimizar.md).

## El problema

Una API puede tener reglas correctas y aun así aceptar entradas innecesariamente amplias o emitir respuestas ambiguas para clientes y navegadores.

## Concepto

Hardening es reducir superficie y ambigüedad. HelpDesk aplica controles pequeños y verificables:

- `Content-Type: application/json` en mutaciones que leen JSON;
- body máximo de 64 KiB;
- `X-Content-Type-Options: nosniff`;
- `Referrer-Policy: no-referrer`;
- CSP restrictiva para sus respuestas JSON.

Estos controles complementan validación, reglas y despliegue; no sustituyen autenticación, TLS, autorización ni revisión de amenazas.

## Demostración

[DEMO] Ejecuta `HelpDeskHttpServerTest`. Observa que media type incorrecto y body sobredimensionado dejan el listado vacío. Después inspecciona los headers de `/health`.

## Código real

Los headers se agregan en `send`, una única frontera de salida. El límite se aplica antes de deserializar, por lo que Jackson nunca necesita materializar un payload arbitrariamente grande.

## Qué acaba de pasar

Los controles viven en la capa que posee el riesgo. El dominio permanece centrado en tickets.

## Errores comunes

- Llamar “segura” a la aplicación sólo por tener CSP.
- Añadir autenticación ficticia con secretos hard-coded.
- Validar tamaño después de leer todo el body.
- Registrar payloads inválidos completos.
- Responder `400` para todos los problemas de protocolo.

## Buenas prácticas

Usa códigos específicos, límites documentados y defaults restrictivos. Para producción real agrega TLS, identidad, autorización, rate limiting y gestión de secretos según el contexto.

## Tu turno — Checkpoint 04

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar

```bash
mvn verify
```

Comprueba además manualmente un `POST` con `text/plain`, un payload mayor al límite y los headers defensivos.

## Solución enlazada

Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) sólo después de completar tu intento.

## Reto adicional

Haz un mini threat model: enumera tres riesgos que estos controles sí reducen y tres que permanecen fuera de alcance.

## Resumen

- Hardening reduce superficie; no reemplaza un modelo de seguridad completo.
- Los límites se aplican antes de parsear.
- Los headers se centralizan en la frontera de salida.
- Las pruebas demuestran que rechazar entrada no muta el dominio.

## Siguiente paso

La próxima lección será la evaluación Junior autónoma sin receta.

## Referencias

- [OWASP HTTP Headers Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/HTTP_Headers_Cheat_Sheet.html)
- [HttpExchange — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/jdk.httpserver/com/sun/net/httpserver/HttpExchange.html)
