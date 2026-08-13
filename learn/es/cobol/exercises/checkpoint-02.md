# Checkpoint 02 — Reconciliación de registros

Extiende NominaBatch sin abrir todavía la solución.

## Objetivo

El resumen actual distingue procesados y rechazados. Agrega un contador independiente de registros leídos para poder comprobar esta relación:

```text
LEIDOS = PROCESADOS + RECHAZADOS
```

## Requisitos

- Incrementa `LEIDOS` una vez por cada registro recibido antes de clasificarlo.
- El evento EOF no incrementa el contador.
- Conserva los contadores de procesados y rechazados.
- Conserva los importes de bruto, deducciones y neto.
- Incluye `LEIDOS` en la línea `RESUMEN`.
- Agrega una comprobación al smoke para los tres conteos.

Con el fixture actual el resultado esperado es:

```text
LEIDOS=8|PROCESADOS=2|RECHAZADOS=6
```

## Criterio de diseño

Mide `LEIDOS` en la frontera de lectura. No lo calcules al final a partir de los otros dos contadores, porque entonces dejaría de ser un control independiente.

## Evidencia requerida

1. Compilación con `cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob`.
2. `bash tests/smoke.sh` en verde.
3. Resumen con `LEIDOS=8`, `PROCESADOS=2` y `RECHAZADOS=6`.
4. Una explicación breve de por qué el contador pertenece al flujo de lectura y no al cálculo de nómina.

## Pregunta de diseño

Si apareciera un tercer resultado `OMITIDO`, ¿cómo cambiaría la relación de control?

## Después del intento

Consulta la [solución de referencia](../solutions/checkpoint-02.md).
