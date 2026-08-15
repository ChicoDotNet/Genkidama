# Checkpoint 04 — Invariantes operativas

Trabaja sobre la versión actual de NominaBatch y no abras la solución antes de completar un intento.

## Escenario

El reporte contiene totales globales y cuatro agregados por banda. Hoy las pruebas verifican valores concretos, pero queremos una defensa más fuerte contra una modificación futura que actualice una banda sin actualizar el total global, o viceversa.

## Encargo

Agrega una comprobación explícita de reconciliación al final del lote.

Debe demostrar que:

1. la suma de `WS-BAND-COUNT` coincide con `WS-PROCESSED`;
2. la suma de `WS-BAND-NET` coincide con `WS-TOTAL-NET`;
3. si alguna invariancia falla, el proceso termina con un código no cero distinto de los códigos de apertura/lectura ya existentes;
4. el diagnóstico identifica una falla de reconciliación;
5. el camino normal conserva exactamente el reporte funcional existente.

No se prescribe el nombre del párrafo ni el código de retorno exacto. Documenta la decisión.

## Prueba obligatoria

Incluye una regresión que pueda demostrar el camino de error sin dejar permanentemente incorrecta la aplicación canónica. Puede usar una copia temporal del código/fixture o una técnica equivalente reproducible.

## Entrega

- código modificado;
- prueba nueva;
- comando ejecutado;
- explicación de la invariancia;
- resultado observado para camino normal y camino forzado de error.

## Comprobación mínima

```bash
cd app
bash tools/verify.sh
```

Después ejecuta tu escenario específico de reconciliación rota y confirma código no cero.

Cuando termines, compara tu criterio con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).
