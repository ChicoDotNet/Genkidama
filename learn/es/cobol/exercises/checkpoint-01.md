# Checkpoint 01 — Agrega pago de horas extra

Extiende NominaBatch sin abrir todavía la solución.

## Requisito

La política nueva es:

- hasta 40 horas se pagan a tarifa normal;
- las horas por encima de 40 y hasta el máximo ya permitido se pagan a **1.5×** la tarifa normal;
- la deducción porcentual se calcula sobre el bruto total;
- un registro inválido sigue rechazándose antes del cálculo;
- el reporte mantiene las mismas columnas públicas actuales.

## Restricciones

- No dupliques parsing ni validación.
- No conviertas la regla en lógica de texto dentro del reporte.
- Usa aritmética decimal explícita y define dónde redondeas.
- Conserva el comportamiento para empleados de 40 horas o menos.

## Fixture mínimo sugerido

Agrega temporalmente un registro ficticio equivalente a 45 horas con tarifa `100.00` y deducción `10.00`.

El bruto esperado es:

```text
40 × 100.00 + 5 × 150.00 = 4750.00
```

La deducción esperada es `475.00` y el neto `4275.00`.

## Evidencia requerida

1. `cobc -x -free -Wall` compila sin convertir warnings en deuda ignorada.
2. El smoke existente sigue pasando.
3. Añades una comprobación del caso de 45 horas.
4. Explica en dos o tres frases en qué párrafo colocaste la regla y por qué.

## Después del intento

Consulta la [solución de referencia](../solutions/checkpoint-01.md).
