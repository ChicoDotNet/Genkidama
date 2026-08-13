# Checkpoint 01 — Detecta facturas duplicadas

Un mismo `invoice_id` repetido puede inflar totales y volver ambigua una conciliación.

## Trabajo

Extiende el parsing para que, dentro de un mismo CSV:

- la primera aparición de un `invoice_id` válido pueda continuar;
- una aparición posterior del mismo identificador se marque como `ValidationIssue` en el campo `invoice_id`;
- la fila duplicada no entre a la conciliación;
- la comparación sea sensible sólo al valor del identificador ya normalizado con `strip()`;
- el comportamiento quede cubierto por al menos una prueba.

No cambies el formato del CSV y no introduzcas una base de datos todavía.

## Restricciones

Mantén la responsabilidad en la frontera de entrada. `reconcile` debería seguir recibiendo registros ya aceptados y no encargarse de descubrir duplicados del archivo.

## Cómo comprobar

Con dos filas cuyo `invoice_id` sea `F-1`, el resultado debe conservar una sola como registro y reportar la otra como inválida.

Ejecuta:

```bash
python -m pytest
```

Después compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).
