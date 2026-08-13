# Checkpoint 03 — Reconciliar agregados por banda

Trabaja sobre NominaBatch sin abrir la solución.

## Objetivo

Amplía cada elemento de `WS-DEDUCTION-BANDS` para acumular también **bruto por banda** y demuestra que los agregados conservan las mismas reglas que los totales globales.

## Requisitos

1. Cada banda conserva nombre, empleados, neto y ahora bruto.
2. Un registro aceptado incrementa exactamente una banda.
3. Un registro rechazado —incluido un ID duplicado— no modifica ninguna banda.
4. El reporte de cada banda agrega `BRUTO=<importe>` sin eliminar los campos existentes.
5. La suma del bruto de las cuatro bandas debe coincidir con el `BRUTO` del `RESUMEN` para el fixture canónico.
6. Actualiza `tests/smoke.sh` con expectativas que detecten si el bruto se acumula en una banda equivocada o si un duplicado contamina los agregados.

No cambies parsing, reglas de horas/tarifa/deducción ni la política de duplicados para hacer pasar la prueba.

## Evidencia

Desde `app/`:

```text
bash tests/smoke.sh
```

Entrega una explicación breve de por qué colocaste la acumulación en el punto elegido del flujo.
