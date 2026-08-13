# Checkpoint 03 — Propiedades de reembolso

Trabaja sobre la aplicación después de la Lección 12. Intenta el ejercicio antes de abrir la solución.

## Historia
Fortalece las pruebas de `refund()` sin repetir los casos existentes.

## Objetivos
- Verifica con fuzzing que un reembolso válido termina en `Refunded` y deja saldo cero.
- Verifica con distintos callers que sólo el cliente puede pedir el reembolso.
- Después de un rechazo comprueba que estado y saldo siguen intactos.
- Conserva el gate `bash tools/verify.sh` verde.

## Criterios
No cambies las reglas del contrato para facilitar la prueba. Mantén los ejemplos actuales y explica qué propiedad aporta cada test nuevo.

## Comprobación
```bash
forge test -vv
bash tools/verify.sh
```

Después compara con [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md).
