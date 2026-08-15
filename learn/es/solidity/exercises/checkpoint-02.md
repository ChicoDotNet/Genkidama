# Checkpoint 02 — Falla de pago sin corromper el escrow

Trabaja sobre la aplicación canónica y no abras la solución antes de intentarlo.

## Escenario
El freelancer puede ser un contrato y su `receive()` puede rechazar Ether. Debes demostrar que `release()` no deja el escrow en un estado terminal falso cuando el pago falla.

## Tu tarea
1. Crea un receptor de prueba que pueda marcar la entrega y que rechace Ether al recibirlo.
2. Despliega FreelanceEscrow usando ese receptor como freelancer.
3. Marca la entrega desde la identidad correcta.
4. Haz que el cliente intente `release()`.
5. Comprueba el error `TransferFailed`.
6. Después del revert, comprueba que el estado continúa `Delivered` y que el contrato conserva el depósito completo.

No cambies `FreelanceEscrow` sólo para facilitar el test.

## Evidencia
Ejecuta:

```bash
bash tools/verify.sh
```

Tu prueba debe fallar si el contrato pierde fondos o queda en `Released` después del pago rechazado.

## Reflexión
Explica en tus palabras por qué un cambio de estado ejecutado antes de la llamada externa puede desaparecer si una instrucción posterior revierte la transacción completa.

Cuando termines, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).
