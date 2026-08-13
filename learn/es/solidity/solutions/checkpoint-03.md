# Solución de referencia — Checkpoint 03

Consulta esta referencia después de intentar el ejercicio.

La solución conserva `refund()` y amplía únicamente la suite.

Debe comprobar que:

- distintos depósitos positivos pueden reembolsarse y dejan el escrow en `Refunded` con saldo cero;
- una identidad no autorizada no cambia ni el estado ni el saldo.

Conserva los tests deterministas existentes y ejecuta:

```bash
forge test -vv
bash tools/verify.sh
```

El objetivo es ampliar evidencia, no sustituir ejemplos ni presentar la suite como auditoría.
