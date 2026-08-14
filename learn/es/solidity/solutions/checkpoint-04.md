# Solución de referencia — Checkpoint 04

> Ábrela sólo después de intentar el checkpoint.

Una solución razonable usa un contrato cliente que despliega FreelanceEscrow para convertirse realmente en `client`. Su `receive()` intenta una segunda llamada de bajo nivel a `refund()` y conserva el booleano de éxito sin revertir el primer pago.

La prueba de referencia vive en [`../app/test/Security.t.sol`](../app/test/Security.t.sol). Comprueba:

```text
estado final          = Refunded
saldo del escrow      = 0
saldo del cliente     = depósito original
reentrada tuvo éxito  = false
```

La defensa importante ya existe en el flujo de producción: `refund()` valida `Funded`, cambia el estado a `Refunded` y sólo después ejecuta la interacción externa. La segunda llamada ocurre cuando el estado dejó de ser `Funded`, por lo que revierte.

No es necesario introducir un modifier de reentrada si el comportamiento actual satisface esta propiedad y no existe otra ruta vulnerable que lo justifique. Agregar controles por nombre, sin un threat model, aumenta complejidad sin demostrar seguridad adicional.

## Qué no demuestra la prueba

No cubre todos los riesgos de un contrato que custodia valor. No demuestra ausencia de problemas económicos, errores de autorización futuros, riesgos de upgrades, manipulación de oráculos —si algún día existieran—, front-running ni seguridad de las claves de los participantes.

## Verificación

```bash
forge test --match-path app/test/Security.t.sol -vvvv
bash tools/verify.sh
```

La solución correcta se evalúa por la propiedad protegida y la explicación de la frontera externa, no por copiar exactamente los nombres de esta referencia.
