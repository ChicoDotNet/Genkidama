# Lección 08 — Transferencias, atomicidad y checkpoint 02

## Qué vas a conseguir
Vas a probar qué ocurre cuando el receptor de un pago rechaza Ether. Al terminar entenderás por qué FreelanceEscrow cambia estado antes de la interacción externa y por qué un revert posterior restaura estado y fondos de toda la transacción.

## Antes de empezar
Completa la [Lección 07](07-interfaces-explicitas-y-compatibilidad.md) y ejecuta `bash tools/verify.sh`.

## El problema
`release()` envía Ether a una dirección externa. Ese receptor puede ejecutar código y también puede rechazar el pago. Un camino feliz no basta para demostrar que el escrow conserva su invariantes ante ese fallo.

## Concepto
FreelanceEscrow sigue una secuencia equivalente a checks → effects → interactions:

1. valida caller y estado;
2. mueve el estado a `Released`;
3. calcula el saldo;
4. ejecuta la llamada externa;
5. revierte con `TransferFailed` si la transferencia falla.

Aunque el efecto ocurre antes de la interacción, un revert posterior revierte la transacción completa. Por eso un pago rechazado no debe dejar el estado en `Released` ni perder los fondos del escrow.

## Demostración
[EN PANTALLA]
```bash
forge test --match-test testReleaseFailureRevertsStateAndRetainsFunds -vv
```

La prueba usa un freelancer contractual cuyo `receive()` siempre revierte. Después del intento fallido comprueba dos invariantes: el estado sigue `Delivered` y el depósito permanece en el escrow.

## Código real
La prueba negativa vive en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). No cambia la regla de negocio para facilitar el test; crea un colaborador hostil controlado y observa el comportamiento real.

## Qué acaba de pasar
Probaste una frontera externa, no sólo una función interna. El escrow demuestra que un receptor que falla no convierte el acuerdo en un estado terminal falso.

## Errores comunes
- Probar sólo receptores EOA que siempre aceptan el pago.
- Suponer que `call` siempre termina correctamente.
- Ignorar el valor booleano devuelto por una llamada de bajo nivel.
- Cambiar el estado después de la llamada externa sin analizar reentrancia.
- Creer que un `revert` sólo revierte la última línea ejecutada.

## Buenas prácticas
Prueba caminos de fallo en fronteras externas. Mantén las invariantes visibles y usa errores explícitos. Un test educativo no sustituye threat modeling ni auditoría para dinero real.

## Tu turno — Checkpoint 02
[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-02.md`](../exercises/checkpoint-02.md) sin abrir la solución.

## Cómo comprobar
Ejecuta:
```bash
bash tools/verify.sh
```
Además explica por qué `state = Released` no queda persistido cuando la transferencia revierte.

## Solución enlazada
Consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md) sólo después de completar tu intento.

## Reto adicional
Analiza qué riesgo aparecería si `release()` hiciera una llamada externa antes de cambiar el estado. No cambies el contrato hasta poder explicar el escenario.

## Resumen
- una transferencia es una frontera externa;
- `call` puede fallar;
- un revert restaura los efectos de toda la transacción;
- las invariantes deben probarse también en caminos hostiles;
- checks-effects-interactions reduce superficie de reentrancia, pero no sustituye revisión de seguridad.

## Siguiente paso
El siguiente bloque profundizará en composición, propiedades e invariantes antes de entrar a tooling y hardening final.

## Referencias
- [Security Considerations — Reentrancy](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html#reentrancy)
- [Sending and Receiving Ether](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html#sending-and-receiving-ether)
- [Expressions and Control Structures — error handling](https://docs.soliditylang.org/en/v0.8.35/control-structures.html#error-handling-assert-require-revert-and-exceptions)
