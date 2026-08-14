# Lección 03 — Liberación, reembolso y transferencia de valor

## Qué vas a conseguir

Vas a entender cómo `FreelanceEscrow` mueve Ether al freelancer o lo devuelve al cliente, por qué el orden **validar → cambiar estado → interactuar** importa y cómo probar saldos sin usar dinero real.

## Antes de empezar

Completa la [Lección 02](02-maquina-de-estados-roles-y-entrega.md). Desde la raíz del curso ejecuta:

```bash
bash tools/verify.sh
```

## El problema

El objetivo del escrow no es sólo registrar estados: debe custodiar y transferir valor. Una transferencia equivocada, duplicada o ejecutada en un estado incorrecto puede convertirse en una pérdida irreversible.

## Concepto

La ruta feliz de pago es:

```text
cliente deposita -> freelancer entrega -> cliente libera -> freelancer recibe saldo
```

El reembolso sólo está permitido mientras el contrato sigue en `Funded`.

Las funciones `release()` y `refund()` siguen una secuencia deliberada:

1. validar quién llama;
2. validar el estado actual;
3. cambiar a un estado terminal;
4. leer el saldo;
5. realizar la llamada externa;
6. revertir todo si la transferencia falla.

Cambiar el estado antes de la interacción externa sigue el principio Checks-Effects-Interactions y reduce superficie para reentradas sobre una operación que sólo debe ejecutarse una vez.

## Demostración

[EJECUTAR]

```bash
forge test --match-test testFreelancerDeliversAndClientReleases -vv
forge test --match-test testClientCanRefundBeforeDelivery -vv
```

La primera prueba observa que el freelancer recibe exactamente el depósito y que el contrato queda vacío. La segunda demuestra que el cliente recupera el depósito antes de la entrega.

## Código real

El pago al freelancer usa una llamada de bajo nivel:

```solidity
uint256 amount = address(this).balance;
(bool sent,) = payable(freelancer).call{value: amount}("");
if (!sent) revert TransferFailed();
```

`call` devuelve un booleano; ignorarlo sería un bug. Si `sent` es falso, `revert TransferFailed()` revierte también el cambio previo de `state`, porque una transacción EVM que revierte deshace sus modificaciones de estado.

La documentación moderna de Solidity desaconseja `send`/`transfer` como mecanismo general de envío de Ether y muestra `call` para transferencias que necesitan propagar gas de forma más flexible.

## Qué acaba de pasar

El contrato ya expresa el núcleo comercial completo: custodia un depósito, reconoce una entrega, paga al freelancer o devuelve al cliente. Las pruebas verifican tanto estado como movimiento de valor.

## Errores comunes

- Enviar Ether antes de marcar la operación como terminada.
- Ignorar el booleano devuelto por `call`.
- Permitir `refund()` después de `Delivered`.
- Medir sólo el estado y no los balances.
- Introducir una función genérica de retiro sin una necesidad concreta.
- Asumir que Checks-Effects-Interactions reemplaza una revisión de seguridad profesional.

## Buenas prácticas

Mantén pequeñas las rutas que mueven valor. Expresa estados terminales antes de interacciones externas, comprueba siempre el resultado de la llamada y protege con pruebas tanto la ruta feliz como los fallos previsibles.

## Tu turno

Agrega una prueba que confirme que el saldo del contrato es cero después de `refund()`. Después explica qué ocurriría con `state` si la llamada de transferencia revirtiera.

## Cómo comprobar

```bash
forge test --match-test testClientCanRefundBeforeDelivery -vv
forge test --match-test testFreelancerDeliversAndClientReleases -vv
bash tools/verify.sh
```

## Solución enlazada

Consulta [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol) después de tu intento para comparar la forma de medir saldos.

## Reto adicional

Explica por qué una función `release()` que pudiera ejecutarse dos veces sería peligrosa incluso si el segundo intento encuentra balance cero.

## Resumen

- las operaciones de valor necesitan autorización y estado correctos;
- Checks-Effects-Interactions reduce una clase importante de riesgos;
- `call` exige comprobar su resultado;
- un `revert` deshace cambios de estado de la llamada;
- las pruebas deben observar dinero y estado.

## Siguiente paso

Continúa con [Lección 04 — Reverts precisos, suite y checkpoint](04-reverts-precisos-suite-y-checkpoint.md).

## Referencias

- [Contracts and Ether transfer — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html)
- [Security Considerations — Solidity](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
- [Writing Tests — Foundry](https://getfoundry.sh/forge/writing-tests)
