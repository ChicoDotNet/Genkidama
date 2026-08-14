# Lección 16 — Hardening, reentrada y checkpoint 04

## Qué vas a conseguir
Vas a comprobar una propiedad de seguridad crítica en un contrato que transfiere Ether: el estado debe protegerse antes de entregar control a código externo. Cerrarás el bloque con un checkpoint de hardening.

## Antes de empezar
Completa la [Lección 15](15-gas-y-rendimiento-con-evidencia.md).

## El problema
`refund()` envía Ether al cliente. Si el cliente es otro contrato, su `receive()` puede ejecutar código y volver a llamar a FreelanceEscrow durante la misma transacción. Pensar sólo en usuarios EOA deja una frontera sin probar.

## Concepto
El patrón **checks → effects → interactions** reduce riesgo de reentrada: valida primero, actualiza el estado antes de la interacción externa y sólo después transfiere control. No es una garantía universal ni sustituye threat modeling, pero hace explícita una defensa importante.

En `refund()` la transición a `Refunded` ocurre antes del `call`. Si el receptor intenta `refund()` otra vez, la segunda llamada encuentra un estado inválido.

## Demostración
[DEMO] Abre [`../app/test/Security.t.sol`](../app/test/Security.t.sol). `ReentrantClient` es deliberadamente hostil: durante su `receive()` intenta un segundo reembolso con una llamada de bajo nivel y registra si funcionó.

Ejecuta:

```bash
forge test --match-test testRefundUpdatesStateBeforeExternalInteraction -vvvv
```

La prueba exige cuatro resultados: estado `Refunded`, escrow vacío, cliente recibe exactamente un depósito y reentrada rechazada.

## Código real
La regresión no agrega una bandera “anti-reentrancy” por exhibición. Primero demuestra que la máquina de estados y el orden de efectos ya protegen este camino concreto. Si el contrato creciera hacia múltiples pagos o callbacks, el threat model tendría que revisarse.

## Qué acaba de pasar
Probaste seguridad contra un actor con código propio, no sólo contra direcciones pasivas. El resultado protege una propiedad específica y auditable.

## Errores comunes
- Asumir que `client` o `freelancer` siempre son EOAs.
- Hacer la transferencia antes de actualizar estado.
- Añadir un guard genérico sin entender la secuencia vulnerable.
- Tratar una sola regresión de reentrada como auditoría completa.
- Ignorar otros riesgos: claves, front-running, lógica económica, upgrades o integraciones.

## Buenas prácticas
Modela quién puede ejecutar código en cada frontera, actualiza efectos antes de interacciones cuando el diseño lo permita, prueba colaboradores hostiles y documenta los límites de la defensa.

## Tu turno — Checkpoint 04
Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

## Cómo comprobar
```bash
bash tools/verify.sh
forge test --match-path app/test/Security.t.sol -vvvv
```

El gate completo y la regresión de seguridad deben quedar verdes.

## Solución enlazada
Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) sólo después de completar tu intento.

## Reto adicional
Construye un pequeño threat model: activos, actores, fronteras externas, tres abusos plausibles y la evidencia que necesitarías para cada mitigación.

## Resumen
- contratos receptores pueden ejecutar código;
- estado antes de interacción protege este flujo de reentrada;
- una regresión hostil convierte la afirmación en evidencia;
- seguridad no se reduce a un modifier;
- una suite educativa verde no equivale a auditoría.

## Siguiente paso
La siguiente lección será la evaluación final sin receta: deberás modificar FreelanceEscrow, corregir un defecto y defender tus decisiones con evidencia.

## Referencias
- [Solidity — Security Considerations: Reentrancy](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html#reentrancy)
- [Solidity — Sending and Receiving Ether](https://docs.soliditylang.org/en/v0.8.35/contracts.html#receive-ether-function)
- [Foundry — Traces](https://getfoundry.sh/forge/traces)
