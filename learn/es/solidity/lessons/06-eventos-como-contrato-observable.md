# Lección 06 — Eventos como contrato observable

## Qué vas a conseguir

Vas a usar los eventos de FreelanceEscrow como una bitácora consumible por software externo y a distinguirlos del estado contractual. Al terminar podrás decidir qué dato pertenece al storage y qué dato puede viajar como señal para indexadores y UIs.

## Antes de empezar

Completa la [Lección 05](05-abi-la-frontera-publica.md) y ejecuta el gate:

```bash
bash tools/verify.sh
```

## El problema

Una aplicación externa necesita enterarse de que el freelancer marcó entrega o de que el escrow terminó en liberación o reembolso. Consultar `state()` repetidamente permite conocer el presente, pero no cuenta la historia ni sustituye una señal observable.

## Concepto

Los eventos escriben logs en la ejecución EVM. Son útiles para consumidores externos, pero **los contratos no deben tratarlos como almacenamiento consultable por lógica on-chain**.

FreelanceEscrow ya emite:

```solidity
event Delivered();
event Released(uint256 amount);
event Refunded(uint256 amount);
```

El estado responde “¿dónde está ahora el acuerdo?”. Los eventos ayudan a responder “¿qué ocurrió durante su vida?”. Son perspectivas complementarias.

## Demostración

[EN PANTALLA]

```bash
forge inspect FreelanceEscrow abi
forge test -vv
```

Busca las entradas de tipo `event` en la ABI. Observa que `Released(uint256)` y `Refunded(uint256)` exponen el importe liquidado sin añadir otra variable de storage sólo para historial.

## Código real

En [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol), cada transición relevante actualiza el estado y emite el evento correspondiente dentro de la misma transacción.

Si la transacción completa revierte, ni el cambio de estado ni sus logs quedan confirmados como resultado exitoso.

## Qué acaba de pasar

Ya puedes diseñar una integración que observe eventos para actualizar su vista y consulte el contrato cuando necesite la verdad actual. Evitaste dos extremos: polling como única estrategia y asumir que un evento sustituye al storage.

## Errores comunes

- Guardar en storage cada dato sólo porque quizá una UI quiera mostrarlo.
- Considerar un evento como prueba de una transacción que finalmente revirtió.
- Cambiar nombres o tipos de eventos sin tratarlo como cambio de integración.
- Indexar parámetros sin criterio.
- Diseñar lógica contractual que dependa de consultar logs anteriores.

## Buenas prácticas

Emite eventos para transiciones importantes y datos que consumidores externos necesitan observar. Mantén nombres estables y documenta semántica, no sólo sintaxis. Usa storage para reglas que el propio contrato debe consultar en ejecuciones futuras.

## Tu turno

[PAUSA PARA EJERCICIO] Diseña en papel un evento `ProjectReferenced(bytes32 projectReference)` y decide si ese valor también tendría que persistirse en storage. Justifica ambas decisiones antes de escribir código.

## Cómo comprobar

Tu explicación debe responder:

1. qué puede reconstruir un indexador con los eventos actuales;
2. qué dato sólo puede conocer consultando `state()`;
3. qué ocurre con los logs de una transacción que revierte.

## Solución enlazada

No hay código único para el ejercicio. Usa la [documentación de eventos](https://docs.soliditylang.org/en/v0.8.35/contracts.html#events) para validar tu razonamiento.

## Reto adicional

Investiga qué coste y limitaciones tiene marcar un parámetro como `indexed` y por qué no conviene hacerlo de forma automática.

## Resumen

- eventos y storage resuelven problemas distintos;
- los logs forman parte de la interfaz observable;
- un revert revierte el resultado completo de la transacción;
- eventos bien diseñados facilitan indexación y UI;
- la estabilidad semántica importa tanto como la firma.

## Siguiente paso

Continúa con la [Lección 07 — Interfaces explícitas y compatibilidad](07-interfaces-explicitas-y-compatibilidad.md).

## Referencias

- [Events — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html#events)
- [ABI events — Solidity](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html#events)
- [Security Considerations — Solidity](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
