# Lección 02 — Máquina de estados, roles y entrega

## Qué vas a conseguir

Vas a modelar el flujo del proyecto como una máquina de estados y a comprobar que sólo el freelancer autorizado puede registrar una entrega.

## Antes de empezar

Completa la [Lección 01](01-primer-deposito-y-estado.md) y confirma que el gate sigue verde:

```bash
bash tools/verify.sh
```

## El problema

Tener Ether en custodia no basta. El contrato necesita saber **qué ha ocurrido** y quién está autorizado a provocar cada transición. Si cualquier dirección pudiera marcar el trabajo como entregado, el estado dejaría de representar el acuerdo real.

## Concepto

`FreelanceEscrow` usa un `enum` para expresar cuatro estados:

```solidity
enum State {
    Funded,
    Delivered,
    Released,
    Refunded
}
```

El flujo permitido es deliberadamente pequeño:

```text
Funded -> Delivered -> Released
   |
   +---------------> Refunded
```

No existe transición desde `Released` o `Refunded`: son estados terminales.

La autorización y el estado son reglas diferentes. `markDelivered()` primero comprueba la identidad del llamador y después exige que el contrato siga en `Funded`.

## Demostración

[EN PANTALLA] Localiza `markDelivered()` y `_requireState()` en [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol).

[EJECUTAR]

```bash
forge test --match-test testOnlyFreelancerCanMarkDelivered -vv
```

La prueba usa `vm.prank` para simular una dirección distinta al freelancer y espera `OnlyFreelancer()`.

## Código real

```solidity
function markDelivered() external {
    if (msg.sender != freelancer) revert OnlyFreelancer();
    _requireState(State.Funded);
    state = State.Delivered;
    emit Delivered();
}
```

`external` expresa que esta operación forma parte de la interfaz pública del contrato. El helper privado concentra la regla de transición:

```solidity
function _requireState(State expected) private view {
    if (state != expected) revert InvalidState(expected, state);
}
```

`InvalidState` incluye el estado esperado y el observado. Esa información será útil tanto para pruebas como para diagnóstico de integraciones.

## Qué acaba de pasar

Separaste tres responsabilidades que suelen confundirse: **quién** puede actuar, **cuándo** puede hacerlo y **qué cambio** de estado ocurre. Esa separación hace más fácil leer, probar y auditar el contrato.

## Errores comunes

- Usar un `bool delivered` y después acumular más booleanos incompatibles.
- Cambiar `state` antes de validar autorización.
- Confiar en la UI para impedir una llamada no autorizada.
- Tratar `enum` como texto; en la EVM se representa mediante un valor numérico acotado.
- Crear modifiers sólo por estilo cuando una condición corta se entiende mejor directamente.

## Buenas prácticas

Modela estados válidos de forma explícita y reduce los estados imposibles. Las reglas críticas deben vivir en el contrato: una interfaz web puede mejorar UX, pero no es una frontera de seguridad.

## Tu turno

Agrega una prueba positiva que demuestre que el freelancer sí puede mover el contrato de `Funded` a `Delivered`. Verifica el estado después de la llamada.

## Cómo comprobar

```bash
forge test --match-test testFreelancerDeliversAndClientReleases -vv
bash tools/verify.sh
```

## Solución enlazada

La suite canónica en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol) muestra una forma de simular participantes. Revísala sólo después de tu intento.

## Reto adicional

Dibuja dos transiciones que **no** deberían existir y explica qué pérdida o abuso permitirían.

## Resumen

- `enum` hace explícitos los estados válidos.
- autorización y transición son reglas distintas;
- `msg.sender` se valida dentro del contrato;
- los estados terminales reducen ambigüedad;
- los custom errors pueden transportar contexto del fallo.

## Siguiente paso

Continúa con [Lección 03 — Liberación, reembolso y transferencia de valor](03-liberacion-reembolso-y-transferencia-de-valor.md).

## Referencias

- [Enums — Solidity types](https://docs.soliditylang.org/en/v0.8.35/types.html#enums)
- [Custom Errors — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html#custom-errors)
- [`prank` — Foundry](https://getfoundry.sh/reference/cheatcodes/prank/)
