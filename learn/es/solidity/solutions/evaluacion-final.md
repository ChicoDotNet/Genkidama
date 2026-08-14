# Solución de referencia — Evaluación final Solidity

> Abre esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable agrega una referencia inmutable de proyecto al constructor y la valida en la misma frontera que valida el depósito. La referencia forma parte del significado del escrow, por lo que no debería poder cambiar después del despliegue.

También rechaza que `client` y `freelancer` sean la misma dirección. Esa validación pertenece al contrato porque cualquier integración puede desplegarlo; depender sólo de una UI dejaría la regla fuera de la fuente de verdad.

## Historia A — Referencia y evento

Una forma posible es declarar:

```solidity
bytes32 public immutable projectId;

error InvalidProjectId();
event EscrowCreated(bytes32 indexed projectId, address indexed client, address indexed freelancer, uint256 amount);
```

El constructor recibe la referencia, rechaza `bytes32(0)`, la asigna y emite el evento después de establecer participantes/estado. Puedes elegir otros nombres si expresan el mismo contrato.

Las pruebas deben demostrar al menos:

- referencia válida persistida;
- referencia cero rechazada con el error esperado;
- evento/ABI coherentes si decides verificar logs o inspección;
- el resto de despliegues de la suite usa una referencia válida.

## Historia B — Identidad

Una política de referencia rechaza `freelancer_ == msg.sender` con un custom error específico, por ejemplo `SameParticipant()`. La prueba de regresión debe desplegar desde un caller conocido y comprobar el revert exacto.

La razón: separar identidades conserva una frontera clara entre quien autoriza liberación/reembolso y quien declara entrega/recibe pago.

## Historia C — Regresión completa

No borres ni simplifiques los tests existentes para acomodar el nuevo constructor. Actualiza helpers como `_deploy()` y `_deployWithAmount()` para incluir una referencia válida y conserva:

```bash
bash tools/verify.sh
```

La regresión de [`../app/test/Security.t.sol`](../app/test/Security.t.sol) debe seguir demostrando un solo reembolso frente a reentrada.

## Historia D — Documentación

Una nota válida puede consultar la documentación oficial de Solidity sobre funciones/estado inmutable, ABI o seguridad, y Foundry sobre pruebas/traces. Lo importante es explicar qué decisión cambió a partir de la fuente, no listar enlaces decorativos.

## Historia E — Criterio esperado

Una respuesta fuerte reconoce que cambiar constructor y getters modifica la frontera de integración aunque la máquina de estados permanezca igual. Distingue validación de datos/roles de mitigaciones contra control externo; reconoce amenazas fuera del curso y mide gas antes de optimizar. Si propone arbitraje, identifica un nuevo actor, nuevas transiciones y reglas de autorización en lugar de añadir un booleano ambiguo.

## Defensa de entrevista

Explica FreelanceEscrow como una máquina de estados pequeña con valor real simulado. Describe por qué usa roles inmutables, custom errors, eventos, checks-effects-interactions, pruebas deterministas, fuzzing y actores hostiles. Cierra con límites: pruebas locales no sustituyen una auditoría ni cubren economía, claves, governance o integraciones futuras.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa comportamiento y explicación, no similitud de líneas con esta referencia.
