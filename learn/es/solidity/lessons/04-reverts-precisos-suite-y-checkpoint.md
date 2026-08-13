# Lección 04 — Reverts precisos, suite y checkpoint

## Qué vas a conseguir

Vas a probar transiciones inválidas con el revert exacto que produce el contrato, entender cómo se codifica un custom error con argumentos y cerrar el primer checkpoint sin debilitar el gate.

## Antes de empezar

Completa la [Lección 03](03-liberacion-reembolso-y-transferencia-de-valor.md) y ejecuta:

```bash
bash tools/verify.sh
```

## El problema

Una prueba que sólo dice “algo revirtió” puede pasar por la razón equivocada. En un contrato que custodia valor necesitamos distinguir autorización, estado inválido y fallo de transferencia.

## Concepto

`InvalidState(State expected, State actual)` tiene argumentos. Solidity codifica un custom error como su selector de cuatro bytes seguido por los argumentos ABI.

Por eso una expectativa precisa incluye todo el payload:

```solidity
vm.expectRevert(
    abi.encodeWithSelector(
        FreelanceEscrow.InvalidState.selector,
        FreelanceEscrow.State.Funded,
        FreelanceEscrow.State.Delivered
    )
);
```

Foundry ofrece una sobrecarga `expectRevert(bytes)` para comparar esos datos completos. Un selector por sí solo es suficiente para errores sin argumentos como `OnlyClient()`.

## Demostración

[EJECUTAR]

```bash
forge test --match-test testCannotRefundAfterDelivery -vv
```

La secuencia primero mueve el escrow a `Delivered` y después confirma que `refund()` exige `Funded` y observa `Delivered`.

## Código real

La suite canónica está en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). No usa red pública, claves privadas ni fondos reales. Las identidades y balances se preparan con cheatcodes de Foundry.

El gate reproducible del curso es:

```bash
bash tools/verify.sh
```

Ese script ejecuta formatter, compilación y suite usando [`../foundry.toml`](../foundry.toml) desde la raíz del curso. El CI ejecuta el mismo proyecto y no sustituye la verificación local.

## Qué acaba de pasar

Ahora las pruebas distinguen entre clases de fallo y verifican los datos relevantes de un error parametrizado. El gate completo demuestra formato, compilación y comportamiento.

## Errores comunes

- Usar sólo el selector para un custom error con argumentos y creer que es un match exacto.
- Cambiar la prueba para que acepte cualquier revert después de descubrir un fallo.
- Ejecutar Foundry desde un directorio que ignora el `foundry.toml` del curso.
- Confundir “compila” con “funciona”.
- Poner claves o fondos reales en una práctica.

## Buenas prácticas

Las pruebas negativas deben fallar por el motivo esperado. Conserva una sola configuración de proyecto, un gate reproducible y expectativas lo bastante específicas para detectar regresiones reales.

## Tu turno — Checkpoint 01

[PAUSA PARA EJERCICIO] Resuelve [`../exercises/checkpoint-01.md`](../exercises/checkpoint-01.md) sin abrir la solución. Vas a endurecer la construcción del escrow contra un freelancer inválido y proteger la regla con una prueba.

## Cómo comprobar

```bash
bash tools/verify.sh
```

Además, la prueba nueva del checkpoint debe pasar y las pruebas existentes deben permanecer verdes.

## Solución enlazada

Consulta [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md) sólo después de completar un intento.

## Reto adicional

Explica cuándo preferirías `expectPartialRevert` sobre comparar el error ABI completo y qué información dejarías de comprobar.

## Resumen

- los custom errors con argumentos contienen selector + datos ABI;
- una prueba negativa debe verificar la causa correcta;
- build y tests son gates distintos;
- la configuración de Foundry forma parte del contrato reproducible del proyecto;
- CI no justifica debilitar pruebas locales.

## Siguiente paso

Después del checkpoint, el siguiente bloque profundizará en ABI, eventos, diseño de interfaces y pruebas de más fronteras del escrow.

## Referencias

- [Custom Errors — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html#custom-errors)
- [ABI Specification — Solidity](https://docs.soliditylang.org/en/latest/abi-spec.html#errors)
- [`expectRevert` — Foundry](https://getfoundry.sh/cheatcodes/expect-revert)
- [Writing Tests — Foundry](https://getfoundry.sh/forge/writing-tests)
