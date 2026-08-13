# Lección 10 — Invariantes de estado y autorización

## Qué vas a conseguir
Aprenderás a expresar como propiedades quién puede mover el escrow y qué debe permanecer intacto cuando una llamada no autorizada falla.

## Antes de empezar
Completa la [Lección 09](09-fuzzing-propiedades-de-valor.md).

## El problema
Una prueba con una sola dirección desconocida no representa toda la regla. La condición real es que sólo el freelancer puede marcar entrega y sólo el cliente puede liberar fondos.

## Concepto
Una invariante describe algo que debe seguir siendo cierto. En FreelanceEscrow:

- un caller distinto del freelancer no cambia `Funded`;
- un caller distinto del cliente no cambia `Delivered` ni el saldo.

## Demostración
```bash
forge test --match-test testFuzzOnlyFreelancerCanMarkDelivered -vv
forge test --match-test testFuzzOnlyClientCanRelease -vv
```

## Código real
Revisa [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). Los tests comprueban error, estado y fondos.

## Qué acaba de pasar
La autorización quedó expresada como una propiedad sobre muchos callers, no como un único ejemplo.

## Errores comunes
- Comprobar sólo que hubo revert.
- No excluir al actor autorizado del caso negativo.
- Confiar en el frontend para imponer roles.
- No revisar el saldo después del fallo.

## Buenas prácticas
Prueba la frontera pública y verifica ausencia de efectos parciales.

## Tu turno
Agrega una propiedad equivalente para `refund()`: callers distintos del cliente deben fallar sin cambiar estado ni saldo.

## Cómo comprobar
```bash
forge test -vv
bash tools/verify.sh
```

## Solución enlazada
Compara después con la suite canónica del curso.

## Reto adicional
Enumera las transiciones de `State` que nunca deben ocurrir.

## Resumen
Autorización, estado y fondos forman una sola historia de seguridad verificable.

## Siguiente paso
Continúa con la [Lección 11](11-composicion-y-colaboradores-hostiles.md).

## Referencias
- [Solidity — Error handling](https://docs.soliditylang.org/en/v0.8.35/control-structures.html#error-handling-assert-require-revert-and-exceptions)
- [Foundry — Fuzz Testing](https://getfoundry.sh/forge/advanced-testing/fuzz-testing)
