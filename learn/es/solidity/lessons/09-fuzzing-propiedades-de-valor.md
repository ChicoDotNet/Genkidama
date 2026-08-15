# Lección 09 — Fuzzing: propiedades sobre valor

## Qué vas a conseguir
Vas a convertir ejemplos concretos en propiedades que Foundry verifica con muchos valores de entrada. Al terminar podrás distinguir una prueba de ejemplo de una prueba fuzz útil y sabrás formular propiedades sobre el depósito de FreelanceEscrow.

## Antes de empezar
Completa la [Lección 08](08-transferencias-atomicidad-y-checkpoint.md) y ejecuta `bash tools/verify.sh`.

## El problema
Hasta ahora probamos principalmente con un depósito de `1 ether`. Eso demuestra casos importantes, pero no responde una pregunta más amplia: ¿las reglas siguen siendo ciertas para otros depósitos positivos representables?

## Concepto
Una prueba fuzz recibe parámetros. Foundry genera múltiples entradas y ejecuta la misma propiedad contra ellas. La clave no es “generar números al azar”, sino expresar una afirmación que debería sostenerse para toda una clase válida de entradas.

Para este escrow hay propiedades sencillas y valiosas:

- todo depósito positivo inicia en `Funded`;
- el saldo inicial del contrato coincide exactamente con el depósito;
- después de entrega + liberación, el freelancer recibe exactamente ese depósito;
- después de un reembolso válido, el escrow queda vacío.

## Demostración
[EN PANTALLA]

```bash
forge test --match-test testFuzzPositiveDepositStartsFunded -vv
forge test --match-test testFuzzReleaseTransfersExactDeposit -vv
```

Las funciones reciben `uint96 rawAmount`. Si el valor es cero, el caso se descarta porque pertenece al contrato negativo de `EmptyDeposit`; para cualquier valor positivo se verifica la misma propiedad.

## Código real
Revisa [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). La ayuda `_deployWithAmount` evita duplicar setup y deja visible qué parte cambia: el valor depositado.

## Qué acaba de pasar
Pasaste de “con 1 ether funciona” a “para una familia amplia de depósitos positivos, estas invariantes económicas se conservan”. Eso aumenta confianza sin multiplicar manualmente casos de ejemplo.

## Errores comunes
- Fuzzear parámetros sin formular una propiedad clara.
- Usar rangos enormes sólo para presumir cantidad de casos.
- Convertir entradas inválidas en falsos fallos de la propiedad.
- Eliminar pruebas de ejemplo porque ya existen pruebas fuzz.
- Confundir fuzzing con una auditoría de seguridad.

## Buenas prácticas
Mantén ejemplos legibles para historias clave y añade fuzzing donde una regla debe sostenerse sobre muchas entradas. Prefiere tipos acotados cuando expresan mejor el dominio educativo y evitan ruido irrelevante.

## Tu turno
[PAUSA PARA EJERCICIO] Agrega una prueba fuzz que demuestre que un depósito positivo reembolsado vuelve al cliente sin dejar saldo en el escrow. No cambies el contrato para facilitar la prueba.

## Cómo comprobar

```bash
forge test --match-test 'testFuzz*' -vv
bash tools/verify.sh
```

Explica qué propiedad prueba cada función y cuál es su precondición.

## Solución enlazada
La suite canónica en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol) contiene una referencia después de tu intento.

## Reto adicional
Investiga `bound` y `vm.assume` en Foundry. Explica cuándo mejorarían la intención de una prueba y cuándo sólo ocultarían casos incómodos.

## Resumen
- fuzzing verifica propiedades sobre múltiples entradas;
- la propiedad importa más que la aleatoriedad;
- ejemplos y fuzz tests son complementarios;
- las precondiciones deben representar el dominio real;
- pruebas verdes no sustituyen auditoría.

## Siguiente paso
Continúa con la [Lección 10](10-invariantes-de-estado-y-autorizacion.md) para formular propiedades que no dependen sólo del monto.

## Referencias
- [Foundry — Fuzz Testing](https://getfoundry.sh/forge/advanced-testing/fuzz-testing)
- [Foundry — Writing Tests](https://getfoundry.sh/forge/writing-tests)
- [Solidity — Units and globally available variables](https://docs.soliditylang.org/en/v0.8.35/units-and-global-variables.html)
