# Lección 01 — Primer depósito y estado del escrow

## Qué vas a conseguir

Vas a ejecutar `FreelanceEscrow`, entender cómo nace un contrato con Ether depositado y comprobar con una prueba que cliente, freelancer, estado y saldo inicial quedaron registrados correctamente.

## Antes de empezar

Instala una release estable de [Foundry](https://getfoundry.sh/introduction/installation/) y confirma:

```bash
forge --version
```

Trabaja desde la raíz de esta carpeta `learn/es/solidity/`.

## El problema

Un cliente quiere separar el dinero de un proyecto antes de que el freelancer entregue. El contrato debe conservar el depósito sin depender de una base de datos ni de un servidor propio y debe dejar inequívoco quién es cada participante.

## Concepto

Un contrato Solidity tiene estado persistente. En nuestro caso:

- `client` es quien despliega y financia;
- `freelancer` se recibe como argumento del constructor;
- `state` comienza en `Funded`;
- `msg.sender` identifica al llamador;
- `msg.value` contiene el Ether enviado con la creación.

El constructor es `payable` porque debe aceptar valor durante el despliegue. Si `msg.value` es cero, el contrato revierte con `EmptyDeposit()`.

## Demostración

[EN PANTALLA] Abre [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol) y localiza `constructor`, `client`, `freelancer` y `state`.

[EJECUTAR]

```bash
forge test --match-test testDepositStartsFunded -vv
```

La prueba usa una cuenta ficticia como cliente, le asigna saldo local y crea el contrato enviando `1 ether`.

## Código real

La idea central del constructor es pequeña:

```solidity
constructor(address freelancer_) payable {
    if (msg.value == 0) revert EmptyDeposit();
    client = msg.sender;
    freelancer = freelancer_;
    state = State.Funded;
}
```

`immutable` hace que las direcciones de los participantes sólo se asignen durante construcción y después queden fijas en la instancia.

La prueba no pregunta sólo si el despliegue “no explotó”. Comprueba cuatro invariantes observables: cliente, freelancer, estado y balance.

## Qué acaba de pasar

Ya ejecutaste una aplicación Solidity real. La instancia de `FreelanceEscrow` contiene valor y estado, y su comportamiento inicial está protegido por una prueba reproducible.

## Errores comunes

- Confundir el saldo de una cuenta con el saldo del contrato.
- Olvidar `payable` y esperar que el constructor acepte Ether.
- Usar `tx.origin` para identificar al cliente; aquí la identidad de la llamada pertenece a `msg.sender`.
- Probar sólo que el contrato se creó sin verificar su estado inicial.
- Usar una red o dinero real para un ejercicio local.

## Buenas prácticas

Mantén pequeño el constructor, valida precondiciones antes de guardar estado y expresa fallos esperados mediante errores explícitos. En pruebas, verifica comportamiento observable en lugar de depender de detalles internos accidentales.

## Tu turno

[PAUSA PARA EJERCICIO] Agrega una prueba que intente desplegar el contrato con depósito cero y espere `EmptyDeposit()`.

No cambies todavía el contrato: la regla ya existe; tu trabajo es demostrarla.

## Cómo comprobar

```bash
forge test --match-test testDepositStartsFunded -vv
forge test -vv
```

El gate completo del curso es:

```bash
bash tools/verify.sh
```

## Solución enlazada

Compara tu prueba con el estilo de [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol) después de intentar resolverla. No necesitas que el nombre sea idéntico mientras el comportamiento quede protegido.

## Reto adicional

Explica por qué `client` y `freelancer` son buenos candidatos para `immutable`, mientras `state` no puede serlo.

## Resumen

- `payable` permite recibir Ether en el constructor.
- `msg.sender` identifica al cliente que crea la instancia.
- `msg.value` representa el depósito enviado.
- `immutable` fija participantes después del despliegue.
- una prueba útil verifica invariantes, no sólo ausencia de excepciones.

## Siguiente paso

Continúa con [Lección 02 — Máquina de estados, roles y entrega](02-maquina-de-estados-roles-y-entrega.md).

## Referencias

- [Contracts — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html)
- [Writing Tests — Foundry](https://getfoundry.sh/forge/writing-tests)
- [`deal` cheatcode — Foundry](https://getfoundry.sh/reference/cheatcodes/deal/)
- [`prank` cheatcode — Foundry](https://getfoundry.sh/reference/cheatcodes/prank/)
