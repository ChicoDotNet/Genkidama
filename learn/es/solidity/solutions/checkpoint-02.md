# Solución de referencia — Checkpoint 02

> Ábrela sólo después de intentar el checkpoint. La forma exacta del helper puede variar; los comportamientos protegidos no.

Una solución simple usa un contrato de prueba que actúa como freelancer y rechaza el pago:

```solidity
contract RejectingFreelancer {
    function deliver(FreelanceEscrow escrow) external {
        escrow.markDelivered();
    }

    receive() external payable {
        revert("reject payment");
    }
}
```

Después, la prueba despliega el escrow con `address(rejecting)` como freelancer, ejecuta `deliver`, espera `TransferFailed` durante `release()` y verifica las invariantes posteriores:

```solidity
vm.expectRevert(FreelanceEscrow.TransferFailed.selector);
vm.prank(CLIENT);
escrow.release();

require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Delivered));
require(address(escrow).balance == DEPOSIT);
```

La parte importante no es el nombre del helper. Es demostrar que el fallo de la interacción externa revierte también el cambio previo de `state` y la transferencia. La transacción completa no confirma un estado parcial.

No resuelvas el caso eliminando el chequeo del resultado de `call`: eso ocultaría el fallo y podría dejar una contabilidad falsa.

Vuelve a la [Lección 08](../lessons/08-transferencias-atomicidad-y-checkpoint.md) y explica el flujo checks → effects → interaction con tus palabras.
