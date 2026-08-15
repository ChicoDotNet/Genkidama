# Solución de referencia — Checkpoint 01

> Consulta esta referencia sólo después de intentar el checkpoint. La forma exacta puede variar; lo que importa es proteger la misma regla.

## Contrato

Declara un error específico:

```solidity
error InvalidFreelancer();
```

Valida la dirección antes de guardar participantes:

```solidity
constructor(address freelancer_) payable {
    if (msg.value == 0) revert EmptyDeposit();
    if (freelancer_ == address(0)) revert InvalidFreelancer();

    client = msg.sender;
    freelancer = freelancer_;
    state = State.Funded;
}
```

El orden entre las dos precondiciones puede documentarse según la política deseada cuando ambas sean inválidas simultáneamente. Lo importante es que ninguna instancia válida termine con freelancer cero.

## Prueba

Una dirección razonable es preparar saldo para el cliente, simularlo como deployer y esperar el error exacto:

```solidity
function testRejectsZeroFreelancer() public {
    vm.expectRevert(FreelanceEscrow.InvalidFreelancer.selector);
    vm.prank(CLIENT);
    new FreelanceEscrow{value: DEPOSIT}(address(0));
}
```

Conserva además la prueba positiva de construcción y toda la suite previa.

## Por qué pertenece al contrato

Una UI puede prevenir accidentes en esa interfaz concreta, pero cualquier cuenta, script o contrato puede invocar el constructor sin usarla. La regla que protege una dirección esencial del acuerdo debe ejecutarse en la misma frontera que acepta y almacena el dato.

## Verificación

```bash
bash tools/verify.sh
```

No consideres terminado el checkpoint si el test nuevo pasa a costa de romper depósito, entrega, liberación o reembolso.
