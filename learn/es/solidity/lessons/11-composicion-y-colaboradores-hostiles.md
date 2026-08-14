# Lección 11 — Composición y colaboradores hostiles

## Qué vas a conseguir
Vas a razonar sobre una propiedad esencial de contratos: una dirección puede ser otro contrato con código propio. Usarás un colaborador hostil para verificar que FreelanceEscrow conserva sus invariantes cuando el receptor rechaza Ether.

## Antes de empezar
Completa la [Lección 10](10-invariantes-de-estado-y-autorizacion.md).

## El problema
Una EOA suele aceptar Ether sin ejecutar lógica propia, pero `freelancer` también puede ser un contrato. Si su `receive()` revierte, la llamada externa falla. El escrow no puede asumir que su contraparte coopera.

## Concepto
Composición significa que un contrato interactúa con otros contratos mediante fronteras públicas. El colaborador puede aceptar, revertir o ejecutar otras llamadas. La prueba `RejectingFreelancer` representa exactamente un receptor que puede marcar entrega pero rechaza el pago.

## Demostración
```bash
forge test --match-test testReleaseFailureRevertsStateAndRetainsFunds -vv
```

Sigue la secuencia: desplegar, marcar entrega, intentar liberar y observar rollback.

## Código real
El colaborador hostil y la regresión viven en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). No se añade una dependencia de producción sólo para probar composición.

## Qué acaba de pasar
Probaste una interacción entre contratos y verificaste que estado y fondos permanecen coherentes después de un fallo externo.

## Errores comunes
- Suponer que toda dirección es una EOA.
- Ignorar qué estado queda después de una llamada fallida.
- Añadir abstracciones productivas sólo porque un test las necesita.
- Considerar checks-effects-interactions una solución universal.

## Buenas prácticas
Construye colaboradores mínimos que reproduzcan una conducta concreta y comprueba las invariantes relevantes después del fallo.

## Tu turno
Diseña un contrato cliente que rechace Ether y explica qué propiedad de `refund()` permitiría probar.

## Cómo comprobar
```bash
forge test --match-test testReleaseFailureRevertsStateAndRetainsFunds -vv
bash tools/verify.sh
```

## Solución enlazada
La prueba canónica está en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol).

## Reto adicional
Explica por qué cambiar estado antes de una llamada externa ayuda, pero sigue siendo necesario manejar el resultado de esa llamada.

## Resumen
- una dirección puede contener código;
- colaboradores externos pueden fallar;
- rollback protege de estados parciales;
- composición complementa ejemplos y fuzzing.

## Siguiente paso
Continúa con la [Lección 12](12-estrategia-de-pruebas-y-checkpoint-03.md).

## Referencias
- [Solidity — Security Considerations](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
- [Solidity — Sending and Receiving Ether](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html#sending-and-receiving-ether)
