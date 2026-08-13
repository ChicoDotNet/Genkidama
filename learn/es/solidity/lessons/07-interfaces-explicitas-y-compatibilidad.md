# Lección 07 — Interfaces explícitas y compatibilidad

## Qué vas a conseguir
Vas a separar la implementación de FreelanceEscrow de la superficie que necesita un consumidor externo. Al terminar podrás usar una `interface` para expresar un contrato mínimo y reconocer cambios que rompen integraciones.

## Antes de empezar
Completa la [Lección 06](06-eventos-como-contrato-observable.md) y ejecuta `bash tools/verify.sh`.

## El problema
Una UI o contrato integrador necesita participantes, estado y operaciones, no los detalles internos de FreelanceEscrow. Importar siempre la implementación concreta aumenta acoplamiento.

## Concepto
Una interface declara funciones externas sin implementar lógica. En [`../app/src/IFreelanceEscrow.sol`](../app/src/IFreelanceEscrow.sol) hacemos explícito el subconjunto público consumido por integraciones.

La ABI es la frontera binaria; la interface vuelve esa frontera legible y compilable en Solidity.

## Demostración
[EN PANTALLA]
```bash
forge inspect FreelanceEscrow abi
forge inspect IFreelanceEscrow abi
forge test -vv
```

No necesitas ABI idénticas: el contrato concreto también publica eventos y errores. Debes conservar compatibles las firmas que el consumidor usa.

## Código real
La prueba `testPublicInterfaceMatchesEscrowAbi` convierte la dirección del contrato a `IFreelanceEscrow` y consulta `client`, `freelancer` y `state` a través de esa frontera.

## Qué acaba de pasar
Ahora existe un contrato de integración separado del detalle interno. Un refactor puede cambiar implementación sin obligar a cambiar consumidores mientras preserve firmas y semántica pública.

## Errores comunes
- Copiar toda la implementación dentro de la interface.
- Tratar cambios de nombre o tipos como detalles internos.
- Confundir compatibilidad ABI con compatibilidad semántica.
- Suponer que una conversión de dirección valida permisos o reglas de negocio.

## Buenas prácticas
Mantén interfaces pequeñas, orientadas al consumidor y protegidas por una prueba que realmente las use.

## Tu turno
[PAUSA PARA EJERCICIO] Diseña una interface de sólo lectura con participantes y estado. Explica cuándo sería mejor que exponer también operaciones mutables.

## Cómo comprobar
Debes poder explicar qué garantiza una interface al compilador, qué no garantiza sobre semántica y qué cambios de firma romperían un consumidor.

## Solución enlazada
Contrasta tu diseño con la [documentación oficial de interfaces](https://docs.soliditylang.org/en/v0.8.35/contracts.html#interfaces).

## Reto adicional
Compara los selectores de `release()` y `refund()` y explica qué parte de la firma canónica los determina.

## Resumen
- una interface expresa una frontera pública mínima;
- ABI y semántica son contratos relacionados pero distintos;
- consumidores diferentes pueden necesitar interfaces diferentes;
- pruebas de integración protegen la frontera realmente usada.

## Siguiente paso
Continúa con la [Lección 08](08-transferencias-atomicidad-y-checkpoint.md).

## Referencias
- [Interfaces — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html#interfaces)
- [ABI Specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
