# Lección 05 — ABI: el contrato que otros programas realmente ven

## Qué vas a conseguir

Vas a distinguir el código Solidity de la **ABI** que consumen herramientas e integraciones. Al terminar podrás inspeccionar la superficie pública de FreelanceEscrow y reconocer qué cambios pueden romper consumidores aunque el contrato siga compilando.

## Antes de empezar

Completa la [Lección 04](04-reverts-precisos-suite-y-checkpoint.md) y ejecuta:

```bash
bash tools/verify.sh
```

## El problema

Una integración no necesita conocer los helpers privados del contrato: necesita saber qué funciones existen, qué argumentos reciben, qué devuelven y qué eventos o errores puede observar. Esa frontera es parte del producto.

## Concepto

La Application Binary Interface describe cómo se codifican llamadas y resultados. Para una función, los primeros cuatro bytes de `keccak256("nombre(tipos)")` forman el selector.

Estas operaciones tienen firmas distintas aunque ninguna reciba argumentos:

```text
markDelivered()
release()
refund()
```

El tipo de retorno no forma parte del selector de una función. Por eso un cambio puede conservar selector y aun así romper a quien decodifica la respuesta con otra expectativa.

## Demostración

[EN PANTALLA]

```bash
forge inspect FreelanceEscrow abi
cast sig "markDelivered()"
cast sig "release()"
cast sig "refund()"
```

No memorices los hexadecimales. Aprende a derivarlos con tooling y a tratarlos como una frontera estable.

## Código real

Abre [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol) y separa mentalmente dos grupos:

- superficie externa: `client`, `freelancer`, `state`, `markDelivered`, `release`, `refund`;
- implementación interna: `_requireState` y el orden concreto de cada comprobación.

`forge inspect` permite revisar la primera sin convertir cada detalle del segundo grupo en una promesa pública.

## Qué acaba de pasar

Separaste **contrato público** de **implementación**. Esa distinción ayuda a revisar cambios y a explicar por qué una refactorización privada puede ser compatible mientras un cambio pequeño de tipos públicos no lo es.

## Errores comunes

- Tratar cualquier función externa como detalle interno.
- Cambiar tipos de parámetros sin revisar consumidores.
- Suponer que compilar demuestra compatibilidad con una integración existente.
- Confundir ABI con bytecode.
- Memorizar selectores en lugar de derivarlos.

## Buenas prácticas

Mantén la superficie externa tan pequeña como permita el problema. Antes de cambiarla, inspecciona qué depende de ella y verifica la ABI generada.

## Tu turno

[PAUSA PARA EJERCICIO] Genera la ABI de FreelanceEscrow y clasifica cada entrada como función, evento o error. Después identifica qué elementos del archivo Solidity no aparecen en esa ABI y explica por qué.

## Cómo comprobar

```bash
forge inspect FreelanceEscrow abi
forge build
```

Explica qué información necesitaría una integración para consultar `state()` y qué información interna no necesita conocer.

## Solución enlazada

La solución de esta lección es conceptual: compara tu explicación con la [especificación ABI de Solidity](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html). No copies selectores desde una respuesta externa cuando puedes derivarlos con `cast sig`.

## Reto adicional

Investiga qué sucede si dos firmas diferentes producen el mismo selector de cuatro bytes y por qué una API pequeña también reduce superficie de integración.

## Resumen

- la ABI es una frontera pública;
- los selectores identifican funciones por nombre y tipos de argumentos;
- retornos, eventos y errores también forman contratos de decodificación;
- implementación privada y API pública deben revisarse por separado;
- tooling supera a memorizar hexadecimales.

## Siguiente paso

Continúa con la [Lección 06 — Eventos como contrato observable](06-eventos-como-contrato-observable.md).

## Referencias

- [Contract ABI Specification — Solidity](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
- [Contracts — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html)
- [`forge inspect`](https://getfoundry.sh/reference/forge/forge-inspect)
- [`cast sig`](https://getfoundry.sh/reference/cast/cast-sig)
