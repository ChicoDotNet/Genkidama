# Lección 07 — Interfaces explícitas y compatibilidad

## Qué vas a conseguir
Vas a separar mentalmente la implementación de FreelanceEscrow de la superficie que necesita un consumidor externo. Al terminar podrás diseñar una `interface` mínima, compararla contra la ABI real y reconocer cambios que rompen integraciones.

## Antes de empezar
Completa la [Lección 06](06-eventos-como-contrato-observable.md) y ejecuta `bash tools/verify.sh`.

## El problema
Una UI o un contrato integrador necesita participantes, estado y operaciones, no los detalles internos de FreelanceEscrow. Si cada consumidor depende de todo lo que expone la implementación, aumenta el acoplamiento y cada cambio resulta más difícil de revisar.

## Concepto
Una interface de Solidity declara funciones externas sin implementar lógica. Para un consumidor de FreelanceEscrow podría bastar algo como:

```solidity
interface IFreelanceEscrow {
    function client() external view returns (address);
    function freelancer() external view returns (address);
    function state() external view returns (uint8);
    function markDelivered() external;
    function release() external;
    function refund() external;
}
```

No copies esta interface al proyecto todavía. Primero compárala con la ABI generada y decide qué necesita realmente el consumidor. La ABI es la frontera binaria; una interface es una forma de expresar en Solidity una parte de esa frontera.

## Demostración
[EN PANTALLA]
```bash
forge inspect FreelanceEscrow abi
cast sig "release()"
cast sig "refund()"
```

Localiza las firmas públicas y observa cuáles provienen de getters generados por variables `public`. Trata cambios de nombre, tipos u orden de parámetros como cambios de integración.

## Código real
El contrato canónico sigue siendo [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol). La lección usa su ABI real como fuente de verdad y evita añadir una segunda API sin una necesidad funcional demostrada.

## Qué acaba de pasar
Ya puedes diseñar una frontera mínima sin confundirla con la implementación. También viste que no toda interface conceptual necesita convertirse inmediatamente en otro artefacto versionado.

## Errores comunes
- Copiar toda la implementación dentro de una interface.
- Tratar cambios de firma como detalles internos.
- Confundir compatibilidad ABI con compatibilidad semántica.
- Añadir interfaces sólo por exhibir una abstracción.

## Buenas prácticas
Diseña interfaces desde las necesidades del consumidor. Mantén pequeña la superficie y valida las firmas contra la ABI compilada antes de publicar un contrato nuevo de integración.

## Tu turno
[PAUSA PARA EJERCICIO] Diseña una interface de sólo lectura con participantes y estado. Después diseña otra que agregue operaciones mutables. Explica cuál consumiría una UI de consulta y por qué.

## Cómo comprobar
Debes poder explicar qué garantiza una interface al compilador, qué no garantiza sobre permisos o semántica y qué cambios de firma romperían un consumidor.

## Solución enlazada
Contrasta tu diseño con la [documentación oficial de interfaces](https://docs.soliditylang.org/en/v0.8.35/contracts.html#interfaces). No hay una única interface correcta: depende del consumidor.

## Reto adicional
Compara los selectores de `release()` y `refund()` y explica qué parte de la firma canónica los determina.

## Resumen
- una interface expresa una frontera pública elegida;
- ABI y semántica son contratos relacionados pero distintos;
- consumidores diferentes pueden necesitar superficies diferentes;
- no añadas abstracciones sin una necesidad concreta.

## Siguiente paso
Continúa con la [Lección 08 — Transferencias, atomicidad y checkpoint 02](08-transferencias-atomicidad-y-checkpoint.md).

## Referencias
- [Interfaces — Solidity](https://docs.soliditylang.org/en/v0.8.35/contracts.html#interfaces)
- [ABI Specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
