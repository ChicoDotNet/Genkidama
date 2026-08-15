# Lección 13 — Tooling y superficie profesional

## Qué vas a conseguir
Vas a usar Foundry para inspeccionar el contrato como lo haría alguien que debe mantenerlo o integrarlo, no sólo compilarlo. Al terminar podrás distinguir código fuente, ABI, bytecode y artefactos de build.

## Antes de empezar
Completa la [Lección 12](12-estrategia-de-pruebas-y-checkpoint-03.md) y confirma que `bash tools/verify.sh` queda verde.

## El problema
Un contrato puede compilar y aun así ser difícil de integrar. Quien consume FreelanceEscrow necesita saber qué funciones, errores y eventos son públicos, qué selector corresponde a una firma y qué artefactos genera el compilador.

## Concepto
La ABI es una frontera versionable; el bytecode es lo que ejecuta la EVM; los artefactos de Foundry conectan ambos mundos. Una modificación aparentemente pequeña en una firma pública puede romper integraciones aunque la lógica interna siga siendo correcta.

## Demostración
[DEMO] Desde la raíz del curso:

```bash
forge build
forge inspect FreelanceEscrow abi
forge inspect FreelanceEscrow bytecode
cast sig "release()"
cast sig "InvalidState(uint8,uint8)"
```

Compara lo que aparece en ABI con el código de [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol).

## Código real
Identifica en la ABI las funciones `markDelivered`, `release` y `refund`, los getters públicos, eventos y custom errors. Explica qué cambio sería compatible para un integrador y cuál exigiría tratar una nueva versión como contrato diferente.

## Qué acaba de pasar
Dejaste de mirar sólo el archivo `.sol`: ahora puedes inspeccionar el contrato compilado y razonar sobre la superficie que realmente consumen otras herramientas.

## Errores comunes
- Confundir ABI con bytecode.
- Cambiar firmas públicas sin pensar en consumidores.
- Versionar `out/` como si fuera fuente de verdad.
- Suponer que una compilación verde prueba compatibilidad de integración.
- Copiar un selector sin saber de qué firma proviene.

## Buenas prácticas
Mantén la superficie pública pequeña, nombres estables y errores/eventos intencionales. Usa inspección reproducible antes de documentar una integración.

## Tu turno
[PAUSA PARA EJERCICIO] Obtén la ABI y el selector de `refund()`. Después explica qué tendría que cambiar un cliente si `refund()` pasara a recibir un argumento.

## Cómo comprobar
```bash
forge inspect FreelanceEscrow abi
cast sig "refund()"
```

Tu explicación debe diferenciar cambio de implementación y cambio de contrato público.

## Solución enlazada
No hay una única línea de código como solución: contrasta tu razonamiento con la ABI generada y la especificación oficial enlazada abajo.

## Reto adicional
Genera el selector de cada custom error y explica por qué un test puede necesitar comparar el error ABI completo cuando contiene argumentos.

## Resumen
- la ABI es frontera de integración;
- el bytecode es el artefacto ejecutable;
- selectors dependen de firmas canónicas;
- una compilación verde no garantiza compatibilidad pública;
- Foundry permite inspeccionar la superficie real.

## Siguiente paso
Continúa con [Lección 14 — Diagnóstico con traces](14-diagnostico-con-traces.md).

## Referencias
- [Solidity ABI Specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
- [Foundry — forge inspect](https://getfoundry.sh/reference/forge/forge-inspect)
- [Foundry — cast sig](https://getfoundry.sh/reference/cast/cast-sig)
