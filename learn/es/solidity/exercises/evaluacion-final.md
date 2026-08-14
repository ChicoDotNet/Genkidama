# Evaluación final — Evoluciona FreelanceEscrow sin receta

Trabaja sobre la aplicación canónica y usa sólo la EVM local. No abras la solución antes de completar un intento.

## Historia A — Referencia de proyecto

El equipo necesita asociar cada escrow con una referencia inmutable de proyecto.

Agrega una referencia `bytes32` al contrato con estas reglas:

- se recibe durante el despliegue;
- no puede ser `bytes32(0)`;
- permanece inmutable;
- puede consultarse desde la ABI;
- el despliegue emite un evento que permita indexar la creación del escrow con referencia, cliente, freelancer y depósito;
- agrega pruebas para valor válido y referencia vacía.

No se prescribe el nombre exacto de cada variable o helper, pero la frontera pública debe ser clara.

## Historia B — Bug de identidad

Hoy el mismo actor puede terminar representando cliente y freelancer. Define e implementa una política explícita para impedir esa ambigüedad en nuevos escrows.

Debe:

- rechazarse dentro del contrato, no sólo en un script;
- usar un error preciso;
- incluir una prueba de regresión;
- no debilitar `EmptyDeposit()` ni las reglas de estado existentes.

Documenta en una frase por qué esa política simplifica autorización y revisión.

## Historia C — Conserva contratos críticos

Demuestra que siguen funcionando:

- depósito inicial;
- `Funded → Delivered → Released`;
- `Funded → Refunded`;
- sólo freelancer entrega;
- sólo cliente libera/reembolsa;
- transferencia fallida revierte sin dejar estado parcial;
- reentrada durante reembolso no consigue un segundo pago;
- fuzzing de montos positivos;
- formatter, build y suite completa.

No elimines una regresión porque el constructor cambió: actualiza sus fixtures de despliegue.

## Historia D — Consulta documentación oficial

Consulta al menos dos fuentes oficiales, una de Solidity y una de Foundry, relacionadas con decisiones de tu cambio.

Entrega una nota breve con:

1. enlaces;
2. qué verificaste;
3. qué decisión concreta tomaste a partir de cada fuente.

## Historia E — Diseño y operación

Escribe entre 220 y 350 palabras respondiendo:

- ¿por qué la referencia de proyecto debe ser inmutable en este modelo?
- ¿qué consumidores podrían romperse al cambiar el constructor/ABI?
- ¿qué diferencia hay entre esta validación de identidad y una mitigación de reentrada?
- ¿qué amenaza importante sigue fuera del alcance del curso?
- ¿qué medirías antes de optimizar gas?
- ¿qué cambiaría en el diseño si aparecieran disputas con un tercero árbitro?

## Entrega

Entrega código, pruebas, comandos ejecutados, resultado del gate, nota de documentación y respuesta de diseño. Explica también un fallo o hipótesis que hayas diagnosticado durante el trabajo y qué evidencia usaste.

## Comprobación mínima

```bash
bash tools/verify.sh
forge inspect FreelanceEscrow abi
forge test --gas-report
```

Ejecuta además un trace de una historia feliz y de un revert esperado.

Evalúate con [`rubrica-final.md`](rubrica-final.md).

> Esta evaluación usa Ether ficticio local. No despliegues la solución a una red pública ni la presentes como contrato auditado.
