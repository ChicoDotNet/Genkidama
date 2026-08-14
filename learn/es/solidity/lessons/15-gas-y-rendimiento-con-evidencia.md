# Lección 15 — Gas y rendimiento con evidencia

## Qué vas a conseguir
Vas a medir gas antes de proponer optimizaciones y aprenderás a distinguir ahorro útil de micro-optimización que empeora claridad o seguridad.

## Antes de empezar
Completa la [Lección 14](14-diagnostico-con-traces.md) y conserva la suite verde.

## El problema
En Solidity cada operación ejecutada tiene costo, pero perseguir números pequeños sin contexto puede producir código difícil de revisar. En un escrow sencillo, seguridad y corrección pesan más que ahorrar gas a costa de una máquina de estados confusa.

## Concepto
Optimizar empieza por una línea base reproducible. Foundry puede reportar gas por llamada durante las pruebas. Una mejora sólo es defendible cuando conserva comportamiento, mantiene legibilidad razonable y demuestra un cambio medido.

## Demostración
[DEMO] Ejecuta:

```bash
forge test --gas-report
forge build --sizes
```

Observa `markDelivered`, `release` y `refund`. No compares cifras entre máquinas o versiones como si fueran absolutas; usa el mismo entorno cuando evalúes un cambio.

## Código real
FreelanceEscrow mantiene pocos campos de estado y transiciones explícitas. Antes de modificar almacenamiento o errores, explica cuál es el cuello observado y qué riesgo introduce la optimización.

## Qué acaba de pasar
Pasaste de “esto debería gastar menos” a una conversación basada en una medición reproducible.

## Errores comunes
- Optimizar antes de medir.
- Reducir claridad de autorización o estados por unos cuantos gas.
- Comparar snapshots generados con toolchains distintas.
- Eliminar custom errors o eventos útiles sin medir el costo/beneficio completo.
- Confundir tamaño de bytecode con gas de cada ejecución.

## Buenas prácticas
Conserva primero seguridad, invariantes y pruebas. Mide en el mismo entorno, documenta el escenario y acepta que no toda diferencia merece una refactorización.

## Tu turno
[PAUSA PARA EJERCICIO] Genera un gas report y elige una sola función. Propón una optimización hipotética y escribe qué pruebas deberían protegerla. No la implementes si no puedes explicar el beneficio y el riesgo.

## Cómo comprobar
```bash
forge test --gas-report
bash tools/verify.sh
```

El gate funcional debe seguir siendo la autoridad de corrección.

## Solución enlazada
No existe una cifra objetivo universal. Una respuesta sólida incluye línea base, función observada, hipótesis, riesgo y criterio para aceptar o rechazar el cambio.

## Reto adicional
Compara dos implementaciones sólo en una rama de experimento y explica por qué una diferencia de gas puede no justificar mayor complejidad permanente.

## Resumen
- medir precede a optimizar;
- gas, bytecode y legibilidad son dimensiones distintas;
- seguridad pesa más que micro-ahorros en un escrow;
- compara en entornos equivalentes;
- el comportamiento sigue protegido por pruebas.

## Siguiente paso
Continúa con [Lección 16 — Hardening y checkpoint 04](16-hardening-reentrada-y-checkpoint-04.md).

## Referencias
- [Foundry — Gas Reports](https://getfoundry.sh/forge/gas-reports)
- [Solidity — Gas Optimizations](https://docs.soliditylang.org/en/v0.8.35/internals/optimizer.html)
- [Solidity — Security Considerations](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
