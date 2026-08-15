# Lección 14 — Diagnóstico con traces

## Qué vas a conseguir
Vas a diagnosticar una transacción fallida usando una prueba focalizada y el trace de Foundry. Al terminar podrás reducir un fallo a una historia concreta antes de modificar el contrato.

## Antes de empezar
Completa la [Lección 13](13-tooling-y-superficie-profesional.md).

## El problema
Cuando una transacción revierte, cambiar código por intuición es peligroso. En contratos que custodian valor necesitas saber quién llamó, en qué estado estaba el escrow y qué interacción externa ocurrió antes de decidir una corrección.

## Concepto
Un trace es evidencia de ejecución. La estrategia útil es reducir primero el escenario con `--match-test` y aumentar verbosidad sólo donde aporta señal. Los custom errors y estados explícitos hacen el diagnóstico más preciso.

## Demostración
[DEMO] Ejecuta una regresión concreta con trace detallado:

```bash
forge test --match-test testReleaseFailureRevertsStateAndRetainsFunds -vvvv
```

Después compara con:

```bash
forge test --match-test testCannotRefundAfterDelivery -vvvv
```

Busca caller, transición de estado, llamada externa y motivo de revert.

## Código real
Usa [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol). No modifiques producción hasta poder explicar por qué la prueba falla o por qué el revert observado es el comportamiento correcto.

## Qué acaba de pasar
Convertiste un mensaje de fallo en una secuencia reproducible. Esa reducción evita arreglos que sólo esconden el síntoma.

## Errores comunes
- Ejecutar toda la suite con máxima verbosidad desde el principio.
- Confundir el revert esperado de una regresión con un bug.
- Cambiar estado o autorización para conseguir verde.
- Leer sólo la última línea del error e ignorar caller y contexto.
- Depurar contra una red real cuando una prueba local reproduce el problema.

## Buenas prácticas
Empieza por una historia mínima, conserva el caso como regresión y vuelve a ejecutar el gate completo después de la corrección. Un trace ayuda a explicar ejecución; no sustituye el modelo mental de la máquina de estados.

## Tu turno
[PAUSA PARA EJERCICIO] Elige una prueba que espere un custom error. Ejecuta sólo esa prueba con `-vvvv` y escribe tres observaciones: caller, estado previo y condición que provoca el revert.

## Cómo comprobar
```bash
forge test --match-test testCannotRefundAfterDelivery -vvvv
bash tools/verify.sh
```

La suite completa debe seguir verde después de cualquier cambio.

## Solución enlazada
La evidencia es el trace de tu ejecución. No copies una salida fija porque offsets y representación pueden cambiar entre versiones de tooling.

## Reto adicional
Provoca temporalmente una expectativa de revert incorrecta en una rama descartable, usa el trace para localizar la diferencia y restaura la prueba antes de continuar.

## Resumen
- reduce antes de reparar;
- `--match-test` aísla historias;
- `-vvvv` expone el trace;
- caller, estado e interacción explican gran parte de los fallos;
- después del diagnóstico vuelve al gate completo.

## Siguiente paso
Continúa con [Lección 15 — Gas y rendimiento con evidencia](15-gas-y-rendimiento-con-evidencia.md).

## Referencias
- [Foundry — forge test](https://getfoundry.sh/reference/forge/forge-test)
- [Foundry — Traces](https://getfoundry.sh/forge/traces)
