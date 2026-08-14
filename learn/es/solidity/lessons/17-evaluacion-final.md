# Lección 17 — Evaluación final sin receta

## Qué vas a conseguir
Vas a demostrar que puedes leer, modificar, probar y explicar FreelanceEscrow sin seguir una receta de archivos y líneas. Esta lección integra el curso; no introduce una dependencia nueva ni un patrón por exhibición.

## Antes de empezar
Completa la [Lección 16](16-hardening-reentrada-y-checkpoint-04.md) y confirma:

```bash
bash tools/verify.sh
```

## El problema
Un equipo quiere incorporar una referencia de proyecto al escrow y cerrar una ambigüedad de identidad entre cliente y freelancer. El cambio parece pequeño, pero toca constructor, ABI, eventos, pruebas y compatibilidad de integración. Debes hacerlo sin romper depósito, entrega, liberación, reembolso ni las regresiones de seguridad.

## Concepto
Una tarea Junior profesional combina **leer → formular → probar → implementar → diagnosticar → verificar → explicar**. El objetivo no es recordar sintaxis: es producir evidencia de que entiendes la máquina de estados y sus fronteras.

## Demostración
[DEMO] Antes de cambiar código, inspecciona:

```bash
forge inspect FreelanceEscrow abi
forge test -vv
forge test --gas-report
```

Explica qué parte del encargo cambia la superficie pública y qué parte sólo endurece una regla interna.

## Código real
Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md). Trabaja sobre la misma aplicación canónica. Puedes consultar documentación oficial, traces y los materiales anteriores; no abras la solución antes de completar un intento.

## Qué acaba de pasar
Ya no estás copiando una implementación conocida. Estás manteniendo un contrato existente con dinero ficticio, pruebas y contratos públicos que debes preservar conscientemente.

## Errores comunes
- Cambiar el constructor sin actualizar todas las pruebas de despliegue.
- Confundir una nueva propiedad pública con estado temporal de una UI.
- Corregir identidad sólo en un script de despliegue y no en el contrato.
- Debilitar las regresiones de transferencia o reentrada para conseguir verde.
- Declarar “seguro” un contrato porque la suite pasa.
- Optimizar gas durante el cambio sin una medición que lo justifique.

## Buenas prácticas
Haz cambios pequeños y coherentes, usa custom errors precisos, mantén la máquina de estados explícita y conserva una prueba por cada regla nueva o defecto corregido. Trata ABI y eventos como contratos con consumidores.

## Tu turno
[PAUSA PARA EJERCICIO] Completa las historias A–E de la evaluación final y prepara una explicación de cinco minutos sobre arquitectura, seguridad, ABI, pruebas y un tradeoff que aceptaste.

## Cómo comprobar
Como mínimo:

```bash
bash tools/verify.sh
forge inspect FreelanceEscrow abi
forge test --gas-report
```

Revisa además el trace de una historia feliz y un revert esperado.

## Solución enlazada
Sólo después de tu intento, compara con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia muestra una dirección posible; no exige que tus líneas sean idénticas.

## Reto adicional
Diseña, sin implementar, cómo soportarías disputas o vencimientos. Identifica qué nuevas transiciones aparecerían, qué actor podría ejecutarlas y qué riesgos introduciría depender del tiempo de bloque.

## Cómo hablar de este proyecto en una entrevista
Cuenta primero el problema: un pago freelance pequeño modelado como máquina de estados. Después explica decisiones: roles inmutables, custom errors, eventos, checks-effects-interactions, pruebas deterministas, fuzzing y colaboradores hostiles. Menciona límites reales: una suite local no sustituye auditoría y el contrato no implementa disputas, oráculos ni upgrades.

Preguntas probables:
- ¿Por qué modelaste estados explícitos?
- ¿Qué diferencia hay entre ABI, bytecode y source?
- ¿Cómo demuestras que una transferencia fallida no deja estado parcial?
- ¿Qué protege la prueba de reentrada y qué no protege?
- ¿Cuándo usarías fuzzing en lugar de otro ejemplo determinista?
- ¿Qué medirías antes de optimizar gas?
- ¿Qué cambiarías antes de custodiar valor real?

## Resumen
Completar el curso significa poder modificar un contrato pequeño, demostrar sus reglas con Foundry y explicar límites de seguridad. La evaluación produce evidencia de competencia inicial; **no garantiza contratación ni certifica el contrato para producción**.

## Siguiente paso
Repite las áreas débiles de la rúbrica, conserva el proyecto como evidencia y construye una variante local propia antes de explorar frameworks o despliegues públicos.

## Referencias
- [Solidity documentation](https://docs.soliditylang.org/en/v0.8.35/)
- [Solidity ABI Specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
- [Solidity Security Considerations](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
- [Foundry — Writing Tests](https://getfoundry.sh/forge/writing-tests)
- [Foundry — Traces](https://getfoundry.sh/forge/traces)
