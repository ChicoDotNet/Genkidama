# Lección 12 — Estrategia de pruebas y checkpoint 03

## Qué vas a conseguir
Vas a combinar pruebas de ejemplo, fuzzing y colaboradores hostiles en una estrategia coherente. Al terminar podrás explicar qué evidencia aporta cada capa y ejecutar un único gate antes de publicar cambios.

## Antes de empezar
Completa la [Lección 11](11-composicion-y-colaboradores-hostiles.md).

## El problema
Una suite puede tener muchos tests y aun así dejar huecos. El objetivo no es cantidad: es cubrir historias críticas, propiedades amplias y fronteras hostiles sin duplicar la misma evidencia.

## Concepto
En FreelanceEscrow usamos tres capas complementarias:

1. **Ejemplos deterministas:** documentan historias concretas.
2. **Fuzzing:** verifica propiedades sobre múltiples montos y callers.
3. **Composición hostil:** demuestra qué ocurre cuando otra dirección ejecuta código y falla.

`bash tools/verify.sh` agrega formato, compilación y toda la suite; CI usa el mismo contrato operativo.

## Demostración
```bash
forge test -vv
bash tools/verify.sh
```

Clasifica cada test existente por la pregunta que responde.

## Código real
La aplicación sigue pequeña en [`../app/src/FreelanceEscrow.sol`](../app/src/FreelanceEscrow.sol). La profundidad adicional vive en [`../app/test/FreelanceEscrow.t.sol`](../app/test/FreelanceEscrow.t.sol), sin contaminar producción con código sólo útil para tests.

## Qué acaba de pasar
Construiste evidencia desde ángulos distintos sobre la misma máquina de estados.

## Errores comunes
- Medir calidad sólo por número de tests.
- Repetir el mismo caso con nombres distintos.
- Introducir fuzzing sin una propiedad.
- Considerar una suite verde equivalente a auditoría.
- Ejecutar comandos distintos en local y CI.

## Buenas prácticas
Mantén un gate único, nombres que expresen comportamiento, regresiones para fallos reales y propiedades para reglas amplias. Documenta límites de la suite.

## Tu turno — Checkpoint 03
Resuelve [`../exercises/checkpoint-03.md`](../exercises/checkpoint-03.md) sin abrir la solución.

## Cómo comprobar
```bash
bash tools/verify.sh
```

Después clasifica cada prueba nueva como ejemplo, propiedad o composición.

## Solución enlazada
Consulta [`../solutions/checkpoint-03.md`](../solutions/checkpoint-03.md) sólo después de tu intento.

## Reto adicional
Propón una invariante stateful que merecería un handler de Foundry y explica qué secuencias generaría.

## Resumen
- capas distintas responden preguntas distintas;
- fuzzing amplía entradas;
- composición prueba colaboradores con código propio;
- el gate local y CI deben compartir contrato;
- verde significa evidencia, no auditoría.

## Siguiente paso
Continúa con [Lección 13 — Tooling y superficie profesional](13-tooling-y-superficie-profesional.md).

## Referencias
- [Foundry — Forge](https://getfoundry.sh/forge/overview)
- [Foundry — Fuzz Testing](https://getfoundry.sh/forge/advanced-testing/fuzz-testing)
- [Foundry — Invariant Testing](https://getfoundry.sh/forge/invariant-testing)
