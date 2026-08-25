# Lección 05 — Protege reglas con pruebas

## Qué vas a conseguir

Ejecutarás pruebas automatizadas que protegen cálculos, umbrales y failure modes reales de QuoteRules.

## El problema

Una regla de descuento puede romperse con un cambio aparentemente pequeño. Necesitamos ejemplos ejecutables que fallen cuando el contrato cambie sin intención.

## Concepto

El proyecto `QuoteRules.Tests` usa xUnit. Cada prueba protege comportamiento observable: subtotal, descuento estándar, umbral Preferred, validación de cantidad y cotización Partner completa.

[EJECUTAR]

```bash
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj
```

Para recolectar coverage cuando el entorno lo permita:

```bash
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --collect:"XPlat Code Coverage"
```

Coverage es evidencia, no el objetivo. Una prueba debe detectar una regresión real; no añadimos asserts triviales para inflar un porcentaje.

## Tu turno

Agrega una prueba para `Preferred` con subtotal `499m`. Debe confirmar que el descuento sigue siendo `0m` justo debajo del umbral.

[PAUSA PARA EJERCICIO]

Después de intentarlo, compara con las pruebas existentes y conserva el mismo estilo Arrange/Act/Assert implícito por bloques pequeños.

## Checkpoint 1

Sin mirar las lecciones anteriores, explica y demuestra:

1. por qué `CustomerTier` es más seguro que un string libre;
2. por qué `discountRate` es fácil de probar;
3. cómo `Result` diferencia entrada inválida de una cotización válida;
4. qué comando compila y cuál ejecuta pruebas.

## Resumen

Completaste el primer vertical slice: dominio, reglas, errores y pruebas. El siguiente incremento incorporará composición funcional, colecciones más ricas y entrada desde datos externos sin abandonar QuoteRules.

[Anterior](04-errores-result.md)
