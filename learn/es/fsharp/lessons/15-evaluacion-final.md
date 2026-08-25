# Lección 15 — Evaluación final: extiende QuoteRules sin receta

## Qué vas a conseguir

Vas a demostrar que puedes leer, modificar, probar y explicar QuoteRules sin seguir instrucciones paso a paso.

## Regla de la evaluación

Trabaja primero sin consultar la solución de referencia. Puedes usar la documentación oficial de F# y .NET. El objetivo no es memorizar sintaxis: es resolver un cambio razonable sobre código existente.

## Encargo

Un cliente pide que QuoteRules pueda marcar una cotización con un **código de referencia opcional** proveniente de la entrada y que el reporte lo muestre cuando exista, sin modificar los cálculos de precios.

Debes entregar una solución que cubra estas siete capacidades:

1. **Lectura de código:** identifica qué módulos participan desde entrada hasta reporte.
2. **Funcionalidad nueva:** incorpora el código de referencia sin convertirlo en una cadena global sin dueño.
3. **Bugfix:** localiza y corrige un defecto que tú mismo introduzcas de forma controlada en una rama o copia de trabajo; documenta cómo lo detectaste.
4. **Manejo de errores:** rechaza una referencia vacía o inválida con un error explícito antes de persistir.
5. **Prueba nueva:** protege al menos un caso feliz y un failure mode relevantes.
6. **Documentación oficial:** cita una página oficial que hayas consultado y explica qué decisión desbloqueó.
7. **Diseño de mejora:** propone una siguiente extensión pequeña y justifica en qué módulo viviría.

No se prescribe el nombre exacto del tipo, función o archivo. Debes elegir una forma compatible con el estilo funcional-first ya presente.

## Restricciones

- `Pricing` no debe depender del filesystem.
- El cálculo monetario existente no debe cambiar por la nueva referencia.
- No ocultes errores con valores por defecto silenciosos.
- El reporte debe continuar siendo determinista.
- No agregues una dependencia externa si la biblioteca estándar resuelve el problema.
- La suite existente debe seguir pasando.

## Evidencia mínima

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
```

Incluye además una ejecución de CLI que demuestre la referencia válida y otra que demuestre el error elegido.

## Entrega escrita breve

En cinco a diez líneas explica:

- dónde modelaste la referencia y por qué;
- qué contrato protege tu prueba más importante;
- qué failure mode decidiste manejar;
- qué documentación oficial consultaste;
- qué mejorarías después.

## Material de evaluación

Usa el enunciado independiente en [exercises/evaluacion-final.md](../exercises/evaluacion-final.md). No abras la solución hasta terminar tu primer intento.

## Siguiente paso

La lección 16 te permite calificar el resultado con una rúbrica observable y comparar decisiones con una solución de referencia.

[Anterior](14-depuracion-con-evidencia.md) · [Siguiente](16-rubrica-y-solucion.md)

## Referencias

- [F# types](https://learn.microsoft.com/dotnet/fsharp/language-reference/fsharp-types)
- [F# options](https://learn.microsoft.com/dotnet/fsharp/language-reference/options)
- [F# results](https://learn.microsoft.com/dotnet/fsharp/language-reference/results)
