# Lección 14 — Depura con evidencia y endurece el flujo

## Qué vas a conseguir

Vas a diagnosticar fallos de QuoteRules sin adivinar: reproducirás el síntoma, reducirás el caso, localizarás la frontera responsable y dejarás una prueba de regresión cuando el defecto sea reproducible.

## El problema

Una CLI pequeña puede fallar por razones distintas: datos mal formados, una regla de negocio incorrecta, una ruta inválida o una excepción del filesystem. Si todos los fallos se investigan igual, es fácil corregir el lugar equivocado.

## Método de depuración

Trabaja siempre en este orden:

1. **Reproduce** con el comando o test más pequeño posible.
2. **Clasifica** el fallo: parsing, dominio, pricing, rendering o persistencia.
3. **Reduce** la entrada hasta conservar sólo lo necesario para provocar el problema.
4. **Observa** valores y resultados en la frontera implicada.
5. **Corrige** la causa, no el mensaje superficial.
6. **Protege** el contrato con una prueba cuando el comportamiento sea automatizable.

[DEMO] Ejecuta primero la suite completa y después un test individual. Compara el tiempo y la claridad de la señal.

```bash
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
```

## Caso guiado

Supón que un archivo contiene:

```text
Servicio A|2|100.00
Servicio B|dos|50.00
```

No empieces en `Pricing.fs`: la cantidad todavía no es dominio válido. Sigue la entrada hasta `Input.fs` y comprueba que el error se mantiene como `Error` en vez de convertirse en una excepción genérica.

Después prueba un fallo distinto: una ruta de salida que no pueda aceptarse como `OutputFile`. Ese contrato pertenece a la frontera de salida y debe rechazarse antes de intentar escribir.

## Buenas prácticas

- No uses `printfn` permanente como sustituto de una prueba.
- No captures una excepción para devolver éxito silencioso.
- No cambies una regla pura para resolver un problema de I/O.
- Prefiere mensajes que describan la operación que falló sin exponer datos sensibles innecesarios.
- Una prueba de regresión debe demostrar el contrato roto, no copiar la implementación.

## Tu turno

Elige un failure mode ya cubierto por la aplicación. Modifica temporalmente el código para romperlo, confirma que una prueba falla y restáuralo. Después identifica un caso límite razonable todavía no protegido y decide si merece una prueba.

[PAUSA PARA EJERCICIO]

## Cómo comprobar tu solución

La evidencia mínima es:

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
```

Además debes poder explicar qué módulo poseía el defecto y por qué.

## Reto adicional

Investiga en la documentación oficial de F# cómo funciona el pattern matching exhaustivo y explica cómo una unión discriminada puede hacer visible un estado nuevo durante compilación en vez de ocultarlo como cadena.

## Resumen

Depurar no es probar cambios al azar. En QuoteRules las fronteras tipadas y las funciones puras reducen el espacio de búsqueda: primero localizas la responsabilidad y luego proteges el contrato.

## Siguiente paso

La siguiente lección define la evaluación final sin receta paso a paso.

[Anterior](13-checkpoint-persistencia.md) · [Siguiente](15-evaluacion-final.md)

## Referencias

- [F# language reference](https://learn.microsoft.com/dotnet/fsharp/language-reference/)
- [F# pattern matching](https://learn.microsoft.com/dotnet/fsharp/language-reference/pattern-matching)
