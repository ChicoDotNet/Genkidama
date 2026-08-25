# Checkpoint 01 — Primera cotización mantenible

Sin copiar la solución, extiende QuoteDesk para que una partida con precio unitario exactamente `0` sea válida, pero una descripción compuesta sólo por espacios siga siendo rechazada.

## Evidencia

1. Agrega o ajusta una prueba del dominio.
2. Agrega una prueba del presenter con una partida gratuita válida.
3. Ejecuta:

```powershell
dotnet test ../app/QuoteDesk.Tests/QuoteDesk.Tests.vbproj -c Release
```

4. Explica por qué la regla vive en `QuoteLine` y no únicamente en `MainForm`.

Cuando hayas terminado, compara con la [solución de referencia](../solutions/checkpoint-01.md).