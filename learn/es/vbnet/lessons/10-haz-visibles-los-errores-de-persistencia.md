# Lección 10 — Haz visibles los errores de persistencia

## Qué vas a conseguir

Vas a distinguir entre “no hay resultados” y “un documento no pudo leerse”.

## El problema

Capturar cualquier excepción y devolver una lista vacía convierte una falla operativa en información falsa. El usuario necesita saber qué documentos fueron válidos y cuáles requieren atención.

## Concepto

`QuoteCatalogResult` separa `Entries` de `Issues`. Un directorio inexistente sigue siendo una falla explícita; un archivo corrupto dentro de un directorio válido se conserva como incidencia por archivo.

## Código real

Ver pruebas: [QuoteCatalogTests.vb](../app/QuoteDesk.Tests/QuoteCatalogTests.vb)

## Buenas prácticas

- No uses `Catch ex As Exception` para ocultar todo.
- Captura sólo fallas que puedes representar de forma honesta.
- Conserva ruta y mensaje suficientes para diagnosticar sin inventar datos.

## Siguiente paso

Continúa con [Lección 11 — Trata JSON como datos externos no confiables](11-trata-json-como-datos-externos-no-confiables.md).

## Referencias
- [Exception handling in Visual Basic](https://learn.microsoft.com/dotnet/visual-basic/language-reference/statements/try-catch-finally-statement)
