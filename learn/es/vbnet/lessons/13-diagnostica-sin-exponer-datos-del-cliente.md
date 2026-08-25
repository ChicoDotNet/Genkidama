# Lección 13 — Diagnostica sin exponer datos del cliente

## Qué vas a conseguir

Vas a crear un snapshot operacional útil para soporte sin incluir nombres de clientes ni descripciones de partidas.

## El problema

Cuando una aplicación falla en campo necesitas señales para diagnosticarla. Copiar documentos completos a un ticket de soporte puede filtrar información que no hace falta para entender el estado operativo.

## Concepto

`QuoteDiagnostics` reutiliza el catálogo y produce sólo contadores y totales agregados. La frontera es read-only: observar no autoriza a modificar, reparar ni borrar archivos.

## Código real

Ver [QuoteDiagnostics.vb](../app/QuoteDesk.Core/QuoteDiagnostics.vb).

## Tu turno

Prepara dos cotizaciones y un archivo corrupto; inspecciona el directorio y verifica los conteos sin imprimir datos del cliente.

## Siguiente paso

Continúa con [Lección 14 — Depura con evidencia reproducible](14-depura-con-evidencia-reproducible.md).

## Referencias
- [Logging guidance for .NET](https://learn.microsoft.com/dotnet/core/extensions/logging)
