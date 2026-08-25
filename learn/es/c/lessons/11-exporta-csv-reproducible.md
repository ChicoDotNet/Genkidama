# Lección 11 — Exporta CSV de forma reproducible

## Qué vas a conseguir

Exportarás una consulta a CSV determinista sin convertir CSV en la fuente canónica de TelemetryTape.

## El problema

El binario es compacto y versionado, pero muchas herramientas de análisis entienden texto tabular. Necesitamos interoperabilidad sin duplicar reglas de filtrado.

## Concepto

`telemetry_export_csv` valida primero el archivo fuente y después reutiliza `telemetry_query_file`. Así sensor, tiempo y parsing conservan un solo contrato.

La salida fija columnas y orden:

```text
timestamp_ms,sensor_id,value_milli,status
```

Los campos son enteros, por lo que no dependen de separadores decimales del locale. Los registros conservan el orden físico del `.gtl`.

[DEMO]

```bash
./app/build/telemetry_cli export sample.gtl report.csv '*' 1000 3000
cat report.csv
```

## Errores comunes

- exportar antes de validar el origen y dejar un CSV que aparenta éxito;
- reimplementar filtros dentro del exportador;
- cambiar silenciosamente el orden entre ejecuciones.

## Tu turno

Exporta dos veces el mismo filtro y compara los archivos byte a byte con la herramienta de tu sistema.

## Siguiente paso

Continúa con [Lección 12 — No prolongues un archivo corrupto](12-no-prolongues-un-archivo-corrupto.md).
