# Lección 06 — Resume sin cargar todo el archivo

## Qué vas a conseguir

Generarás un resumen de TelemetryTape mediante streaming: conteo, primera/última marca de tiempo, mínimo, máximo y promedio.

## El problema

Para imprimir todos los registros sí necesitas almacenarlos o procesarlos uno por uno. Para calcular un resumen no necesitas mantener el archivo completo en RAM.

## Concepto

`telemetry_analyze_file` lee un registro, actualiza `telemetry_summary` y descarta ese registro antes de leer el siguiente. El consumo de memoria del algoritmo no crece con el tamaño del archivo.

El promedio se actualiza incrementalmente:

```text
nuevo_promedio = promedio + (valor - promedio) / nuevo_conteo
```

Así evitamos acumular primero una suma potencialmente enorme sólo para dividir al final.

[EJECUTAR]

```bash
./build/telemetry_cli summary sample.gtl
```

Un archivo válido vacío devuelve `record_count = 0`; no inventamos mínimo, máximo o timestamps significativos donde no existen.

## Buenas prácticas

- mantén I/O en la biblioteca y presentación en la CLI;
- no conviertas un resumen en un motivo para duplicar el parser binario;
- prueba el caso vacío además del caso normal.

## Tu turno

Registra tres valores conocidos y verifica manualmente mínimo, máximo y promedio contra `summary`.

## Siguiente paso

[Lección 07 — Distingue versión incompatible de datos truncados](07-distingue-version-y-truncado.md).
