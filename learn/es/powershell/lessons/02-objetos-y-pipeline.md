# Lección 02 — Trabaja con objetos y pipeline

## Qué vas a conseguir

Usarás propiedades, `Where-Object`, `Select-Object` y el pipeline para consultar el snapshot sin convertirlo prematuramente en texto.

## El problema

Un auditor necesita filtrar y transformar información. Si cada comando imprime strings difíciles de volver a procesar, el reporte se vuelve frágil.

## Concepto

El operador `|` pasa objetos al siguiente comando. Sus propiedades siguen disponibles. El formateo (`Format-Table`) pertenece al borde humano; las funciones de negocio deberían devolver objetos reutilizables.

## Demostración

```powershell
$snapshot = Get-PlatformSnapshot
$snapshot.Drives |
    Where-Object TotalBytes -gt 0 |
    Select-Object Name, Root, FreeBytes, TotalBytes
```

Observa la diferencia entre seleccionar propiedades y formatearlas. `Select-Object` produce nuevos objetos; `Format-Table` prepara visualización.

## Código real

`Get-PlatformSnapshot` usa `ForEach-Object` para convertir cada `PSDriveInfo` en una forma estable para nuestro dominio.

## Qué acaba de pasar

La aplicación desacopla el objeto que entrega el sistema de la estructura que queremos conservar. Ese límite nos permitirá probar reglas con fixtures simples.

## Errores comunes

- Usar `Format-Table` dentro de una función que luego debe exportarse a JSON.
- Parsear la salida visual de un cmdlet cuando el objeto ya expone la propiedad necesaria.
- Suponer que todas las unidades tienen tamaño medible.

## Buenas prácticas

Mantén los objetos hasta el último momento. Da nombres con unidades (`FreeBytes`) en vez de valores ambiguos (`Free`).

## Tu turno

Filtra las unidades cuya capacidad total sea mayor que cero y calcula cuántas son.

## Cómo comprobar tu solución

```powershell
$measurable = @($snapshot.Drives | Where-Object TotalBytes -gt 0)
$measurable.Count
```

El resultado depende de tu máquina; lo importante es que siga siendo un entero obtenido de objetos.

## Solución

La comprobación anterior es una referencia mínima. Prueba también `Measure-Object` y explica cuál te resulta más legible.

## Reto adicional

Usa `Get-Member` sobre un elemento de `Drives` y distingue propiedades de métodos.

## Resumen

El pipeline de PowerShell compone transformaciones de objetos. No necesitas volver todo texto para poder automatizarlo.

## Siguiente paso

Continúa con [Lección 03 — Convierte señales en hallazgos](03-funciones-parametros-y-hallazgos.md).

## Referencias

- https://learn.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-pscustomobject
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_pipelines
