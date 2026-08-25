# Lección 03 — Convierte señales en hallazgos

## Qué vas a conseguir

Entenderás funciones avanzadas, parámetros y pipeline mientras conviertes espacio libre en un hallazgo con severidad.

## El problema

Una lista de números no es todavía un diagnóstico. WorkstationAudit necesita expresar una conclusión reproducible: suficiente espacio, advertencia o condición crítica.

## Concepto

Una función con `[CmdletBinding()]` participa en convenciones de cmdlets. `[Parameter(..., ValueFromPipeline)]` permite recibir objetos del pipeline. La regla debe depender de datos de entrada, no de variables globales.

## Demostración

```powershell
[pscustomobject]@{
    Name = 'Demo'
    FreeBytes = 15
    TotalBytes = 100
} | Get-StorageFinding
```

La severidad es `Warning`. Con 5 de 100 será `Critical`; con 25 será `Info`.

## Código real

Revisa `Get-StorageFinding` en `app/WorkstationAudit.psm1`.

La función también trata `TotalBytes <= 0` como `storage.unknown`. No inventa un porcentaje dividiendo entre cero.

## Qué acaba de pasar

Separaste dos responsabilidades:

1. `Get-PlatformSnapshot` observa el sistema.
2. `Get-StorageFinding` interpreta datos ya observados.

Así puedes probar la regla sin necesitar llenar realmente un disco.

## Errores comunes

- Leer el filesystem dentro de la regla que sólo debería clasificar datos.
- Usar `Write-Host` como valor de retorno de una función reutilizable.
- Elegir umbrales sin poder localizarlos o explicarlos después.

## Buenas prácticas

Los hallazgos incluyen `Code`, `Severity`, `Message` y `Evidence`. El mensaje ayuda a humanos; `Code` y `Evidence` permiten automatizar.

## Tu turno

Crea tres objetos de prueba (25%, 15% y 5% libre), pásalos por pipeline a `Get-StorageFinding` y confirma sus severidades.

## Cómo comprobar tu solución

```powershell
$cases | Get-StorageFinding | Select-Object Severity, Message
```

Debes observar `Info`, `Warning` y `Critical` en ese orden si tus casos usan esos porcentajes.

## Solución

Después de intentarlo, revisa los casos equivalentes en `app/tests/WorkstationAudit.Tests.ps1`.

## Reto adicional

Explica por qué `Evidence` contiene números y no sólo una frase ya formateada.

## Resumen

Ya puedes diseñar una función PowerShell que recibe objetos, aplica una regla y produce otro objeto reutilizable.

## Siguiente paso

Continúa con [Lección 04 — Maneja errores y prueba comportamiento](04-errores-y-pruebas.md).

## Referencias

- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_functions_advanced
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters
