# Lección 01 — Ejecuta tu primer auditor

## Qué vas a conseguir

Ejecutarás PowerShell 7, importarás el módulo de WorkstationAudit y obtendrás un primer diagnóstico real del equipo.

## Antes de empezar

Instala PowerShell 7.6 LTS y comprueba `$PSVersionTable.PSVersion`. No necesitas conocer terminal ni programación previamente.

## El problema

Un diagnóstico manual empieza con preguntas dispersas: ¿qué sistema estoy usando?, ¿qué versión de PowerShell tengo?, ¿qué almacenamiento ve la sesión? Queremos una observación repetible.

## Concepto

PowerShell no mueve sólo texto: sus comandos producen **objetos** con propiedades. Un módulo agrupa comandos reutilizables. `Import-Module` carga esas funciones en la sesión actual.

## Demostración

[EN PANTALLA]

```powershell
$PSVersionTable.PSVersion
Import-Module ./app/WorkstationAudit.psd1 -Force
Get-PlatformSnapshot
```

[EJECUTAR]

```powershell
./app/Invoke-Audit.ps1
```

La aplicación imprime hallazgos de almacenamiento y un resumen. Ya ejecutaste la misma app que evolucionará durante todo el curso.

## Código real

Revisa `app/Invoke-Audit.ps1` y `app/WorkstationAudit.psm1`. No copies el módulo al Markdown: el archivo real es la fuente de verdad.

## Qué acaba de pasar

`Get-PlatformSnapshot` consulta `Get-PSDrive` para filesystem y construye un objeto con nombre del equipo, sistema operativo, versión, momento de captura y unidades visibles.

## Errores comunes

- Ejecutar `powershell.exe` y asumir que es PowerShell 7. Usa `pwsh`.
- Copiar comandos sin mirar la salida.
- Confundir el host de terminal con el lenguaje PowerShell.

## Buenas prácticas

Usa `Set-StrictMode` en scripts mantenibles y evita cambiar configuración del sistema dentro de una herramienta cuyo propósito es auditar.

## Tu turno

[PAUSA PARA EJERCICIO]

Ejecuta `Get-PlatformSnapshot`, guarda el resultado en `$snapshot` y muestra únicamente `ComputerName` y `PowerShellVersion` con `Select-Object`.

## Cómo comprobar tu solución

La salida debe contener exactamente esas dos propiedades y valores reales de tu sesión.

## Solución

Inténtalo primero. Después compara:

```powershell
$snapshot = Get-PlatformSnapshot
$snapshot | Select-Object ComputerName, PowerShellVersion
```

## Reto adicional

Explora `$snapshot.Drives` sin modificar ningún objeto.

## Resumen

Ya sabes ejecutar PowerShell 7, importar un módulo, invocar funciones y observar objetos reales.

## Siguiente paso

Continúa con [Lección 02 — Trabaja con objetos y pipeline](02-objetos-y-pipeline.md).

## Referencias

- https://learn.microsoft.com/powershell/scripting/learn/ps101/01-getting-started
