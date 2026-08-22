# Lección 07 — Convierte señales en reglas reutilizables

## Qué vas a conseguir
Separarás la obtención de datos de la evaluación de una regla de memoria.

## El problema
Una consulta dice qué observaste; una regla dice qué significa. Si ambas cosas viven juntas, probar umbrales obliga a depender del hardware real.

## Concepto
`Get-MemoryFinding` recibe un snapshot y configuración. La regla puede probarse con objetos pequeños y deterministas.

```powershell
$fixture = [pscustomobject]@{
    Supported = $true
    TotalMemoryBytes = 100
    FreeMemoryBytes = 10
}
Get-MemoryFinding -SystemSnapshot $fixture
```

## Buenas prácticas
- Entrada por objetos, salida por objetos.
- Código/severidad/evidencia estables para automatización.
- Mensaje humano separado de la evidencia estructurada.

## Tu turno
Cambia `MemoryWarningPercent` y demuestra que el mismo fixture cambia de clasificación sin tocar la función.

## Siguiente paso
[Lección 08 — Compón un diagnóstico reproducible](08-compone-un-diagnostico-reproducible.md)

## Referencias
- https://learn.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-pscustomobject
