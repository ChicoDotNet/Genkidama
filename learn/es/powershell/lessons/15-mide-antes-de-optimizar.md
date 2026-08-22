# Lección 15 — Mide antes de optimizar

## Qué vas a conseguir
Medirás una operación sin convertir tiempos variables del runner en un gate frágil.

## Concepto
`Measure-AuditOperation` usa `Stopwatch` y devuelve tanto el resultado como `ElapsedMilliseconds`.

```powershell
$measurement = Measure-AuditOperation -Name fleet -Operation {
    Get-AuditFleetSummary -Path ./reports/*.json -ThrottleLimit 4
}
$measurement
```

Una medición responde «¿qué observé aquí?». No prueba que una implementación sea universalmente más rápida.

## Debugging
Cuando algo falla, conserva `Set-StrictMode -Version Latest`, revisa la excepción interna y reduce el caso a un fixture reproducible. No escondas el problema con `SilentlyContinue` salvo que la ausencia sea parte explícita del contrato.

## Tu turno
Mide la lectura secuencial y paralela del mismo fixture. Registra el resultado sin afirmar que una ejecución aislada constituye un benchmark.

## Siguiente paso
Continúa con [Lección 16 — Hardening y contrato de entrega](16-hardening-y-entrega.md).

## Referencias
- https://learn.microsoft.com/dotnet/api/system.diagnostics.stopwatch
- https://learn.microsoft.com/powershell/module/microsoft.powershell.utility/measure-command
