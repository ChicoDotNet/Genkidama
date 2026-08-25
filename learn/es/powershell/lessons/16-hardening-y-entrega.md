# Lección 16 — Hardening y contrato de entrega

## Qué vas a conseguir
Harás explícito qué constituye un reporte válido y cómo una automatización puede interpretar su severidad sin parsear texto humano.

## Concepto
`Test-WorkstationAuditContract` revisa el contrato mínimo y la semántica de severidad. `Get-WorkstationAuditExitCode` traduce el resumen a una convención simple:

- `0`: sin Warning ni Critical.
- `1`: al menos un Warning.
- `2`: al menos un Critical.

```powershell
$audit = Get-WorkstationAudit
$contract = $audit | Test-WorkstationAuditContract
$exitCode = Get-WorkstationAuditExitCode -Audit $audit
```

Un contrato estructurado permite integrar el auditor con CI, RMM o tareas programadas sin buscar palabras dentro de un reporte localizado.

## Hardening
- Mantén StrictMode.
- Falla ante datos corruptos en vez de fabricar defaults silenciosos.
- No escribas secretos en el reporte.
- Conserva la aplicación read-only.
- Documenta las fronteras Windows/no-Windows y local/remoto.

## Tu turno
Construye un fixture con severidad inválida y confirma que el contrato lo rechaza. Después crea resúmenes Info, Warning y Critical y comprueba sus códigos.

## Siguiente paso
Completa [Checkpoint 04](../exercises/checkpoint-04.md) y continúa con [Lección 17 — Evaluación final autónoma](17-evaluacion-final.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_automatic_variables
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_strict_mode
