# Solución de referencia — Checkpoint 04

Una solución razonable conserva tres ideas: **opt-in para remoting**, **fan-out acotado con resultados equivalentes** y **contratos estructurados antes de automatizar decisiones**.

```powershell
Resolve-AuditTarget localhost
Resolve-AuditTarget server-01 -AllowRemote

$serial = Get-AuditFleetSummary -Path ./fixtures/*.json -ThrottleLimit 1
$parallel = Get-AuditFleetSummary -Path ./fixtures/*.json -ThrottleLimit 2

$serial.TotalFindings -eq $parallel.TotalFindings

Measure-AuditOperation -Name serial -Operation {
    Get-AuditFleetSummary -Path ./fixtures/*.json -ThrottleLimit 1
}

$report = Import-WorkstationAudit ./fixtures/critical.json
$report | Test-WorkstationAuditContract
Get-WorkstationAuditExitCode -Audit $report
```

No hay una expectativa de que `ThrottleLimit 2` gane siempre. La diferencia depende de tamaño de archivos, almacenamiento, CPU, caché y carga del sistema. La prueba importante compara **semántica**, no milisegundos.

Tampoco se habilita remoting desde WorkstationAudit. Si una organización decide usar WinRM/SSH, esa configuración se gobierna por separado.
