# Solución de referencia — Checkpoint 02

```powershell
$config = @{
    StorageWarningPercent = 35
    StorageCriticalPercent = 15
    MemoryWarningPercent = 25
}

$resolved = Resolve-AuditConfiguration -Configuration $config
$drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 20; TotalBytes = 100 }
$system = [pscustomobject]@{ Supported = $true; TotalMemoryBytes = 100; FreeMemoryBytes = 20 }

($drive | Get-StorageFinding -Configuration $resolved).Severity
(Get-MemoryFinding -SystemSnapshot $system -Configuration $resolved).Severity
```

Ambos resultados deben ser `Warning`.

La idea importante no es memorizar los números: es comprobar que política, observación y evaluación permanecen separadas y que un fixture controla el escenario completo.
