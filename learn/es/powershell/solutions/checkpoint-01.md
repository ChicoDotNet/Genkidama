# Solución de referencia — Checkpoint 01

Una solución razonable mantiene la regla pura respecto al entorno:

```powershell
function Get-PowerShellVersionFinding {
    [CmdletBinding()]
    param([Parameter(Mandatory)][psobject]$Snapshot)

    $major = ([version]$Snapshot.PowerShellVersion).Major
    $severity = if ($major -ge 7) { 'Info' } else { 'Warning' }

    [pscustomobject]@{
        Code = 'runtime.powershell-version'
        Severity = $severity
        Message = if ($major -ge 7) {
            "PowerShell $($Snapshot.PowerShellVersion) es una línea moderna."
        } else {
            "PowerShell $($Snapshot.PowerShellVersion) es anterior a PowerShell 7; revisa compatibilidad y soporte."
        }
        Evidence = @{ Version = $Snapshot.PowerShellVersion; Major = $major }
    }
}
```

Después se incorpora su resultado al arreglo de hallazgos y se exporta la función sólo si queremos que forme parte del contrato público del módulo.

Una prueba no debe depender de la versión real instalada:

```powershell
It 'marca PowerShell 7 como Info' {
    $snapshot = [pscustomobject]@{ PowerShellVersion = '7.6.4' }
    (Get-PowerShellVersionFinding -Snapshot $snapshot).Severity | Should -Be 'Info'
}
```

La decisión importante no es copiar exactamente este código. Es conservar la separación **observación → regla → reporte** para que un fallo de plataforma no vuelva impredecibles las reglas.

Regresa a [Checkpoint 01](../exercises/checkpoint-01.md) y compara únicamente después de haber intentado tu propia solución.
