[CmdletBinding()]
param(
    [string]$OutputPath
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

Import-Module (Join-Path $PSScriptRoot 'WorkstationAudit.psd1') -Force

try {
    $audit = Get-WorkstationAudit

    if ($OutputPath) {
        $file = $audit | Export-WorkstationAudit -Path $OutputPath
        Write-Host "Reporte: $($file.FullName)"
    }

    $audit.Findings |
        Select-Object Severity, Code, Message |
        Format-Table -AutoSize

    Write-Host "Hallazgos: $($audit.Summary.TotalFindings); críticos: $($audit.Summary.Critical); advertencias: $($audit.Summary.Warning)"
}
catch {
    Write-Error "WorkstationAudit no pudo completar el diagnóstico: $($_.Exception.Message)"
    exit 1
}
