[CmdletBinding()]
param(
    [string]$OutputPath,
    [string]$TextOutputPath,
    [string]$CompareWith
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
Import-Module (Join-Path $PSScriptRoot 'WorkstationAudit.psd1') -Force

try {
    $audit = Get-WorkstationAudit
    if ($OutputPath) {
        $file = $audit | Export-WorkstationAudit -Path $OutputPath
        Write-Host "Reporte JSON: $($file.FullName)"
    }
    if ($TextOutputPath) {
        $file = $audit | Export-WorkstationAuditText -Path $TextOutputPath
        Write-Host "Reporte texto: $($file.FullName)"
    }
    $audit.Findings | Select-Object Severity, Code, Message | Format-Table -AutoSize
    Write-Host "Hallazgos: $($audit.Summary.TotalFindings); críticos: $($audit.Summary.Critical); advertencias: $($audit.Summary.Warning)"
    if ($CompareWith) {
        $baseline = Import-WorkstationAudit -Path $CompareWith
        $comparison = Compare-WorkstationAudit -Baseline $baseline -Current $audit
        Write-Host "Cambios vs baseline: nuevos=$($comparison.Summary.Added); resueltos=$($comparison.Summary.Resolved); severidad=$($comparison.Summary.Changed)"
    }
}
catch {
    Write-Error "WorkstationAudit no pudo completar el diagnóstico: $($_.Exception.ToString())"
    exit 1
}
