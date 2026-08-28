Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$patternDirectory = Join-Path $PSScriptRoot 'patterns'
$files = @(Get-ChildItem -Path $patternDirectory -Filter '*.ps1' -File | Sort-Object Name)

if ($files.Count -ne 39) {
    throw "Expected 39 canonical PowerShell pattern cells, found $($files.Count)."
}

foreach ($file in $files) {
    & $file.FullName
}

Write-Output "powershell-pattern-sweep: $($files.Count)/39 passed"
