Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$script:Registry = [pscustomobject]@{ Count = 0 }

function Get-Registry {
    return $script:Registry
}

$first = Get-Registry
$second = Get-Registry
$first.Count++

Write-Output ("same={0}" -f ([object]::ReferenceEquals($first, $second).ToString().ToLowerInvariant()))
Write-Output ("count={0}" -f $second.Count)
