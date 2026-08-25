Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$styles = @{}

function Get-Style([string]$Font, [int]$Size, [string]$Color) {
    $key = "$Font|$Size|$Color"
    if (-not $styles.ContainsKey($key)) {
        $styles[$key] = [pscustomobject]@{ Font = $Font; Size = $Size; Color = $Color }
    }
    return $styles[$key]
}

$red1 = Get-Style 'Inter' 12 'red'
$red2 = Get-Style 'Inter' 12 'red'
$null = Get-Style 'Inter' 12 'blue'
$shared = [object]::ReferenceEquals($red1, $red2).ToString().ToLowerInvariant()
"styles=$($styles.Count);shared=$shared;text=ABC"
