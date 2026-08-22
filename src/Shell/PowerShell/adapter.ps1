Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$legacy = [pscustomobject]@{
    ReadFahrenheit = { 86 }
}

$reader = [pscustomobject]@{
    ReadCelsius = {
        $fahrenheit = & $legacy.ReadFahrenheit
        [math]::Round(($fahrenheit - 32) * 5 / 9)
    }.GetNewClosure()
}

Write-Output "legacy=$(& $legacy.ReadFahrenheit)F"
Write-Output "adapted=$(& $reader.ReadCelsius)C"
