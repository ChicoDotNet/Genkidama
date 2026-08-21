Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-ServiceProfile {
    param(
        [Parameter(Mandatory)] [string] $Name,
        [Parameter(Mandatory)] [string[]] $Features
    )

    [pscustomobject]@{
        Name = $Name
        Features = [System.Collections.Generic.List[string]]::new([string[]]$Features)
    }
}

function Copy-ServiceProfile {
    param([Parameter(Mandatory)] $Profile)

    New-ServiceProfile -Name $Profile.Name -Features @($Profile.Features)
}

function Format-ServiceProfile {
    param([Parameter(Mandatory)] $Profile)

    '{0}: {1}' -f $Profile.Name, ($Profile.Features -join ',')
}

$original = New-ServiceProfile -Name 'orders' -Features @('metrics')
$canary = Copy-ServiceProfile -Profile $original
$canary.Name = 'orders-canary'
[void]$canary.Features.Add('tracing')

'original={0}' -f (Format-ServiceProfile $original)
'clone={0}' -f (Format-ServiceProfile $canary)
