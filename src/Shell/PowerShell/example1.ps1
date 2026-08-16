Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-UiFactory {
    param(
        [Parameter(Mandatory)]
        [scriptblock]$CreateButton,

        [Parameter(Mandatory)]
        [scriptblock]$CreateCheckbox
    )

    return @{
        CreateButton = $CreateButton
        CreateCheckbox = $CreateCheckbox
    }
}

$DarkFactory = New-UiFactory `
    -CreateButton { 'Dark Button' } `
    -CreateCheckbox { 'Dark Checkbox' }

$LightFactory = New-UiFactory `
    -CreateButton { 'Light Button' } `
    -CreateCheckbox { 'Light Checkbox' }

function Show-UiComponents {
    param(
        [Parameter(Mandatory)]
        [hashtable]$Factory
    )

    & $Factory.CreateButton
    & $Factory.CreateCheckbox
}

Show-UiComponents -Factory $DarkFactory
Show-UiComponents -Factory $LightFactory
