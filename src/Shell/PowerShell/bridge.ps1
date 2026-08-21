Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-Device([string]$Name) {
    @{
        TurnOn = { "${Name}:on" }.GetNewClosure()
        Mute   = { "${Name}:muted" }.GetNewClosure()
    }
}

function Invoke-BasicRemote($Device) { & $Device.TurnOn }
function Invoke-MuteRemote($Device) { & $Device.Mute }

$tv = New-Device 'TV'
$radio = New-Device 'Radio'
"basic-tv=$(Invoke-BasicRemote $tv)"
"basic-radio=$(Invoke-BasicRemote $radio)"
"mute-tv=$(Invoke-MuteRemote $tv)"
"mute-radio=$(Invoke-MuteRemote $radio)"
