Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $services = @{ Clock={ [datetime]'2026-01-01' } }
  $locate = { param($name) $services[$name] }
  $clock = & $locate 'Clock'
  $value = & $clock
  if ($value.Date -ne ([datetime]'2026-01-01').Date) { throw 'Service Locator resolution failed.' }
}