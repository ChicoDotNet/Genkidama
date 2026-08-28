Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $inventory = { param($id) $id -eq 9 }
  $billing = { param($id) "paid:$id" }
  $facade = { param($id) if (& $inventory $id) { & $billing $id } else { 'unavailable' } }
  if ((& $facade 9) -ne 'paid:9') { throw 'Enterprise Facade orchestration failed.' }
}