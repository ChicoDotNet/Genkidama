Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $receiver = [pscustomobject]@{ Value = 0 }
  $command = { param($r, $delta) $r.Value += $delta }
  & $command $receiver 3
  if ($receiver.Value -ne 3) { throw 'Command did not encapsulate the request.' }
}