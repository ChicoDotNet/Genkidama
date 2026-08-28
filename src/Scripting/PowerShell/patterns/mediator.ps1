Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $log = [System.Collections.Generic.List[string]]::new()
  $mediator = { param($sender,$message) $log.Add("$sender->$message") }
  & $mediator 'checkout' 'inventory'
  if ($log[0] -ne 'checkout->inventory') { throw 'Mediator did not coordinate peers.' }
}