Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $sync = [object]::new(); $state=[pscustomobject]@{Value=0}
  [System.Threading.Monitor]::Enter($sync)
  try { $state.Value++ } finally { [System.Threading.Monitor]::Exit($sync) }
  if ($state.Value -ne 1) { throw 'Monitor Object guarded state failed.' }
}