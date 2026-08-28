Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $queue = [System.Collections.Generic.Queue[object]]::new()
  $state = [pscustomobject]@{ Value=0 }
  $queue.Enqueue({ $state.Value += 1 }); $queue.Enqueue({ $state.Value += 2 })
  while($queue.Count -gt 0){ & $queue.Dequeue() }
  if ($state.Value -ne 3) { throw 'Active Object request queue failed.' }
}