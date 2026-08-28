Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $asyncIngress=[System.Collections.Generic.Queue[int]]::new(); $asyncIngress.Enqueue(1); $asyncIngress.Enqueue(2)
  $syncResults=[System.Collections.Generic.List[int]]::new()
  while($asyncIngress.Count -gt 0){ $syncResults.Add($asyncIngress.Dequeue()*10) }
  if (($syncResults -join ',') -ne '10,20') { throw 'Half-Sync/Half-Async handoff failed.' }
}