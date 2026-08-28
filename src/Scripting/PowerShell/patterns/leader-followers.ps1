Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $jobs=[System.Collections.Generic.Queue[string]]::new(); $jobs.Enqueue('job')
  $workers=@('worker-1','worker-2'); $leader=0; $handled=@()
  $handled += "$($workers[$leader]):$($jobs.Dequeue())"; $leader=1
  if ($handled[0] -ne 'worker-1:job' -or $workers[$leader] -ne 'worker-2') { throw 'Leader/Followers handoff failed.' }
}