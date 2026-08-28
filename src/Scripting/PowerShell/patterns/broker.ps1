Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $handlers = @{ echo={param($payload)"echo:$payload"} }
  $broker = { param($route,$payload) & $handlers[$route] $payload }
  if ((& $broker 'echo' 'hi') -ne 'echo:hi') { throw 'Broker routing failed.' }
}