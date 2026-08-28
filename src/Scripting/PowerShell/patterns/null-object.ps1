Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $events=[System.Collections.Generic.List[string]]::new()
  $realLogger={param($m)$events.Add($m)}; $nullLogger={param($m)}
  $service={param($logger)& $logger 'done';'done'}
  if((& $service $nullLogger) -ne 'done' -or $events.Count -ne 0){throw 'Null Object did not preserve the collaborator protocol.'}
}