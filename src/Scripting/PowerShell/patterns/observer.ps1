Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $events = [System.Collections.Generic.List[string]]::new()
  $observers = @({ param($e) $events.Add("audit:$e") }, { param($e) $events.Add("ui:$e") })
  foreach ($observer in $observers) { & $observer 'changed' }
  if ($events.Count -ne 2) { throw 'Observers were not notified.' }
}