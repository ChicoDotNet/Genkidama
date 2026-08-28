Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $bus = @{}
  $subscribe = { param($topic,$handler) if(-not $bus.ContainsKey($topic)){$bus[$topic]=[System.Collections.Generic.List[object]]::new()}; $bus[$topic].Add($handler) }
  $events = [System.Collections.Generic.List[string]]::new()
  & $subscribe 'orders' { param($e) $events.Add($e) }
  foreach($handler in $bus.orders){ & $handler 'created' }
  if ($events[0] -ne 'created') { throw 'Message Bus delivery failed.' }
}