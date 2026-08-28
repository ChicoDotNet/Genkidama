Set-StrictMode -Version Latest
# Service Locator: consumers resolve services from a registry.
$services=@{clock={'12:00'}};if((&$services.clock)-ne'12:00'){throw 'Service Locator failed'}
