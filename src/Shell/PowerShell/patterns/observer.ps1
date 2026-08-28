Set-StrictMode -Version Latest
# Observer: subscribers react to a subject notification.
$seen=@();$subscribers=@({param($event)$script:seen+=$event});$subscribers|ForEach-Object {&$_ 'changed'};if($seen[0]-ne'changed'){throw 'Observer failed'}
