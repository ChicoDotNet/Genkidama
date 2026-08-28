Set-StrictMode -Version Latest
# Mediator: colleagues communicate through one coordinator.
$events=@();$mediator={param($sender,$message)$script:events+="$sender:$message"};&$mediator 'checkout' 'paid';if($events[0]-ne'checkout:paid'){throw 'Mediator failed'}
