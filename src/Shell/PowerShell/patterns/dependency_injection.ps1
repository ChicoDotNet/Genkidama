Set-StrictMode -Version Latest
# Dependency Injection: collaborator is supplied rather than located internally.
$clock={'noon'};$greet={param($dependency)"hello@$(&$dependency)"};if((&$greet $clock)-ne'hello@noon'){throw 'Dependency Injection failed'}
