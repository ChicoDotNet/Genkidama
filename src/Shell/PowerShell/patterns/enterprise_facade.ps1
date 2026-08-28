Set-StrictMode -Version Latest
# Facade: one operation coordinates multiple subsystem calls.
$stock={$true};$charge={'paid'};$checkout={if(&$script:stock){&$script:charge}else{'sold_out'}};if((&$checkout)-ne'paid'){throw 'Facade failed'}
