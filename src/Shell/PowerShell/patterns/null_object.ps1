Set-StrictMode -Version Latest
# Null Object: no-op collaborator preserves the normal collaboration contract.
$nullLogger={param($message)};$service={param($logger)&$logger 'run';'ok'};if((&$service $nullLogger)-ne'ok'){throw 'Null Object failed'}
