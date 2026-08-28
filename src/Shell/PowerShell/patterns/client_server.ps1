Set-StrictMode -Version Latest
# Client-Server: client sends a request through a server contract.
$server={param($request)@{echo=$request}};$client={param($v)(& $script:server $v).echo};if((&$client 'ping')-ne'ping'){throw 'Client-Server failed'}
