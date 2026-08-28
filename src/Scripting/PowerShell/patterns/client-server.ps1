Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $server={param($request)[pscustomobject]@{Status=200;Body="hello:$request"}}
  $client={param($request) & $server $request}
  $response=& $client 'ada'
  if($response.Status -ne 200 -or $response.Body -ne 'hello:ada'){throw 'Client-Server exchange failed.'}
}