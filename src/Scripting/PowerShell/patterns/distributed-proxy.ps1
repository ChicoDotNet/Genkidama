Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $remote={param($id)"remote:$id"}; $state=[pscustomobject]@{Calls=0}
  $proxy={param($id)$state.Calls++; & $remote $id}
  $result=& $proxy 42
  if($result -ne 'remote:42' -or $state.Calls -ne 1){throw 'Distributed Proxy delegation failed.'}
}