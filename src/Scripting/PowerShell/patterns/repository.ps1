Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $store=@([pscustomobject]@{Id=1;Name='Ada'},[pscustomobject]@{Id=2;Name='Grace'})
  $repository=[pscustomobject]@{FindById={param($id)$store | Where-Object Id -eq $id | Select-Object -First 1}}
  $entity=& $repository.FindById 2
  if($entity.Name -ne 'Grace'){throw 'Repository collection abstraction failed.'}
}