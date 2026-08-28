Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $pending=[System.Collections.Generic.List[object]]::new(); $store=@{}
  $register={param($entity)$pending.Add($entity)}
  $commit={foreach($entity in @($pending)){$store[$entity.Id]=$entity};$pending.Clear()}
  & $register ([pscustomobject]@{Id=7;Name='Ada'}); & $commit
  if($store[7].Name -ne 'Ada' -or $pending.Count -ne 0){throw 'Unit of Work commit failed.'}
}