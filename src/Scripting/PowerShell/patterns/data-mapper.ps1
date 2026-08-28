Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $row=@{customer_id=1;customer_name='Ada'}
  $mapper={param($r)[pscustomobject]@{Id=$r.customer_id;Name=$r.customer_name}}
  $domain=& $mapper $row
  if($domain.Id -ne 1 -or $domain.Name -ne 'Ada'){throw 'Data Mapper translation failed.'}
}