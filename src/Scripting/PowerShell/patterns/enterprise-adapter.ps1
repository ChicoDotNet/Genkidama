Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $legacy = @{ customer_id=7; display_name='Ada' }
  $adapter = { param($row) [pscustomobject]@{ Id=$row.customer_id; Name=$row.display_name } }
  $customer = & $adapter $legacy
  if ($customer.Id -ne 7 -or $customer.Name -ne 'Ada') { throw 'Enterprise Adapter translation failed.' }
}