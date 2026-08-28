Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $pricing = { param($qty) $qty * 5 }
  $tax = { param($amount) [decimal]($amount * 0.1) }
  $price = & $pricing 2
  $taxDue = & $tax $price
  if ($price -ne 10 -or $taxDue -ne 1) { throw 'Microservice collaboration failed.' }
}