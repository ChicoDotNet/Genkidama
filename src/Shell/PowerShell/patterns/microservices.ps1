Set-StrictMode -Version Latest
# Microservices: independently scoped services collaborate through a narrow contract.
$inventory={param($sku)@{sku=$sku;available=$true}};$order={param($sku)(& $script:inventory $sku).available};if(-not(&$order 'A-1')){throw 'Microservices failed'}
