Set-StrictMode -Version Latest
# Adapter: translate a caller-facing amount into the legacy cents contract.
$legacy={param([int]$cents)$cents};$adapter={param([decimal]$amount)&$script:legacy ([int]($amount*100))};if((&$adapter 12.34)-ne1234){throw 'Adapter failed'}
