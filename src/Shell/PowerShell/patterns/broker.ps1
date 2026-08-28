Set-StrictMode -Version Latest
# Broker: requests are routed by a broker to registered handlers.
$handlers=@{price={param($sku)9}};$request={param($topic,$payload)&$script:handlers[$topic] $payload};if((&$request 'price' 'A')-ne9){throw 'Broker failed'}
