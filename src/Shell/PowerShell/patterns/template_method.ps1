Set-StrictMode -Version Latest
# Template Method: a fixed algorithm surrounds a variable hook.
$render={param($body)"<$(& $body)>"};if((&$render {'sales'})-ne'<sales>'){throw 'Template Method failed'}
