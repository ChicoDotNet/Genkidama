Set-StrictMode -Version Latest
# Microkernel: a minimal core delegates extension behavior to plugins.
$plugins=@{upper={param($text)$text.ToUpperInvariant()}};if((&$plugins.upper 'plugin')-ne'PLUGIN'){throw 'Microkernel failed'}
