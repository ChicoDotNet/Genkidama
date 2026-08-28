Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $plugins = @{}
  $register = { param($name,$plugin) $plugins[$name]=$plugin }
  & $register 'upper' { param($text) $text.ToUpperInvariant() }
  $plugin = $plugins['upper']
  $result = & $plugin 'kernel'
  if ($result -ne 'KERNEL') { throw 'Microkernel plugin extension failed.' }
}