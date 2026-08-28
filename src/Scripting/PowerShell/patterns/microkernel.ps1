Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $plugins = @{}
  $register = { param($name,$plugin) $plugins[$name]=$plugin }
  & $register 'upper' { param($text) $text.ToUpperInvariant() }
  $result = & $plugins.upper 'kernel'
  if ($result -ne 'KERNEL') { throw 'Microkernel plugin extension failed.' }
}