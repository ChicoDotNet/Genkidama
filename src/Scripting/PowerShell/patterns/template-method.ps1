Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $template = { param($load,$transform) $value=& $load; & $transform $value }
  $result = & $template { 6 } { param($n) $n * 2 }
  if ($result -ne 12) { throw 'Template Method skeleton failed.' }
}