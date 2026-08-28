Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $strategies = @{ Fast={ param($n) $n * 2 }; Safe={ param($n) $n + 1 } }
  $selected = $strategies.Fast
  $result = & $selected 4
  if ($result -ne 8) { throw 'Strategy selection failed.' }
}