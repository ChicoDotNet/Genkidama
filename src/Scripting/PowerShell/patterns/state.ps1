Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $context = [pscustomobject]@{ State='Locked'; Result=$null }
  $states = @{ Locked = { param($c) $c.State='Open'; $c.Result='unlocked' } }
  $handler = $states[$context.State]
  & $handler $context
  if ($context.State -ne 'Open' -or $context.Result -ne 'unlocked') { throw 'State transition failed.' }
}