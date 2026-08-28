Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $model = [pscustomobject]@{ Count=1 }
  $controller = { param($m) $m.Count++ }
  $view = { param($m) "Count=$($m.Count)" }
  & $controller $model
  if ((& $view $model) -ne 'Count=2') { throw 'MVC collaboration failed.' }
}