Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $model=[pscustomobject]@{Name='Ada'}; $view=[pscustomobject]@{Text=''}
  $presenter={param($m,$v)$v.Text=$m.Name.ToUpperInvariant()}
  & $presenter $model $view
  if($view.Text -ne 'ADA'){throw 'MVP presenter did not update passive view.'}
}