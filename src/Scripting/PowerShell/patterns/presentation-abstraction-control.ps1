Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $model=[pscustomobject]@{Value=3}
  $abstraction={param($m)$m.Value*2}; $control={param($m)& $abstraction $m}; $presentation={param($v)"value=$v"}
  $value=& $control $model
  if((& $presentation $value) -ne 'value=6'){throw 'PAC collaboration failed.'}
}