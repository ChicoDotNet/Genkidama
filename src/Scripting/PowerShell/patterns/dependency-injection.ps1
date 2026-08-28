Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $clock={ [datetime]'2026-01-01' }
  $service={param($clockDependency) (& $clockDependency).Date }
  $date=& $service $clock
  if($date -ne ([datetime]'2026-01-01').Date){throw 'Dependency Injection collaborator failed.'}
}