Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $subscribers=@({param($e)"audit:$e"},{param($e)"ui:$e"})
  $published=@($subscribers | ForEach-Object { & $_ 'saved' })
  if($published.Count -ne 2 -or $published[1] -ne 'ui:saved'){throw 'Publish-Subscribe fanout failed.'}
}