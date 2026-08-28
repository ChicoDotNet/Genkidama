Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $nodes = @([pscustomobject]@{Kind='Number';Value=10},[pscustomobject]@{Kind='Text';Value='x'})
  $visitors = @{ Number={param($n)$n.Value*2}; Text={param($n)$n.Value.ToUpperInvariant()} }
  $results = @($nodes | ForEach-Object { & $visitors[$_.Kind] $_ })
  if ($results[0] -ne 20 -or $results[1] -ne 'X') { throw 'Visitor dispatch failed.' }
}