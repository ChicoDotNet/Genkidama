Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $pool=[System.Collections.Generic.Stack[object]]::new(); $pool.Push([pscustomobject]@{Id=1})
  $acquire={if($pool.Count -eq 0){throw 'Pool exhausted'};$pool.Pop()}; $release={param($item)$pool.Push($item)}
  $item=& $acquire; & $release $item
  if($item.Id -ne 1 -or $pool.Count -ne 1){throw 'Object Pool lifecycle failed.'}
}