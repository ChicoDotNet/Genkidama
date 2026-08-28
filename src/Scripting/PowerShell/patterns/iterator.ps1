Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $aggregate = @(2,4,6)
  $iterator = $aggregate.GetEnumerator()
  $seen = [System.Collections.Generic.List[int]]::new()
  while ($iterator.MoveNext()) { $seen.Add($iterator.Current) }
  if (($seen -join ',') -ne '2,4,6') { throw 'Iterator traversal failed.' }
}