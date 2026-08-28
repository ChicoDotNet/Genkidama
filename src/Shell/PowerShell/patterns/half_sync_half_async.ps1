Set-StrictMode -Version Latest
# Half-Sync/Half-Async: async intake is queued before synchronous processing.
$q=[System.Collections.Queue]::new();'a','b'|ForEach-Object {$q.Enqueue($_)};$done=@();while($q.Count){$done+=$q.Dequeue().ToUpperInvariant()};if(($done-join',')-ne'A,B'){throw 'Half-Sync/Half-Async failed'}
