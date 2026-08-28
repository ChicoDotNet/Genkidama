Set-StrictMode -Version Latest
# Object Pool: acquire and release reuse a bounded expensive object.
$pool=[System.Collections.Queue]::new();$item=[pscustomobject]@{id=1};$pool.Enqueue($item);$borrowed=$pool.Dequeue();$pool.Enqueue($borrowed);if(-not[object]::ReferenceEquals($item,$pool.Peek())){throw 'Object Pool failed'}
