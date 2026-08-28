Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $peerA={param($m)"A:$m"}; $peerB={param($m)"B:$m"}
  $toB=& $peerB 'from-a'; $toA=& $peerA 'from-b'
  if($toB -ne 'B:from-a' -or $toA -ne 'A:from-b'){throw 'Peer-to-Peer symmetric exchange failed.'}
}