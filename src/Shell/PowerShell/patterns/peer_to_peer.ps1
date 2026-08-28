Set-StrictMode -Version Latest
# Peer-to-Peer: peers can both send and receive without a central server role.
$peers=@{a=@();b=@()};$send={param($source,$target,$message)$script:peers[$target]+=,"$source:$message"};&$send 'a' 'b' 'hello';if($peers.b[0]-ne'a:hello'){throw 'Peer-to-Peer failed'}
