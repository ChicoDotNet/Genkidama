Set-StrictMode -Version Latest
# Command: requests are executable values queued by an invoker.
$balance=100; $commands=@({$script:balance+=50},{$script:balance-=20}); $commands|ForEach-Object { & $_ }; if($balance-ne130){throw 'Command failed'}
