Set-StrictMode -Version Latest
# Active Object: method requests wait in a mailbox before execution.
$mailbox=[System.Collections.Queue]::new();$state=@();$mailbox.Enqueue({$script:state+='done'});&$mailbox.Dequeue();if($state[0]-ne'done'){throw 'Active Object failed'}
