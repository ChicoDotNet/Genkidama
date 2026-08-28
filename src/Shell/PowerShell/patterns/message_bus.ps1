Set-StrictMode -Version Latest
# Message Bus: producers publish through a bus rather than invoking consumers directly.
$seen=@();$bus=@{paid=@({param($v)$script:seen+=$v})};$bus.paid|ForEach-Object {&$_ 42};if($seen[0]-ne42){throw 'Message Bus failed'}
