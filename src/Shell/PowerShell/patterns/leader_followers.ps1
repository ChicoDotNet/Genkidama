Set-StrictMode -Version Latest
# Leader/Followers: workers rotate responsibility for accepting events.
$workers=@('leader','follower');$events=@('one','two');$handled=for($i=0;$i-lt2;$i++){"$($workers[$i]):$($events[$i])"};if(($handled-join',')-ne'leader:one,follower:two'){throw 'Leader/Followers failed'}
