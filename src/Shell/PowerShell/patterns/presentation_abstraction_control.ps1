Set-StrictMode -Version Latest
# PAC: control coordinates abstraction state and presentation.
$abstraction=@{value=1};$control={param($d)$script:abstraction.value+=$d};$presentation={"$($script:abstraction.value)"};&$control 2;if((&$presentation)-ne'3'){throw 'PAC failed'}
