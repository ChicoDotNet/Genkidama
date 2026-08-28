Set-StrictMode -Version Latest
# State: behavior transition is represented by explicit current state.
$state='closed';$toggle={if($script:state-eq'closed'){$script:state='open'}else{$script:state='closed'}};&$toggle;if($state-ne'open'){throw 'State failed'}
