Set-StrictMode -Version Latest
# Monitor Object: access to mutable state goes through a synchronized boundary.
$gate=[object]::new();$counter=0;[System.Threading.Monitor]::Enter($gate);try{$counter++}finally{[System.Threading.Monitor]::Exit($gate)};if($counter-ne1){throw 'Monitor Object failed'}
