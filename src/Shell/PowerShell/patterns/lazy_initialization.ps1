Set-StrictMode -Version Latest
# Lazy Initialization: resource is created on first demand and then reused.
$calls=0;$value=$null;$get={if($null-eq$script:value){$script:calls++;$script:value=[object]::new()};$script:value};$a=&$get;$b=&$get;if(-not[object]::ReferenceEquals($a,$b)-or$calls-ne1){throw 'Lazy Initialization failed'}
