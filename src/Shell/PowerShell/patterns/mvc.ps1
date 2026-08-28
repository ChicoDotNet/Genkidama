Set-StrictMode -Version Latest
# MVC: controller mutates model; view reads model.
$model=@{count=0};$controller={$script:model.count++};$view={"count=$($script:model.count)"};&$controller;if((&$view)-ne'count=1'){throw 'MVC failed'}
