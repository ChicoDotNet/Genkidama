Set-StrictMode -Version Latest
# Active Record: domain-shaped value owns its persistence operation.
$table=@{};$record=[pscustomobject]@{id=1;name='Ada'};$save={param($r)$script:table[$r.id]=@{name=$r.name}};&$save $record;if($table[1].name-ne'Ada'){throw 'Active Record failed'}
