Set-StrictMode -Version Latest
# Data Mapper: mapper translates storage rows into domain values.
$row=@{name='Ada'};$mapper={param($r)[pscustomobject]@{Name=$r.name}};$user=&$mapper $row;if($user.Name-ne'Ada'){throw 'Data Mapper failed'}
