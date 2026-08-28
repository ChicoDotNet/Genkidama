Set-StrictMode -Version Latest
# Repository: collection-like boundary hides storage representation.
$data=@{1=@{name='Ada'}};$get={param($id)$script:data[$id]};if((&$get 1).name-ne'Ada'){throw 'Repository failed'}
