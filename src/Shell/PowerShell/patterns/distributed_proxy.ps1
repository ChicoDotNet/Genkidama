Set-StrictMode -Version Latest
# Distributed Proxy: local proxy hides a remote lookup boundary.
$remote={param($id)@{id=$id;name='Ada'}};$proxy={param($id)(& $script:remote $id).name};if((&$proxy 7)-ne'Ada'){throw 'Distributed Proxy failed'}
