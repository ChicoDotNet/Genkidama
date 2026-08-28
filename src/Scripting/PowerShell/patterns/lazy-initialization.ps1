Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $holder=[pscustomobject]@{Created=0;Value=$null}
  $get={if($null -eq $holder.Value){$holder.Created++;$holder.Value=[pscustomobject]@{Value=7}};$holder.Value}
  $first=& $get; $second=& $get
  if($first.Value -ne 7 -or -not [object]::ReferenceEquals($first,$second) -or $holder.Created -ne 1){throw 'Lazy Initialization caching failed.'}
}