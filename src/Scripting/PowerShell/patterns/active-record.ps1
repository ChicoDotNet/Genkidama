Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $store=@{}
  $record=[pscustomobject]@{Id=1;Name='Ada';Store=$store}
  $record | Add-Member -MemberType ScriptMethod -Name Save -Value { $this.Store[$this.Id]=[pscustomobject]@{Id=$this.Id;Name=$this.Name} }
  $record.Save()
  if($store[1].Name -ne 'Ada'){throw 'Active Record did not own persistence behavior.'}
}