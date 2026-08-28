Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $implementations = @{ Text={param($v)"value=$v"}; Json={param($v)"{`"value`":$v}"} }
  $abstraction = { param($formatter,$value) & $formatter $value }
  $result = & $abstraction $implementations.Text 4
  if ($result -ne 'value=4') { throw 'Enterprise Bridge delegation failed.' }
}