Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-PlainMessage {
    { 'alert' }.GetNewClosure()
}

function Add-AuditDecorator([scriptblock]$Inner) {
    { "audit($(& $Inner))" }.GetNewClosure()
}

function Add-EncryptDecorator([scriptblock]$Inner) {
    { "enc($(& $Inner))" }.GetNewClosure()
}

$component = New-PlainMessage
$audit = Add-AuditDecorator $component
$encrypted = Add-EncryptDecorator $component
$stacked = Add-AuditDecorator (Add-EncryptDecorator $component)

"base=$(& $component)"
"audit=$(& $audit)"
"encrypted=$(& $encrypted)"
"stacked=$(& $stacked)"
