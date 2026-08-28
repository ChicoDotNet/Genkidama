Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $document=[pscustomobject]@{Text='hello'}
  $textView={param($d)$d.Text}; $lengthView={param($d)$d.Text.Length}
  if((& $textView $document) -ne 'hello' -or (& $lengthView $document) -ne 5){throw 'Document-View projections failed.'}
}