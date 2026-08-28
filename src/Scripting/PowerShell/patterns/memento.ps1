Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $originator = @{ Text = 'draft' }
  $memento = $originator.Clone()
  $originator.Text = 'edited'
  $originator = $memento.Clone()
  if ($originator.Text -ne 'draft') { throw 'Memento did not restore state.' }
}