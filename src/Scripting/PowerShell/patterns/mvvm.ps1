Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
& {
  $model = [pscustomobject]@{ First='Ada'; Last='Lovelace' }
  $viewModel = [pscustomobject]@{ FullName="$($model.First) $($model.Last)" }
  $view = { param($vm) $vm.FullName }
  if ((& $view $viewModel) -ne 'Ada Lovelace') { throw 'MVVM binding surface failed.' }
}