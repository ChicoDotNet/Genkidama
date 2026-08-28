Set-StrictMode -Version Latest
# MVVM: view-model projects model state into presentation-ready data.
$model=@{first='Ada';last='Lovelace'};$viewModel={"$($script:model.first) $($script:model.last)"};if((&$viewModel)-ne'Ada Lovelace'){throw 'MVVM failed'}
