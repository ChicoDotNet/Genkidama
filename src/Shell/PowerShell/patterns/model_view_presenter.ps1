Set-StrictMode -Version Latest
# MVP: presenter reads model and pushes display state into a passive view.
$model=@{name='Ada'};$view=@{};$presenter={$script:view.text=$script:model.name.ToUpperInvariant()};&$presenter;if($view.text-ne'ADA'){throw 'MVP failed'}
