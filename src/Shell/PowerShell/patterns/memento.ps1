Set-StrictMode -Version Latest
# Memento: capture state and restore it without exposing restoration logic.
$state=@{text='draft'};$snapshot=$state.Clone();$state.text='edited';$state=$snapshot;if($state.text-ne'draft'){throw 'Memento failed'}
