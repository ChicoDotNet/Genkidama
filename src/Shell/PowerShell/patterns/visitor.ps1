Set-StrictMode -Version Latest
# Visitor: an operation is supplied separately from the visited element.
$node=@{value=5};$visitor={param($n)$n.value*2};if((&$visitor $node)-ne10){throw 'Visitor failed'}
