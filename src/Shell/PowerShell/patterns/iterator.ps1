Set-StrictMode -Version Latest
# Iterator: traversal is separated from the collection representation.
$items=@(3,2,1);$seen=@();foreach($item in $items){$seen+=$item};if(($seen-join',')-ne'3,2,1'){throw 'Iterator failed'}
