Set-StrictMode -Version Latest
# Unit of Work: changes accumulate and commit as one explicit batch.
$pending=[System.Collections.ArrayList]@(@{id=1});$db=@();$db+=@($pending);$pending.Clear();if($db[0].id-ne1-or$pending.Count-ne0){throw 'Unit of Work failed'}
