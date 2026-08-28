module UnitOfWorkExample
open System.Collections.Generic
let run ()=let store=ResizeArray<int>() in let pending=ResizeArray<int>([2;3]) in store.AddRange pending;pending.Clear();Seq.toList store=[2;3]&&pending.Count=0
