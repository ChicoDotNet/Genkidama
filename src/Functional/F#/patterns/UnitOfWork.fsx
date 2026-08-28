module UnitOfWorkExample
open System.Collections.Generic

let run () =
    let store = ResizeArray<int>()
    let pending = ResizeArray<int>([ 2; 3 ])
    store.AddRange(pending)
    pending.Clear()
    Seq.toList store = [ 2; 3 ] && pending.Count = 0
