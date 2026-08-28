module ObjectPoolExample
open System.Collections.Generic

let run () =
    let pool = ResizeArray<int>([ 1; 2 ])
    let borrowed = pool[1]
    pool.RemoveAt(1)
    pool.Add(borrowed)
    pool.Count = 2 && pool.Contains(borrowed)
