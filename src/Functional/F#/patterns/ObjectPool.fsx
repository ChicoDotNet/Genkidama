module ObjectPoolExample
open System.Collections.Generic
let run ()=let pool=ResizeArray<int>([1;2]) in let borrowed=pool[1] in pool.RemoveAt(1);pool.Add borrowed;pool.Count=2&&pool.Contains borrowed
