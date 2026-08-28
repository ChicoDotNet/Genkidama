module ActiveRecordExample
open System.Collections.Generic

let run () =
    let table = Dictionary<int, string>()
    table[7] <- "Ada"
    table[7] = "Ada"
