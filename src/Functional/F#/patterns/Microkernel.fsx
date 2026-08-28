module MicrokernelExample
let run () =
    let plugins = dict [ "double", (fun x -> x * 2); "square", (fun x -> x * x) ]
    plugins["double"] 4 = 8 && plugins["square"] 4 = 16
