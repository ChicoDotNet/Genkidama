module MicrokernelExample
let run () =
    let plugins =
        dict [
            "double", (fun value -> value * 2)
            "square", (fun value -> value * value)
        ]
    plugins["double"] 4 = 8 && plugins["square"] 4 = 16
