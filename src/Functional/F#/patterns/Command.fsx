module CommandExample
let run () =
    let commands = [ (fun x -> x + 50); (fun x -> x - 20) ]
    let balance = List.fold (fun value command -> command value) 100 commands
    balance = 130 && commands[1] 150 = 130
