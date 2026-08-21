type ProcessRegistry private () =
    let mutable count = 0
    static let instance = ProcessRegistry()
    static member Instance = instance
    member _.Increment() = count <- count + 1
    member _.Count = count

let first = ProcessRegistry.Instance
let second = ProcessRegistry.Instance
first.Increment()
printfn "same=%b" (obj.ReferenceEquals(first, second))
printfn "count=%d" second.Count
