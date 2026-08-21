type ServiceProfile = {
    Name: string
    Features: string list
}

let clone profile =
    { profile with Features = List.ofSeq profile.Features }

let describe profile =
    $"{profile.Name}: {System.String.Join(",", profile.Features)}"

let original = {
    Name = "orders"
    Features = ["metrics"]
}

let canary =
    let copy = clone original
    { copy with
        Name = "orders-canary"
        Features = copy.Features @ ["tracing"] }

printfn "original=%s" (describe original)
printfn "clone=%s" (describe canary)
