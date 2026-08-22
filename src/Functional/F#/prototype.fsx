type ServiceProfile = {
    Name: string
    Features: string list
}

let clone profile =
    { profile with Features = List.ofSeq profile.Features }

let describe profile =
    let features = System.String.Join(",", profile.Features)
    $"{profile.Name}: {features}"

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
