module StateExample

type Gate =
    | Locked
    | Unlocked

let private transition state action =
    match state, action with
    | Locked, "unlock" -> Unlocked
    | Unlocked, "lock" -> Locked
    | _ -> state

let run () =
    transition (transition Locked "unlock") "lock" = Locked
