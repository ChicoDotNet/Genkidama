module MementoExample
let run () =
    let mutable state = "draft"
    let snapshot = state
    state <- "published"
    let published = state = "published"
    state <- snapshot
    published && state = "draft"
