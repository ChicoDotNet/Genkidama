open System.Collections.Generic

type IDocumentStore =
    abstract member Get: int -> string

type RemoteDocumentStore() =
    let mutable fetchCount = 0
    member _.FetchCount = fetchCount
    interface IDocumentStore with
        member _.Get id =
            fetchCount <- fetchCount + 1
            $"doc({id})"

type DocumentStoreProxy() =
    let mutable backend: RemoteDocumentStore option = None
    let cache = Dictionary<int, string>()

    member _.BackendCount = if backend.IsSome then 1 else 0
    member _.FetchCount = backend |> Option.map (fun value -> value.FetchCount) |> Option.defaultValue 0

    member _.Get id =
        match cache.TryGetValue id with
        | true, value -> value
        | false, _ ->
            let real =
                match backend with
                | Some value -> value
                | None ->
                    let value = RemoteDocumentStore()
                    backend <- Some value
                    value
            let value = (real :> IDocumentStore).Get id
            cache[id] <- value
            value

    interface IDocumentStore with
        member this.Get id = this.Get id

let store = DocumentStoreProxy()
let first = store.Get 42
let second = store.Get 42
printfn $"backend={store.BackendCount};fetches={store.FetchCount};first={first};second={second}"
