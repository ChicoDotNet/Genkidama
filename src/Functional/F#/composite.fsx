type Component =
    | File of int
    | Folder of Component list

let rec size component =
    match component with
    | File bytes -> bytes
    | Folder children -> children |> List.sumBy size

let readme = File 2
let docs = Folder [ File 3; File 5 ]
let root = Folder [ readme; docs ]

printfn "leaf=%d" (size readme)
printfn "docs=%d" (size docs)
printfn "root=%d" (size root)
