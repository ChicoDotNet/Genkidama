module RepositoryExample
let run () =
    let rows = [ (1, "Ada"); (2, "Grace") ]
    rows
    |> List.find (fun (id, _) -> id = 2)
    |> snd
    |> (=) "Grace"
