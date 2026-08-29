type expr =
  | Lit of int
  | Var of string
  | Add of expr * expr

let rec eval environment = function
  | Lit value -> value
  | Var name -> List.assoc name environment
  | Add (left, right) -> eval environment left + eval environment right

let () =
  let expression = Add (Var "x", Lit 3) in
  assert (eval [ ("x", 4) ] expression = 7)
