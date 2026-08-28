module VisitorExample
open System

type Shape =
    | Circle of float
    | Rect of float * float

let private area = function
    | Circle radius -> Math.PI * radius * radius
    | Rect (width, height) -> width * height

let run () =
    let total = [ Circle 2.0; Rect (3.0, 4.0) ] |> List.sumBy area
    abs (total - (4.0 * Math.PI + 12.0)) < 1e-9
