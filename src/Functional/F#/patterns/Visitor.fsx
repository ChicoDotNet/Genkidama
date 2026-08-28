module VisitorExample
open System
type Shape=Circle of float|Rect of float*float
let area=function Circle r->Math.PI*r*r|Rect(w,h)->w*h
let run ()=abs([Circle 2.0;Rect(3.0,4.0)]|>List.sumBy area|>fun total->total-(4.0*Math.PI+12.0))<1e-9
