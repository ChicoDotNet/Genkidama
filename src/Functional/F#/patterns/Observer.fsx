module ObserverExample
let run ()=[(fun i->$"audit:{i}");(fun i->$"dashboard:{i}")]|>List.map(fun f->f 42)|>String.concat ">"="audit:42>dashboard:42"
