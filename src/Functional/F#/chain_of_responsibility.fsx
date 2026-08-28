type Handler = {
    Name: string
    CanHandle: int -> bool
    Next: Handler option
}

let rec handle handler amount visited =
    let visitedNow = visited @ [handler.Name]
    if handler.CanHandle amount then
        handler.Name, visitedNow
    else
        match handler.Next with
        | Some next -> handle next amount visitedNow
        | None -> failwith "No handler accepted the request."

let escalation = {
    Name = "escalation"
    CanHandle = fun _ -> true
    Next = None
}

let billing = {
    Name = "billing"
    CanHandle = fun amount -> amount <= 500
    Next = Some escalation
}

let faq = {
    Name = "faq"
    CanHandle = fun amount -> amount <= 50
    Next = Some billing
}

let handled, visited = handle faq 250 []
printfn "visited=%s;handled=%s;result=refund(250)" (System.String.Join(">", visited)) handled
