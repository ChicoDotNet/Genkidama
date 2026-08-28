module ClientServerExample

type Response = { Status: int; Body: string }

let run () =
    let server key =
        if key = "sku-1" then { Status = 200; Body = "stock=7" }
        else { Status = 404; Body = "missing" }
    let response = server "sku-1"
    response.Status = 200 && response.Body = "stock=7"
