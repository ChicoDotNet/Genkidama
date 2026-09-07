module MediatorExample

type Message =
    | PaymentPaid
    | InventoryReserved

type Colleague = Message -> string

type Mediator = Map<string, Colleague>

let payment = function
    | InventoryReserved -> "payment.ack"
    | _ -> "payment.ignore"

let inventory = function
    | PaymentPaid -> "inventory.reserve"
    | _ -> "inventory.ignore"

let send (mediator: Mediator) recipient message =
    match mediator |> Map.tryFind recipient with
    | Some receive -> Ok (receive message)
    | None -> Error "unknown-colleague"

let run () =
    let mediator =
        Map [
            "payment", payment
            "inventory", inventory
        ]

    send mediator "inventory" PaymentPaid = Ok "inventory.reserve"
    && send mediator "payment" InventoryReserved = Ok "payment.ack"
    && send mediator "shipping" PaymentPaid = Error "unknown-colleague"
