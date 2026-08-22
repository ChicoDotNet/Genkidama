type Database =
    { Connect: unit -> unit
      Query: unit -> unit }

let postgresDatabase () =
    { Connect = fun () -> printfn "PostgreSQL connect"
      Query = fun () -> printfn "PostgreSQL query" }

let mySqlDatabase () =
    { Connect = fun () -> printfn "MySQL connect"
      Query = fun () -> printfn "MySQL query" }

let useDatabase (createDatabase: unit -> Database) =
    let database = createDatabase ()
    database.Connect ()
    database.Query ()

useDatabase postgresDatabase
useDatabase mySqlDatabase
