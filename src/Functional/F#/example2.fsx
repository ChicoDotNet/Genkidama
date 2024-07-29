type IDatabase =
    abstract member Connect: unit -> unit
    abstract member Query: unit -> unit

type Postgres() =
    interface IDatabase with
        member _.Connect() = printfn "Connecting to PostgreSQL"
        member _.Query() = printfn "Querying PostgreSQL"

type MySQL() =
    interface IDatabase with
        member _.Connect() = printfn "Connecting to MySQL"
        member _.Query() = printfn "Querying MySQL"

type IDatabaseFactory =
    abstract member CreateDatabase: unit -> IDatabase

type PostgresFactory() =
    interface IDatabaseFactory with
        member _.CreateDatabase() = Postgres() :> IDatabase

type MySQLFactory() =
    interface IDatabaseFactory with
        member _.CreateDatabase() = MySQL() :> IDatabase

let useDatabase (factory: IDatabaseFactory) =
    let db = factory.CreateDatabase()
    db.Connect()
    db.Query()

// Usage
useDatabase (PostgresFactory() :> IDatabaseFactory)
useDatabase (MySQLFactory() :> IDatabaseFactory)
