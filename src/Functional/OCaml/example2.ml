module type Database = sig
  val connect : unit -> unit
  val query : unit -> unit
end

module Postgres : Database = struct
  let connect () = print_endline "Connecting to PostgreSQL"
  let query () = print_endline "Querying PostgreSQL"
end

module MySQL : Database = struct
  let connect () = print_endline "Connecting to MySQL"
  let query () = print_endline "Querying MySQL"
end

module type DatabaseFactory = sig
  val create_database : unit -> (module Database)
end

module PostgresFactory : DatabaseFactory = struct
  let create_database () = (module Postgres : Database)
end

module MySQLFactory : DatabaseFactory = struct
  let create_database () = (module MySQL : Database)
end

let use_database (factory : (module DatabaseFactory)) =
  let module F = (val factory : DatabaseFactory) in
  let module D = (val F.create_database () : Database) in
  D.connect ();
  D.query ()

(* Usage *)
let () =
  use_database (module PostgresFactory);
  use_database (module MySQLFactory)
