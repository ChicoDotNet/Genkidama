type database = {
  connect : unit -> unit;
  query : unit -> unit;
}

let create_postgres () = {
  connect = (fun () -> print_endline "PostgreSQL connect");
  query = (fun () -> print_endline "PostgreSQL query");
}

let create_mysql () = {
  connect = (fun () -> print_endline "MySQL connect");
  query = (fun () -> print_endline "MySQL query");
}

let use_database create_database =
  let database = create_database () in
  database.connect ();
  database.query ()

let () =
  use_database create_postgres;
  use_database create_mysql
