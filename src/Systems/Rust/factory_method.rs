trait Database {
    fn connect(&self);
    fn query(&self);
}

struct PostgresDatabase;
impl Database for PostgresDatabase {
    fn connect(&self) { println!("PostgreSQL connect"); }
    fn query(&self) { println!("PostgreSQL query"); }
}

struct MySqlDatabase;
impl Database for MySqlDatabase {
    fn connect(&self) { println!("MySQL connect"); }
    fn query(&self) { println!("MySQL query"); }
}

fn use_database(create: fn() -> Box<dyn Database>) {
    let database = create();
    database.connect();
    database.query();
}

fn create_postgres() -> Box<dyn Database> { Box::new(PostgresDatabase) }
fn create_mysql() -> Box<dyn Database> { Box::new(MySqlDatabase) }

fn main() {
    use_database(create_postgres);
    use_database(create_mysql);
}
