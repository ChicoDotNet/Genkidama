trait Database {
    fn connect(&self);
    fn query(&self);
}

struct Postgres;

impl Database for Postgres {
    fn connect(&self) {
        println!("Connecting to PostgreSQL");
    }

    fn query(&self) {
        println!("Querying PostgreSQL");
    }
}

struct MySQL;

impl Database for MySQL {
    fn connect(&self) {
        println!("Connecting to MySQL");
    }

    fn query(&self) {
        println!("Querying MySQL");
    }
}

trait DatabaseFactory {
    fn create_database(&self) -> Box<dyn Database>;
}

struct PostgresFactory;

impl DatabaseFactory for PostgresFactory {
    fn create_database(&self) -> Box<dyn Database> {
        Box::new(Postgres)
    }
}

struct MySQLFactory;

impl DatabaseFactory for MySQLFactory {
    fn create_database(&self) -> Box<dyn Database> {
        Box::new(MySQL)
    }
}

fn use_database(factory: &dyn DatabaseFactory) {
    let db = factory.create_database();
    db.connect();
    db.query();
}

fn main() {
    let postgres_factory = PostgresFactory;
    let mysql_factory = MySQLFactory;
    use_database(&postgres_factory);
    use_database(&mysql_factory);
}
