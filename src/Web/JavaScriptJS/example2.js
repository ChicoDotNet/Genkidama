// Abstract Factory
function createDatabase(dbType) {
    if (dbType === "postgresql") {
        return new Postgres();
    } else if (dbType === "mysql") {
        return new MySQL();
    }
}

// Concrete Products
class Postgres {
    connect() {
        console.log("Connecting to PostgreSQL");
    }

    query() {
        console.log("Querying PostgreSQL");
    }
}

class MySQL {
    connect() {
        console.log("Connecting to MySQL");
    }

    query() {
        console.log("Querying MySQL");
    }
}

// Usage
const db1 = createDatabase("postgresql");
db1.connect();
db1.query();

const db2 = createDatabase("mysql");
db2.connect();
db2.query();
