#include <stdio.h>

// Abstract Factory
typedef struct {
    void (*connect)();
    void (*query)();
} Database;

// Concrete Implementations
void connect_postgresql() {
    printf("Connecting to PostgreSQL\n");
}

void query_postgresql() {
    printf("Querying PostgreSQL\n");
}

void connect_mysql() {
    printf("Connecting to MySQL\n");
}

void query_mysql() {
    printf("Querying MySQL\n");
}

// Concrete Factories
Database create_postgres_factory() {
    Database db;
    db.connect = connect_postgresql;
    db.query = query_postgresql;
    return db;
}

Database create_mysql_factory() {
    Database db;
    db.connect = connect_mysql;
    db.query = query_mysql;
    return db;
}

// Usage
void use_database(Database db) {
    db.connect();
    db.query();
}

int main() {
    Database postgres = create_postgres_factory();
    Database mysql = create_mysql_factory();

    use_database(postgres);
    use_database(mysql);

    return 0;
}
