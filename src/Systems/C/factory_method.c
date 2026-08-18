#include <stdio.h>

typedef struct {
    void (*connect)(void);
    void (*query)(void);
} Database;

typedef Database (*FactoryMethod)(void);

static void postgres_connect(void) { puts("PostgreSQL connect"); }
static void postgres_query(void) { puts("PostgreSQL query"); }
static void mysql_connect(void) { puts("MySQL connect"); }
static void mysql_query(void) { puts("MySQL query"); }

static Database create_postgres(void) {
    Database database = { postgres_connect, postgres_query };
    return database;
}

static Database create_mysql(void) {
    Database database = { mysql_connect, mysql_query };
    return database;
}

static void use_database(FactoryMethod create_database) {
    Database database = create_database();
    database.connect();
    database.query();
}

int main(void) {
    use_database(create_postgres);
    use_database(create_mysql);
    return 0;
}
