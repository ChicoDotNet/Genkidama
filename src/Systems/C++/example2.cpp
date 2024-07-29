#include <iostream>

// Abstract Product
class Database {
public:
    virtual void connect() = 0;
    virtual void query() = 0;
};

// Concrete Product
class Postgres : public Database {
public:
    void connect() override {
        std::cout << "Connecting to PostgreSQL" << std::endl;
    }
    void query() override {
        std::cout << "Querying PostgreSQL" << std::endl;
    }
};

class MySQL : public Database {
public:
    void connect() override {
        std::cout << "Connecting to MySQL" << std::endl;
    }
    void query() override {
        std::cout << "Querying MySQL" << std::endl;
    }
};

// Abstract Factory
class DatabaseFactory {
public:
    virtual Database* createDatabase() = 0;
};

// Concrete Factory
class PostgresFactory : public DatabaseFactory {
public:
    Database* createDatabase() override {
        return new Postgres();
    }
};

class MySQLFactory : public DatabaseFactory {
public:
    Database* createDatabase() override {
        return new MySQL();
    }
};

// Usage
void useDatabase(DatabaseFactory* factory) {
    Database* db = factory->createDatabase();
    db->connect();
    db->query();
    delete db;
}

int main() {
    DatabaseFactory* postgresFactory = new PostgresFactory();
    DatabaseFactory* mysqlFactory = new MySQLFactory();
    useDatabase(postgresFactory);
    useDatabase(mysqlFactory);
    delete postgresFactory;
    delete mysqlFactory;
    return 0;
}
