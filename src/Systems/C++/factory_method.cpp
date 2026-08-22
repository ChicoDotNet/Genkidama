#include <iostream>
#include <memory>

class Database {
public:
    virtual ~Database() = default;
    virtual void connect() const = 0;
    virtual void query() const = 0;
};

class PostgresDatabase final : public Database {
public:
    void connect() const override { std::cout << "PostgreSQL connect\n"; }
    void query() const override { std::cout << "PostgreSQL query\n"; }
};

class MySqlDatabase final : public Database {
public:
    void connect() const override { std::cout << "MySQL connect\n"; }
    void query() const override { std::cout << "MySQL query\n"; }
};

class DatabaseCreator {
public:
    virtual ~DatabaseCreator() = default;

    void useDatabase() const {
        auto database = createDatabase();
        database->connect();
        database->query();
    }

private:
    virtual std::unique_ptr<Database> createDatabase() const = 0;
};

class PostgresCreator final : public DatabaseCreator {
private:
    std::unique_ptr<Database> createDatabase() const override {
        return std::make_unique<PostgresDatabase>();
    }
};

class MySqlCreator final : public DatabaseCreator {
private:
    std::unique_ptr<Database> createDatabase() const override {
        return std::make_unique<MySqlDatabase>();
    }
};

int main() {
    PostgresCreator{}.useDatabase();
    MySqlCreator{}.useDatabase();
}
