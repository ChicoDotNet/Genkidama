// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Abstract Product
abstract contract Database {
    function connect() public virtual view returns (string memory);
    function query() public virtual view returns (string memory);
}

// Concrete Product
contract Postgres is Database {
    function connect() public pure override returns (string memory) {
        return "Connecting to PostgreSQL";
    }

    function query() public pure override returns (string memory) {
        return "Querying PostgreSQL";
    }
}

contract MySQL is Database {
    function connect() public pure override returns (string memory) {
        return "Connecting to MySQL";
    }

    function query() public pure override returns (string memory) {
        return "Querying MySQL";
    }
}

// Abstract Factory
abstract contract DatabaseFactory {
    function createDatabase() public virtual returns (Database);
}

// Concrete Factory
contract PostgresFactory is DatabaseFactory {
    function createDatabase() public override returns (Database) {
        return new Postgres();
    }
}

contract MySQLFactory is DatabaseFactory {
    function createDatabase() public override returns (Database) {
        return new MySQL();
    }
}

contract Example2 {
    function useDatabase(DatabaseFactory factory) public view returns (string memory, string memory) {
        Database db = factory.createDatabase();
        return (db.connect(), db.query());
    }

    function test() public view returns (string memory, string memory, string memory, string memory) {
        DatabaseFactory postgresFactory = new PostgresFactory();
        DatabaseFactory mysqlFactory = new MySQLFactory();
        (string memory postgresConnect, string memory postgresQuery) = useDatabase(postgresFactory);
        (string memory mysqlConnect, string memory mysqlQuery) = useDatabase(mysqlFactory);
        return (postgresConnect, postgresQuery, mysqlConnect, mysqlQuery);
    }
}
