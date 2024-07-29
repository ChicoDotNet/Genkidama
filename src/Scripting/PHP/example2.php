<?php

interface Database {
    public function connect();
    public function query();
}

class Postgres implements Database {
    public function connect() {
        echo "Connecting to PostgreSQL\n";
    }

    public function query() {
        echo "Querying PostgreSQL\n";
    }
}

class MySQL implements Database {
    public function connect() {
        echo "Connecting to MySQL\n";
    }

    public function query() {
        echo "Querying MySQL\n";
    }
}

interface DatabaseFactory {
    public function createDatabase(): Database;
}

class PostgresFactory implements DatabaseFactory {
    public function createDatabase(): Database {
        return new Postgres();
    }
}

class MySQLFactory implements DatabaseFactory {
    public function createDatabase(): Database {
        return new MySQL();
    }
}

function useDatabase(DatabaseFactory $factory) {
    $db = $factory->createDatabase();
    $db->connect();
    $db->query();
}

// Usage
useDatabase(new PostgresFactory());
useDatabase(new MySQLFactory());
