package main

import "fmt"

// Abstract Product
type Database interface {
    Connect()
    Query()
}

// Concrete Product
type Postgres struct{}

func (p *Postgres) Connect() {
    fmt.Println("Connecting to PostgreSQL")
}

func (p *Postgres) Query() {
    fmt.Println("Querying PostgreSQL")
}

type MySQL struct{}

func (m *MySQL) Connect() {
    fmt.Println("Connecting to MySQL")
}

func (m *MySQL) Query() {
    fmt.Println("Querying MySQL")
}

// Abstract Factory
type DatabaseFactory interface {
    CreateDatabase() Database
}

// Concrete Factory
type PostgresFactory struct{}

func (pf *PostgresFactory) CreateDatabase() Database {
    return &Postgres{}
}

type MySQLFactory struct{}

func (mf *MySQLFactory) CreateDatabase() Database {
    return &MySQL{}
}

func useDatabase(factory DatabaseFactory) {
    db := factory.CreateDatabase()
    db.Connect()
    db.Query()
}

func main() {
    postgresFactory := &PostgresFactory{}
    mysqlFactory := &MySQLFactory{}
    useDatabase(postgresFactory)
    useDatabase(mysqlFactory)
}
