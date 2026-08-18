package main

import "fmt"

type database interface {
	connect()
	query()
}

type postgresDatabase struct{}
func (postgresDatabase) connect() { fmt.Println("PostgreSQL connect") }
func (postgresDatabase) query() { fmt.Println("PostgreSQL query") }

type mySqlDatabase struct{}
func (mySqlDatabase) connect() { fmt.Println("MySQL connect") }
func (mySqlDatabase) query() { fmt.Println("MySQL query") }

type factoryMethod func() database

func useDatabase(create factoryMethod) {
	database := create()
	database.connect()
	database.query()
}

func main() {
	useDatabase(func() database { return postgresDatabase{} })
	useDatabase(func() database { return mySqlDatabase{} })
}
