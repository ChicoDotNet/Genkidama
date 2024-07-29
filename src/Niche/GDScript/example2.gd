extends Node

# Abstract Factory
func create_database(db_type: String) -> Object:
    if db_type == "postgresql":
        return Postgres.new()
    elif db_type == "mysql":
        return MySQL.new()

# Concrete Products
class Postgres:
    func connect():
        print("Connecting to PostgreSQL")
    
    func query():
        print("Querying PostgreSQL")

class MySQL:
    func connect():
        print("Connecting to MySQL")
    
    func query():
        print("Querying MySQL")

# Usage
func _ready():
    var db1 = create_database("postgresql")
    db1.connect()
    db1.query()
    
    var db2 = create_database("mysql")
    db2.connect()
    db2.query()
