class Database:
    def connect(self):
        pass

    def query(self):
        pass

class Postgres(Database):
    def connect(self):
        print("Connecting to PostgreSQL")

    def query(self):
        print("Querying PostgreSQL")

class MySQL(Database):
    def connect(self):
        print("Connecting to MySQL")

    def query(self):
        print("Querying MySQL")

class DatabaseFactory:
    def create_database(self):
        pass

class PostgresFactory(DatabaseFactory):
    def create_database(self):
        return Postgres()

class MySQLFactory(DatabaseFactory):
    def create_database(self):
        return MySQL()

def use_database(factory):
    db = factory.create_database()
    db.connect()
    db.query()

# Usage
use_database(PostgresFactory())
use_database(MySQLFactory())
