from abc import ABC, abstractmethod


class Database(ABC):
    @abstractmethod
    def connect(self) -> None:
        raise NotImplementedError

    @abstractmethod
    def query(self) -> None:
        raise NotImplementedError


class PostgresDatabase(Database):
    def connect(self) -> None:
        print("PostgreSQL connect")

    def query(self) -> None:
        print("PostgreSQL query")


class MySqlDatabase(Database):
    def connect(self) -> None:
        print("MySQL connect")

    def query(self) -> None:
        print("MySQL query")


class DatabaseCreator(ABC):
    @abstractmethod
    def create_database(self) -> Database:
        raise NotImplementedError

    def use_database(self) -> None:
        database = self.create_database()
        database.connect()
        database.query()


class PostgresCreator(DatabaseCreator):
    def create_database(self) -> Database:
        return PostgresDatabase()


class MySqlCreator(DatabaseCreator):
    def create_database(self) -> Database:
        return MySqlDatabase()


if __name__ == "__main__":
    PostgresCreator().use_database()
    MySqlCreator().use_database()
