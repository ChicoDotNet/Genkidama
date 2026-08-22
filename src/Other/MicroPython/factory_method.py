def create_postgres():
    return {
        "connect": lambda: print("PostgreSQL connect"),
        "query": lambda: print("PostgreSQL query"),
    }


def create_mysql():
    return {
        "connect": lambda: print("MySQL connect"),
        "query": lambda: print("MySQL query"),
    }


def use_database(create_database):
    database = create_database()
    database["connect"]()
    database["query"]()


use_database(create_postgres)
use_database(create_mysql)
