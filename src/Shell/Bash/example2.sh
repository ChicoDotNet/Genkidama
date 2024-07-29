#!/bin/bash

# Abstract Factory
create_database() {
    local db_type=$1
    if [ "$db_type" == "postgresql" ]; then
        connect_postgresql
        query_postgresql
    elif [ "$db_type" == "mysql" ]; then
        connect_mysql
        query_mysql
    fi
}

# Concrete Implementations
connect_postgresql() {
    echo "Connecting to PostgreSQL"
}

query_postgresql() {
    echo "Querying PostgreSQL"
}

connect_mysql() {
    echo "Connecting to MySQL"
}

query_mysql() {
    echo "Querying MySQL"
}

# Usage
create_database "postgresql"
create_database "mysql"
