# Abstract Factory
function Create-Database {
    param (
        [string]$dbType
    )

    if ($dbType -eq "postgresql") {
        Connect-Postgresql
        Query-Postgresql
    } elseif ($dbType -eq "mysql") {
        Connect-MySQL
        Query-MySQL
    }
}

# Concrete Implementations
function Connect-Postgresql {
    Write-Output "Connecting to PostgreSQL"
}

function Query-Postgresql {
    Write-Output "Querying PostgreSQL"
}

function Connect-MySQL {
    Write-Output "Connecting to MySQL"
}

function Query-MySQL {
    Write-Output "Querying MySQL"
}

# Usage
Create-Database -dbType "postgresql"
Create-Database -dbType "mysql"
