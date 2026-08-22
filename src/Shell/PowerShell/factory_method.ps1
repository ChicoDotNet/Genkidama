Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-PostgresDatabase {
    [pscustomobject]@{
        Connect = { Write-Output 'PostgreSQL connect' }.GetNewClosure()
        Query   = { Write-Output 'PostgreSQL query' }.GetNewClosure()
    }
}

function New-MySqlDatabase {
    [pscustomobject]@{
        Connect = { Write-Output 'MySQL connect' }.GetNewClosure()
        Query   = { Write-Output 'MySQL query' }.GetNewClosure()
    }
}

function Use-Database {
    param(
        [Parameter(Mandatory)]
        [scriptblock]$CreateDatabase
    )

    $database = & $CreateDatabase
    & $database.Connect
    & $database.Query
}

Use-Database { New-PostgresDatabase }
Use-Database { New-MySqlDatabase }
