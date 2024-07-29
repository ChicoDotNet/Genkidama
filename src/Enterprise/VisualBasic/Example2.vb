Imports System

' Abstract Product
Public Interface Database
    Sub Connect()
    Sub Query()
End Interface

' Concrete Product
Public Class Postgres
    Implements Database
    Public Sub Connect() Implements Database.Connect
        Console.WriteLine("Connecting to PostgreSQL")
    End Sub
    Public Sub Query() Implements Database.Query
        Console.WriteLine("Querying PostgreSQL")
    End Sub
End Class

Public Class MySQL
    Implements Database
    Public Sub Connect() Implements Database.Connect
        Console.WriteLine("Connecting to MySQL")
    End Sub
    Public Sub Query() Implements Database.Query
        Console.WriteLine("Querying MySQL")
    End Sub
End Class

' Abstract Factory
Public Interface DatabaseFactory
    Function CreateDatabase() As Database
End Interface

' Concrete Factory
Public Class PostgresFactory
    Implements DatabaseFactory
    Public Function CreateDatabase() As Database Implements DatabaseFactory.CreateDatabase
        Return New Postgres()
    End Function
End Class

Public Class MySQLFactory
    Implements DatabaseFactory
    Public Function CreateDatabase() As Database Implements DatabaseFactory.CreateDatabase
        Return New MySQL()
    End Function
End Class

Module Example2
    Sub UseDatabase(factory As DatabaseFactory)
        Dim db As Database = factory.CreateDatabase()
        db.Connect()
        db.Query()
    End Sub

    Sub Main()
        UseDatabase(New PostgresFactory())
        UseDatabase(New MySQLFactory())
    End Sub
End Module
