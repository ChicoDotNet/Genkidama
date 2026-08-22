Imports System

Public Interface IFactoryMethodDatabase
    Sub Connect()
    Sub Query()
End Interface

Public NotInheritable Class FactoryMethodPostgres
    Implements IFactoryMethodDatabase

    Public Sub Connect() Implements IFactoryMethodDatabase.Connect
        Console.WriteLine("PostgreSQL connect")
    End Sub

    Public Sub Query() Implements IFactoryMethodDatabase.Query
        Console.WriteLine("PostgreSQL query")
    End Sub
End Class

Public NotInheritable Class FactoryMethodMySql
    Implements IFactoryMethodDatabase

    Public Sub Connect() Implements IFactoryMethodDatabase.Connect
        Console.WriteLine("MySQL connect")
    End Sub

    Public Sub Query() Implements IFactoryMethodDatabase.Query
        Console.WriteLine("MySQL query")
    End Sub
End Class

Public MustInherit Class FactoryMethodCreator
    Protected MustOverride Function CreateDatabase() As IFactoryMethodDatabase

    Public Sub UseDatabase()
        Dim database = CreateDatabase()
        database.Connect()
        database.Query()
    End Sub
End Class

Public NotInheritable Class FactoryMethodPostgresCreator
    Inherits FactoryMethodCreator

    Protected Overrides Function CreateDatabase() As IFactoryMethodDatabase
        Return New FactoryMethodPostgres()
    End Function
End Class

Public NotInheritable Class FactoryMethodMySqlCreator
    Inherits FactoryMethodCreator

    Protected Overrides Function CreateDatabase() As IFactoryMethodDatabase
        Return New FactoryMethodMySql()
    End Function
End Class

Module FactoryMethodExample
    Sub Main()
        Dim postgres As FactoryMethodCreator = New FactoryMethodPostgresCreator()
        Dim mysql As FactoryMethodCreator = New FactoryMethodMySqlCreator()
        postgres.UseDatabase()
        mysql.UseDatabase()
    End Sub
End Module
