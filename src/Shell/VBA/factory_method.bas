Attribute VB_Name = "FactoryMethodExample"
Option Explicit

Private Sub UseDatabase(ByVal creator As IDatabaseCreator)
    Dim database As IDatabase
    Set database = creator.CreateDatabase()
    database.Connect
    database.Query
End Sub

Public Sub Usage()
    Dim postgres As IDatabaseCreator
    Dim mysql As IDatabaseCreator

    Set postgres = New PostgresCreator
    Set mysql = New MySqlCreator

    UseDatabase postgres
    UseDatabase mysql
End Sub
