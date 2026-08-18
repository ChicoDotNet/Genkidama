VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "PostgresCreator"
Option Explicit
Implements IDatabaseCreator

Private Function IDatabaseCreator_CreateDatabase() As IDatabase
    Dim database As New PostgresDatabase
    Set IDatabaseCreator_CreateDatabase = database
End Function
