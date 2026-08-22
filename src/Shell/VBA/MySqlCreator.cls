VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "MySqlCreator"
Option Explicit
Implements IDatabaseCreator

Private Function IDatabaseCreator_CreateDatabase() As IDatabase
    Dim database As New MySqlDatabase
    Set IDatabaseCreator_CreateDatabase = database
End Function
