VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "PostgresDatabase"
Option Explicit
Implements IDatabase

Private Sub IDatabase_Connect()
    Debug.Print "PostgreSQL connect"
End Sub

Private Sub IDatabase_Query()
    Debug.Print "PostgreSQL query"
End Sub
