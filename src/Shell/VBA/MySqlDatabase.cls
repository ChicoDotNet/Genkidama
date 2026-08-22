VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "MySqlDatabase"
Option Explicit
Implements IDatabase

Private Sub IDatabase_Connect()
    Debug.Print "MySQL connect"
End Sub

Private Sub IDatabase_Query()
    Debug.Print "MySQL query"
End Sub
