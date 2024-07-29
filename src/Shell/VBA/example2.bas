' Abstract Factory
Function CreateDatabase(dbType As String) As Object
    If dbType = "postgresql" Then
        Set CreateDatabase = New Postgres
    ElseIf dbType = "mysql" Then
        Set CreateDatabase = New MySQL
    End If
End Function

' Concrete Implementations
Class Postgres
    Sub Connect()
        MsgBox "Connecting to PostgreSQL"
    End Sub
    Sub Query()
        MsgBox "Querying PostgreSQL"
    End Sub
End Class

Class MySQL
    Sub Connect()
        MsgBox "Connecting to MySQL"
    End Sub
    Sub Query()
        MsgBox "Querying MySQL"
    End Sub
End Class

Sub Usage()
    Dim db As Object
    Set db = CreateDatabase("postgresql")
    db.Connect
    db.Query

    Set db = CreateDatabase("mysql")
    db.Connect
    db.Query
End Sub
