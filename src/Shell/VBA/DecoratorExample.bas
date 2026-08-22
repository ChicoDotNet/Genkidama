Option Explicit

Private Function PlainMessage() As String
    PlainMessage = "alert"
End Function

Private Function AuditDecorator(ByVal inner As String) As String
    AuditDecorator = "audit(" & inner & ")"
End Function

Private Function EncryptDecorator(ByVal inner As String) As String
    EncryptDecorator = "enc(" & inner & ")"
End Function

Public Sub RunDecoratorExample()
    Dim base As String
    base = PlainMessage()

    Debug.Print "base=" & base
    Debug.Print "audit=" & AuditDecorator(base)
    Debug.Print "encrypted=" & EncryptDecorator(base)
    Debug.Print "stacked=" & AuditDecorator(EncryptDecorator(base))
End Sub
