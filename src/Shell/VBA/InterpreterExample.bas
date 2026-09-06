Option Explicit

Private Function InterpretExpression(ByVal expression As String) As Long
    Dim tokens() As String
    Dim index As Long
    Dim total As Long

    tokens = Split(expression, " ")
    If UBound(tokens) < 0 Or (UBound(tokens) Mod 2) <> 0 Then
        Err.Raise vbObjectError + 1, , "Invalid expression"
    End If

    total = CLng(tokens(0))
    For index = 1 To UBound(tokens) Step 2
        If tokens(index) <> "+" Then
            Err.Raise vbObjectError + 2, , "Unsupported operator"
        End If
        total = total + CLng(tokens(index + 1))
    Next index

    InterpretExpression = total
End Function

Public Sub InterpreterExample()
    Dim value As Long
    value = InterpretExpression("2 + 3 + 4")
    If value <> 9 Then
        Err.Raise vbObjectError + 3, , "Unexpected interpreter result"
    End If
    Debug.Print "interpreter=" & CStr(value)
End Sub
