Option Explicit

Private mBalance As Long

Private Sub Deposit(ByVal amount As Long)
    mBalance = mBalance + amount
End Sub

Private Sub Withdraw(ByVal amount As Long)
    mBalance = mBalance - amount
End Sub

Private Sub ExecuteCommand(ByVal operation As String, ByVal amount As Long)
    Select Case operation
        Case "deposit"
            Deposit amount
        Case "withdraw"
            Withdraw amount
        Case Else
            Err.Raise vbObjectError + 513, "CommandExample", "Unknown command"
    End Select
End Sub

Public Sub DemoCommand()
    mBalance = 100
    ExecuteCommand "deposit", 50
    ExecuteCommand "withdraw", 20

    Debug.Assert mBalance = 130
    Debug.Print "balance=" & CStr(mBalance) & ";commands=2"
End Sub
