Option Explicit

Private Function RouteMessage(ByVal senderName As String, ByVal targetName As String, ByVal messageText As String) As String
    Select Case targetName
        Case "inventory"
            RouteMessage = "inventory<-" & senderName & ":" & messageText
        Case "payment"
            RouteMessage = "payment<-" & senderName & ":" & messageText
        Case Else
            Err.Raise vbObjectError + 2048, "Mediator", "UnknownColleague:" & targetName
    End Select
End Function

Public Function PaymentSend(ByVal messageText As String) As String
    PaymentSend = RouteMessage("payment", "inventory", messageText)
End Function

Public Function InventorySend(ByVal messageText As String) As String
    InventorySend = RouteMessage("inventory", "payment", messageText)
End Function

Public Sub VerifyMediator()
    Debug.Assert PaymentSend("reserve") = "inventory<-payment:reserve"
    Debug.Assert InventorySend("reserved") = "payment<-inventory:reserved"

    On Error GoTo ExpectedFailure
    Dim ignored As String
    ignored = RouteMessage("payment", "shipping", "probe")
    Err.Raise vbObjectError + 2049, "Mediator", "Expected UnknownColleague failure"

ExpectedFailure:
    Debug.Assert Err.Description = "UnknownColleague:shipping"
    Err.Clear
    Debug.Print "VBA Mediator: passed"
End Sub
