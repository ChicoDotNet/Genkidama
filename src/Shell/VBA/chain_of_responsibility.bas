' Chain of Responsibility — procedural VBA representation
Option Explicit

Private Function HandleEscalation(ByVal amount As Long, ByRef visited As String) As String
    visited = visited & ">escalation"
    HandleEscalation = "handled=escalation;result=refund(" & CStr(amount) & ")"
End Function

Private Function HandleBilling(ByVal amount As Long, ByRef visited As String) As String
    visited = visited & ">billing"
    If amount <= 500 Then
        HandleBilling = "handled=billing;result=refund(" & CStr(amount) & ")"
    Else
        HandleBilling = HandleEscalation(amount, visited)
    End If
End Function

Private Function HandleFaq(ByVal amount As Long, ByRef visited As String) As String
    visited = "faq"
    If amount <= 50 Then
        HandleFaq = "handled=faq;result=refund(" & CStr(amount) & ")"
    Else
        HandleFaq = HandleBilling(amount, visited)
    End If
End Function

Public Sub Usage()
    Dim visited As String
    Dim result As String
    result = HandleFaq(250, visited)
    Debug.Print "visited=" & visited & ";" & result
End Sub
