Option Explicit

Public Function Authenticate(ByVal user As String) As String
    Authenticate = "auth(" & user & ")"
End Function

Public Function ReserveInventory(ByVal sku As String) As String
    ReserveInventory = "reserve(" & sku & ")"
End Function

Public Function Charge(ByVal cents As Long) As String
    Charge = "charge(" & CStr(cents) & ")"
End Function

Public Function CheckoutFacade(ByVal user As String, ByVal sku As String, ByVal cents As Long) As String
    CheckoutFacade = Authenticate(user) & ">" & ReserveInventory(sku) & ">" & Charge(cents)
End Function

Public Sub DemoFacade()
    Debug.Print "checkout=" & CheckoutFacade("alice", "SKU-42", 499)
End Sub
