Imports System

Friend NotInheritable Class AuthService
    Public Function Authenticate(user As String) As String
        Return $"auth({user})"
    End Function
End Class

Friend NotInheritable Class InventoryService
    Public Function Reserve(sku As String) As String
        Return $"reserve({sku})"
    End Function
End Class

Friend NotInheritable Class BillingService
    Public Function Charge(cents As Integer) As String
        Return $"charge({cents})"
    End Function
End Class

Friend NotInheritable Class CheckoutFacade
    Private ReadOnly auth As AuthService
    Private ReadOnly inventory As InventoryService
    Private ReadOnly billing As BillingService

    Public Sub New(auth As AuthService, inventory As InventoryService, billing As BillingService)
        Me.auth = auth
        Me.inventory = inventory
        Me.billing = billing
    End Sub

    Public Function Checkout(user As String, sku As String, cents As Integer) As String
        Return $"checkout={auth.Authenticate(user)}>{inventory.Reserve(sku)}>{billing.Charge(cents)}"
    End Function
End Class

Module FacadeExample
    Sub Main()
        Dim facade = New CheckoutFacade(New AuthService(), New InventoryService(), New BillingService())
        Console.WriteLine(facade.Checkout("alice", "SKU-42", 499))
    End Sub
End Module
