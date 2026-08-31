Imports System
Imports System.Collections.Generic

Friend Module MediatorExample
    Private NotInheritable Class CheckoutMediator
        Private ReadOnly colleagues As New Dictionary(Of String, Action(Of String, String))(StringComparer.Ordinal)

        Public Sub Register(name As String, receiver As Action(Of String, String))
            colleagues(name) = receiver
        End Sub

        Public Function Send(sender As String, recipient As String, message As String) As Boolean
            Dim receiver As Action(Of String, String) = Nothing
            If Not colleagues.TryGetValue(recipient, receiver) Then Return False
            receiver(sender, message)
            Return True
        End Function
    End Class

    Friend Function Run() As Boolean
        Dim deliveries As New List(Of String)
        Dim mediator As New CheckoutMediator

        mediator.Register("payment", Sub(sender, message) deliveries.Add($"payment<-{sender}:{message}"))
        mediator.Register("inventory", Sub(sender, message) deliveries.Add($"inventory<-{sender}:{message}"))

        Dim reserveDelivered = mediator.Send("payment", "inventory", "reserve")
        Dim chargedDelivered = mediator.Send("inventory", "payment", "reserved")
        Dim unknownRejected = Not mediator.Send("payment", "shipping", "dispatch")

        Return reserveDelivered AndAlso
            chargedDelivered AndAlso
            unknownRejected AndAlso
            String.Join(">", deliveries) = "inventory<-payment:reserve>payment<-inventory:reserved"
    End Function
End Module
