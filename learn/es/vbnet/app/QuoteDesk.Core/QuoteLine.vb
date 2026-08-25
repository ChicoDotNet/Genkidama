Namespace Core
    ''' <summary>Represents one deterministic line in a quote.</summary>
    Public NotInheritable Class QuoteLine
        Public Sub New(description As String, quantity As Integer, unitPrice As Decimal)
            If String.IsNullOrWhiteSpace(description) Then Throw New ArgumentException("La descripción es obligatoria.", NameOf(description))
            If quantity <= 0 Then Throw New ArgumentOutOfRangeException(NameOf(quantity), "La cantidad debe ser mayor que cero.")
            If unitPrice < 0D Then Throw New ArgumentOutOfRangeException(NameOf(unitPrice), "El precio no puede ser negativo.")

            Me.Description = description.Trim()
            Me.Quantity = quantity
            Me.UnitPrice = unitPrice
        End Sub

        Public ReadOnly Property Description As String
        Public ReadOnly Property Quantity As Integer
        Public ReadOnly Property UnitPrice As Decimal
        Public ReadOnly Property LineTotal As Decimal
            Get
                Return Quantity * UnitPrice
            End Get
        End Property
    End Class
End Namespace
