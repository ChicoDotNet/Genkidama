Imports System.Globalization

Namespace Core
    ''' <summary>Coordinates quote-entry UI without depending on Windows Forms controls.</summary>
    Public NotInheritable Class QuotePresenter
        Private ReadOnly _view As IQuoteView
        Private ReadOnly _draft As New QuoteDraft()

        Public Sub New(view As IQuoteView)
            If view Is Nothing Then
                Throw New ArgumentNullException(NameOf(view))
            End If

            _view = view
            AddHandler _view.AddLineRequested, AddressOf AddLine
        End Sub

        Private Sub AddLine(sender As Object, args As EventArgs)
            Dim customer = _view.CustomerNameInput.Trim()
            If customer.Length = 0 Then
                _view.ShowError("Escribe el nombre del cliente.")
                Return
            End If

            Dim quantity As Integer
            If Not Integer.TryParse(_view.QuantityInput, NumberStyles.Integer, CultureInfo.CurrentCulture, quantity) OrElse quantity <= 0 Then
                _view.ShowError("La cantidad debe ser un entero mayor que cero.")
                Return
            End If

            Dim unitPrice As Decimal
            If Not Decimal.TryParse(_view.UnitPriceInput, NumberStyles.Number, CultureInfo.CurrentCulture, unitPrice) OrElse unitPrice < 0D Then
                _view.ShowError("El precio debe ser un número no negativo.")
                Return
            End If

            Try
                _draft.CustomerName = customer
                _draft.AddLine(New QuoteLine(_view.DescriptionInput, quantity, unitPrice))
                _view.Render(_draft)
            Catch ex As ArgumentException
                _view.ShowError(ex.Message)
            End Try
        End Sub
    End Class
End Namespace
