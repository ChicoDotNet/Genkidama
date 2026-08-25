Namespace Core
    ''' <summary>Defines the passive view surface required by the quote presenter.</summary>
    Public Interface IQuoteView
        Event AddLineRequested As EventHandler

        ReadOnly Property CustomerNameInput As String
        ReadOnly Property DescriptionInput As String
        ReadOnly Property QuantityInput As String
        ReadOnly Property UnitPriceInput As String

        Sub Render(draft As QuoteDraft)
        Sub ShowError(message As String)
    End Interface
End Namespace
