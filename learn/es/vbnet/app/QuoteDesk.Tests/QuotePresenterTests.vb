Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class QuotePresenterTests
    <TestMethod>
    Public Sub AddLine_RendersValidQuote()
        Dim view = New FakeQuoteView With {
            .CustomerNameInputValue = "ACME",
            .DescriptionInputValue = "Diagnóstico",
            .QuantityInputValue = "2",
            .UnitPriceInputValue = "150"
        }
        Dim presenter = New QuotePresenter(view)

        view.RequestAddLine()

        Assert.IsNotNull(view.LastDraft)
        Assert.AreEqual(300D, view.LastDraft.Subtotal)
        Assert.AreEqual(String.Empty, view.LastError)
        GC.KeepAlive(presenter)
    End Sub

    <TestMethod>
    Public Sub AddLine_ShowsErrorForInvalidQuantity()
        Dim view = New FakeQuoteView With {
            .CustomerNameInputValue = "ACME",
            .DescriptionInputValue = "Diagnóstico",
            .QuantityInputValue = "cero",
            .UnitPriceInputValue = "150"
        }
        Dim presenter = New QuotePresenter(view)

        view.RequestAddLine()

        StringAssert.Contains(view.LastError, "cantidad")
        Assert.IsNull(view.LastDraft)
        GC.KeepAlive(presenter)
    End Sub

    Private NotInheritable Class FakeQuoteView
        Implements IQuoteView

        Public Event AddLineRequested As EventHandler Implements IQuoteView.AddLineRequested
        Public Property CustomerNameInputValue As String = String.Empty
        Public Property DescriptionInputValue As String = String.Empty
        Public Property QuantityInputValue As String = String.Empty
        Public Property UnitPriceInputValue As String = String.Empty
        Public Property LastDraft As QuoteDraft
        Public Property LastError As String = String.Empty

        Public ReadOnly Property CustomerNameInput As String Implements IQuoteView.CustomerNameInput
            Get
                Return CustomerNameInputValue
            End Get
        End Property
        Public ReadOnly Property DescriptionInput As String Implements IQuoteView.DescriptionInput
            Get
                Return DescriptionInputValue
            End Get
        End Property
        Public ReadOnly Property QuantityInput As String Implements IQuoteView.QuantityInput
            Get
                Return QuantityInputValue
            End Get
        End Property
        Public ReadOnly Property UnitPriceInput As String Implements IQuoteView.UnitPriceInput
            Get
                Return UnitPriceInputValue
            End Get
        End Property

        Public Sub Render(draft As QuoteDraft) Implements IQuoteView.Render
            LastDraft = draft
            LastError = String.Empty
        End Sub

        Public Sub ShowError(message As String) Implements IQuoteView.ShowError
            LastError = message
        End Sub

        Public Sub RequestAddLine()
            RaiseEvent AddLineRequested(Me, EventArgs.Empty)
        End Sub
    End Class
End Class
