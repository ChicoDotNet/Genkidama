Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class QuoteTests
    <TestMethod>
    Public Sub LineTotal_MultipliesQuantityByUnitPrice()
        Dim line = New QuoteLine("Consultoría", 3, 125.5D)
        Assert.AreEqual(376.5D, line.LineTotal)
    End Sub

    <TestMethod>
    Public Sub QuoteSubtotal_SumsAllLines()
        Dim draft = New QuoteDraft()
        draft.AddLine(New QuoteLine("Análisis", 2, 100D))
        draft.AddLine(New QuoteLine("Implementación", 1, 350D))
        Assert.AreEqual(550D, draft.Subtotal)
    End Sub

    <TestMethod>
    Public Sub QuoteLine_RejectsInvalidQuantity()
        Assert.ThrowsExactly(Of ArgumentOutOfRangeException)(
            Sub()
                Dim unused = New QuoteLine("Soporte", 0, 10D)
            End Sub)
    End Sub
End Class
