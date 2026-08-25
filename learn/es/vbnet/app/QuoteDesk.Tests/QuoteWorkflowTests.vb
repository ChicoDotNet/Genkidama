Imports System.IO
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class QuoteWorkflowTests
    <TestMethod>
    Public Sub Draft_CanReplaceAndRemoveLines()
        Dim draft = New QuoteDraft()
        draft.AddLine(New QuoteLine("Análisis", 1, 100D))
        draft.AddLine(New QuoteLine("Implementación", 1, 300D))

        draft.ReplaceLine(0, New QuoteLine("Descubrimiento", 2, 75D))
        draft.RemoveLine(1)

        Assert.AreEqual(1, draft.Lines.Count)
        Assert.AreEqual("Descubrimiento", draft.Lines(0).Description)
        Assert.AreEqual(150D, draft.Subtotal)
    End Sub

    <TestMethod>
    Public Sub Totals_IncludeConfiguredTaxWithCurrencyRounding()
        Dim draft = New QuoteDraft With {.TaxRate = 0.16D}
        draft.AddLine(New QuoteLine("Servicio", 1, 100.03D))

        Assert.AreEqual(100.03D, draft.Subtotal)
        Assert.AreEqual(16D, draft.TaxAmount)
        Assert.AreEqual(116.03D, draft.Total)
    End Sub

    <TestMethod>
    Public Sub Approve_RequiresCustomerAndAtLeastOneLine()
        Dim draft = New QuoteDraft()
        draft.AddLine(New QuoteLine("Servicio", 1, 100D))

        Assert.ThrowsExactly(Of InvalidOperationException)(
            Sub()
                draft.Approve()
            End Sub)
    End Sub

    <TestMethod>
    Public Sub ApprovedQuote_RejectsFurtherEdits()
        Dim draft = CreateApprovedQuote()

        Assert.ThrowsExactly(Of InvalidOperationException)(
            Sub()
                draft.AddLine(New QuoteLine("Extra", 1, 10D))
            End Sub)
    End Sub

    <TestMethod>
    Public Sub Invoice_CanOnlyBeCreatedFromApprovedQuote()
        Dim draft = New QuoteDraft With {.CustomerName = "Cliente"}
        draft.AddLine(New QuoteLine("Servicio", 1, 100D))

        Assert.ThrowsExactly(Of InvalidOperationException)(
            Sub()
                Dim unused = QuoteInvoiceService.CreateInvoice(draft, "F-001")
            End Sub)

        draft.Approve()
        Dim invoice = QuoteInvoiceService.CreateInvoice(draft, "F-001")
        Assert.AreEqual("F-001", invoice.InvoiceNumber)
        Assert.AreEqual(116D, invoice.Total)
    End Sub

    <TestMethod>
    Public Sub FileStore_RoundTripsApprovedQuote()
        Dim filePath = Path.Combine(Path.GetTempPath(), $"quotedesk-{Guid.NewGuid():N}.json")
        Dim store = New QuoteFileStore()

        Try
            Dim original = CreateApprovedQuote()
            store.Save(filePath, original)
            Dim restored = store.Load(filePath)

            Assert.AreEqual(QuoteStatus.Approved, restored.Status)
            Assert.AreEqual(original.CustomerName, restored.CustomerName)
            Assert.AreEqual(original.Total, restored.Total)
            Assert.AreEqual(original.Lines.Count, restored.Lines.Count)
        Finally
            If File.Exists(filePath) Then File.Delete(filePath)
        End Try
    End Sub

    <TestMethod>
    Public Sub FileStore_RejectsUnsupportedSchema()
        Dim filePath = Path.Combine(Path.GetTempPath(), $"quotedesk-{Guid.NewGuid():N}.json")
        File.WriteAllText(filePath, "{""SchemaVersion"":99,""CustomerName"":""X"",""TaxRate"":0.16,""Status"":0,""Lines"":[]}")

        Try
            Dim store = New QuoteFileStore()
            Assert.ThrowsExactly(Of InvalidDataException)(
                Sub()
                    Dim unused = store.Load(filePath)
                End Sub)
        Finally
            If File.Exists(filePath) Then File.Delete(filePath)
        End Try
    End Sub

    Private Shared Function CreateApprovedQuote() As QuoteDraft
        Dim draft = New QuoteDraft With {
            .CustomerName = "Cliente",
            .TaxRate = 0.16D
        }
        draft.AddLine(New QuoteLine("Servicio", 1, 100D))
        draft.Approve()
        Return draft
    End Function
End Class
