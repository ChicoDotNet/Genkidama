Imports System.IO
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class InvoiceFileStoreTests
    <TestMethod>
    Public Sub InvoiceStore_RoundTripsImmutableSnapshot()
        Dim quote = New QuoteDraft With {.CustomerName = "Cliente", .TaxRate = 0.16D}
        quote.AddLine(New QuoteLine("Trabajo", 2, 50D))
        quote.Approve()
        Dim invoice = QuoteInvoiceService.CreateInvoice(quote, "F-001")
        Dim filePath = Path.Combine(Path.GetTempPath(), $"invoice-{Guid.NewGuid():N}.json")
        Try
            Dim store = New InvoiceFileStore()
            store.Save(filePath, invoice)
            Dim loaded = store.Load(filePath)
            Assert.AreEqual("F-001", loaded.InvoiceNumber)
            Assert.AreEqual(116D, loaded.Total)
            Assert.AreEqual(1, loaded.Lines.Count)
        Finally
            If File.Exists(filePath) Then File.Delete(filePath)
        End Try
    End Sub

    <TestMethod>
    Public Sub InvoiceStore_RejectsMalformedExternalJson()
        Dim filePath = Path.Combine(Path.GetTempPath(), $"invoice-{Guid.NewGuid():N}.json")
        Try
            File.WriteAllText(filePath, "{broken")
            Assert.ThrowsExactly(Of InvalidDataException)(Function() New InvoiceFileStore().Load(filePath))
        Finally
            If File.Exists(filePath) Then File.Delete(filePath)
        End Try
    End Sub

    <TestMethod>
    Public Sub InvoiceStore_RejectsTamperedTotals()
        Dim quote = New QuoteDraft With {.CustomerName = "Cliente", .TaxRate = 0.16D}
        quote.AddLine(New QuoteLine("Trabajo", 2, 50D))
        quote.Approve()
        Dim invoice = QuoteInvoiceService.CreateInvoice(quote, "F-002")
        Dim filePath = Path.Combine(Path.GetTempPath(), $"invoice-{Guid.NewGuid():N}.json")
        Try
            Dim store = New InvoiceFileStore()
            store.Save(filePath, invoice)
            Dim json = File.ReadAllText(filePath)
            Dim tampered = json.Replace("""Total"": 116", """Total"": 999", StringComparison.Ordinal)
            Assert.AreNotEqual(json, tampered)
            File.WriteAllText(filePath, tampered)
            Assert.ThrowsExactly(Of InvalidDataException)(Function() store.Load(filePath))
        Finally
            If File.Exists(filePath) Then File.Delete(filePath)
        End Try
    End Sub

    <TestMethod>
    Public Sub InvoiceDocument_RejectsCallerProvidedInconsistentTotals()
        Dim lines As QuoteLine() = {New QuoteLine("Trabajo", 1, 100D)}
        Assert.ThrowsExactly(Of ArgumentException)(
            Function()
                Return New InvoiceDocument("F-003", "Cliente", lines, 100D, 16D, 999D)
            End Function)
    End Sub
End Class
