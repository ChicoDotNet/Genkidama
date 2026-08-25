Imports System.IO
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class QuoteCatalogTests
    <TestMethod>
    Public Sub Search_FiltersByCustomerAndReportsCorruptFiles()
        Dim directoryPath = Path.Combine(Path.GetTempPath(), $"quotedesk-{Guid.NewGuid():N}")
        Directory.CreateDirectory(directoryPath)
        Try
            Dim store = New QuoteFileStore()
            Dim quote = New QuoteDraft With {.CustomerName = "Acme", .TaxRate = 0.16D}
            quote.AddLine(New QuoteLine("Servicio", 1, 100D))
            store.Save(Path.Combine(directoryPath, "acme.quote.json"), quote)
            File.WriteAllText(Path.Combine(directoryPath, "broken.quote.json"), "{not-json")

            Dim result = New QuoteCatalog(store).Search(directoryPath, "acm")

            Assert.AreEqual(1, result.Entries.Count)
            Assert.AreEqual("Acme", result.Entries(0).CustomerName)
            Assert.AreEqual(1, result.Issues.Count)
            StringAssert.Contains(result.Issues(0).FilePath, "broken.quote.json")
        Finally
            Directory.Delete(directoryPath, True)
        End Try
    End Sub

    <TestMethod>
    Public Sub Search_RejectsMissingDirectoryExplicitly()
        Dim missingPath = Path.Combine(Path.GetTempPath(), $"missing-{Guid.NewGuid():N}")
        Assert.ThrowsExactly(Of DirectoryNotFoundException)(
            Sub()
                Dim unused = New QuoteCatalog().Search(missingPath)
            End Sub)
    End Sub
End Class
