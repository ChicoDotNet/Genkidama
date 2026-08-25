Imports System.IO
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports QuoteDesk.Core

<TestClass>
Public Class QuoteOperationsTests
    <TestMethod>
    Public Sub Diagnostics_AggregatesCountsWithoutExposingCustomerNames()
        Dim directoryPath = Path.Combine(Path.GetTempPath(), $"quotedesk-diagnostics-{Guid.NewGuid():N}")
        Directory.CreateDirectory(directoryPath)
        Try
            Dim store = New QuoteFileStore()
            Dim draft = New QuoteDraft With {.CustomerName = "Cliente Secreto", .TaxRate = 0.16D}
            draft.AddLine(New QuoteLine("Servicio confidencial", 1, 100D))
            store.Save(Path.Combine(directoryPath, "draft.quote.json"), draft)

            Dim approved = New QuoteDraft With {.CustomerName = "Otra Persona", .TaxRate = 0D}
            approved.AddLine(New QuoteLine("Trabajo", 1, 50D))
            approved.Approve()
            store.Save(Path.Combine(directoryPath, "approved.quote.json"), approved)
            File.WriteAllText(Path.Combine(directoryPath, "broken.quote.json"), "{broken")

            Dim snapshot = New QuoteDiagnostics().Inspect(directoryPath)
            Dim text = QuoteDiagnosticsFormatter.Format(snapshot)

            Assert.AreEqual(2, snapshot.ValidDocuments)
            Assert.AreEqual(1, snapshot.InvalidDocuments)
            Assert.AreEqual(1, snapshot.DraftDocuments)
            Assert.AreEqual(1, snapshot.ApprovedDocuments)
            Assert.AreEqual(166D, snapshot.AggregateTotal)
            StringAssert.Contains(text, "valid=2")
            StringAssert.Contains(text, "invalid=1")
            Assert.IsFalse(text.Contains("Cliente Secreto", StringComparison.Ordinal))
            Assert.IsFalse(text.Contains("Servicio confidencial", StringComparison.Ordinal))
        Finally
            Directory.Delete(directoryPath, True)
        End Try
    End Sub

    <TestMethod>
    Public Sub Backup_CopiesValidAndCorruptEvidenceWithoutChangingSource()
        Dim sourcePath = Path.Combine(Path.GetTempPath(), $"quotedesk-source-{Guid.NewGuid():N}")
        Dim backupPath = Path.Combine(Path.GetTempPath(), $"quotedesk-backup-{Guid.NewGuid():N}")
        Directory.CreateDirectory(sourcePath)
        Try
            Dim originalA = "{broken"
            Dim originalB = "{also-broken"
            File.WriteAllText(Path.Combine(sourcePath, "a.quote.json"), originalA)
            File.WriteAllText(Path.Combine(sourcePath, "b.quote.json"), originalB)

            Dim copied = QuoteBackupService.CreateBackup(sourcePath, backupPath)

            Assert.AreEqual(2, copied)
            Assert.AreEqual(originalA, File.ReadAllText(Path.Combine(sourcePath, "a.quote.json")))
            Assert.AreEqual(originalB, File.ReadAllText(Path.Combine(sourcePath, "b.quote.json")))
            Assert.AreEqual(originalA, File.ReadAllText(Path.Combine(backupPath, "a.quote.json")))
            Assert.AreEqual(originalB, File.ReadAllText(Path.Combine(backupPath, "b.quote.json")))
        Finally
            If Directory.Exists(sourcePath) Then Directory.Delete(sourcePath, True)
            If Directory.Exists(backupPath) Then Directory.Delete(backupPath, True)
        End Try
    End Sub

    <TestMethod>
    Public Sub Backup_RejectsSourceAsDestination()
        Dim directoryPath = Path.Combine(Path.GetTempPath(), $"quotedesk-same-{Guid.NewGuid():N}")
        Directory.CreateDirectory(directoryPath)
        Try
            Assert.ThrowsExactly(Of ArgumentException)(
                Function()
                    Return QuoteBackupService.CreateBackup(directoryPath, directoryPath)
                End Function)
        Finally
            Directory.Delete(directoryPath, True)
        End Try
    End Sub

    <TestMethod>
    Public Sub Backup_PreflightsCollisionsBeforeCopyingAnyFile()
        Dim sourcePath = Path.Combine(Path.GetTempPath(), $"quotedesk-source-{Guid.NewGuid():N}")
        Dim backupPath = Path.Combine(Path.GetTempPath(), $"quotedesk-backup-{Guid.NewGuid():N}")
        Directory.CreateDirectory(sourcePath)
        Directory.CreateDirectory(backupPath)
        Try
            File.WriteAllText(Path.Combine(sourcePath, "a.quote.json"), "source-a")
            File.WriteAllText(Path.Combine(sourcePath, "b.quote.json"), "source-b")
            File.WriteAllText(Path.Combine(backupPath, "b.quote.json"), "existing-b")

            Assert.ThrowsExactly(Of IOException)(
                Function()
                    Return QuoteBackupService.CreateBackup(sourcePath, backupPath)
                End Function)

            Assert.IsFalse(File.Exists(Path.Combine(backupPath, "a.quote.json")))
            Assert.AreEqual("existing-b", File.ReadAllText(Path.Combine(backupPath, "b.quote.json")))
        Finally
            Directory.Delete(sourcePath, True)
            Directory.Delete(backupPath, True)
        End Try
    End Sub
End Class
