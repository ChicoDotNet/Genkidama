Imports System.IO

Namespace Core
    ''' <summary>Describes one persisted quote without exposing its mutable domain object.</summary>
    Public NotInheritable Class QuoteCatalogEntry
        ''' <summary>Creates a deterministic summary for one valid persisted quote.</summary>
        Public Sub New(filePath As String, customerName As String, status As QuoteStatus, total As Decimal)
            Me.FilePath = filePath
            Me.CustomerName = customerName
            Me.Status = status
            Me.Total = total
        End Sub

        ''' <summary>Gets the absolute or caller-resolved path of the persisted quote.</summary>
        Public ReadOnly Property FilePath As String
        ''' <summary>Gets the customer name restored through the quote domain contract.</summary>
        Public ReadOnly Property CustomerName As String
        ''' <summary>Gets the restored quote lifecycle state.</summary>
        Public ReadOnly Property Status As QuoteStatus
        ''' <summary>Gets the derived quote total.</summary>
        Public ReadOnly Property Total As Decimal
    End Class

    ''' <summary>Records a file that could not be interpreted as a quote.</summary>
    Public NotInheritable Class QuoteCatalogIssue
        ''' <summary>Creates an observable issue for one failed persisted document.</summary>
        Public Sub New(filePath As String, message As String)
            Me.FilePath = filePath
            Me.Message = message
        End Sub

        ''' <summary>Gets the path that produced the issue.</summary>
        Public ReadOnly Property FilePath As String
        ''' <summary>Gets the actionable failure message without inventing quote data.</summary>
        Public ReadOnly Property Message As String
    End Class

    ''' <summary>Returns valid quote summaries and explicit per-file failures.</summary>
    Public NotInheritable Class QuoteCatalogResult
        ''' <summary>Creates a catalog result from already ordered entries and observed issues.</summary>
        Public Sub New(entries As IReadOnlyList(Of QuoteCatalogEntry), issues As IReadOnlyList(Of QuoteCatalogIssue))
            ArgumentNullException.ThrowIfNull(entries)
            ArgumentNullException.ThrowIfNull(issues)
            Me.Entries = entries
            Me.Issues = issues
        End Sub

        ''' <summary>Gets valid quote summaries in deterministic file-name order.</summary>
        Public ReadOnly Property Entries As IReadOnlyList(Of QuoteCatalogEntry)
        ''' <summary>Gets per-file failures encountered during discovery.</summary>
        Public ReadOnly Property Issues As IReadOnlyList(Of QuoteCatalogIssue)
    End Class

    ''' <summary>Lists and searches persisted quote documents in a directory.</summary>
    Public NotInheritable Class QuoteCatalog
        Private ReadOnly _store As QuoteFileStore

        ''' <summary>Creates a catalog using the provided store or a default versioned JSON store.</summary>
        Public Sub New(Optional store As QuoteFileStore = Nothing)
            _store = If(store, New QuoteFileStore())
        End Sub

        ''' <summary>Loads *.quote.json files in deterministic order, preserving corrupt-file failures as observable issues.</summary>
        Public Function Search(directoryPath As String, Optional term As String = "") As QuoteCatalogResult
            If String.IsNullOrWhiteSpace(directoryPath) Then Throw New ArgumentException("El directorio es obligatorio.", NameOf(directoryPath))
            Dim fullDirectory = Path.GetFullPath(directoryPath)
            If Not Directory.Exists(fullDirectory) Then Throw New DirectoryNotFoundException($"No existe el directorio: {fullDirectory}")

            Dim files = Directory.GetFiles(fullDirectory, "*.quote.json", SearchOption.TopDirectoryOnly).ToList()
            files.Sort(StringComparer.OrdinalIgnoreCase)
            Dim entries As New List(Of QuoteCatalogEntry)()
            Dim issues As New List(Of QuoteCatalogIssue)()
            Dim normalizedTerm = If(term, String.Empty).Trim()

            For Each filePath In files
                Try
                    Dim quote = _store.Load(filePath)
                    If normalizedTerm.Length = 0 OrElse quote.CustomerName.Contains(normalizedTerm, StringComparison.OrdinalIgnoreCase) Then
                        entries.Add(New QuoteCatalogEntry(filePath, quote.CustomerName, quote.Status, quote.Total))
                    End If
                Catch ex As Exception When TypeOf ex Is InvalidDataException OrElse TypeOf ex Is IOException OrElse TypeOf ex Is UnauthorizedAccessException
                    issues.Add(New QuoteCatalogIssue(filePath, ex.Message))
                End Try
            Next

            Return New QuoteCatalogResult(entries, issues)
        End Function
    End Class
End Namespace
