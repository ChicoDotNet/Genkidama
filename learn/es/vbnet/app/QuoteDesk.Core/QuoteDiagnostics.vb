Imports System.Globalization
Imports System.IO
Imports System.Text

Namespace Core
    ''' <summary>Privacy-safe operational counters for a directory of persisted quotes.</summary>
    Public NotInheritable Class QuoteDiagnosticSnapshot
        ''' <summary>Creates a diagnostics snapshot containing only aggregate operational data.</summary>
        Public Sub New(directoryPath As String, validDocuments As Integer, invalidDocuments As Integer, draftDocuments As Integer, approvedDocuments As Integer, aggregateTotal As Decimal)
            Me.DirectoryPath = directoryPath
            Me.ValidDocuments = validDocuments
            Me.InvalidDocuments = invalidDocuments
            Me.DraftDocuments = draftDocuments
            Me.ApprovedDocuments = approvedDocuments
            Me.AggregateTotal = aggregateTotal
        End Sub

        ''' <summary>Gets the inspected directory path.</summary>
        Public ReadOnly Property DirectoryPath As String
        ''' <summary>Gets the count of valid quote documents.</summary>
        Public ReadOnly Property ValidDocuments As Integer
        ''' <summary>Gets the count of invalid or unreadable quote documents.</summary>
        Public ReadOnly Property InvalidDocuments As Integer
        ''' <summary>Gets the count of valid draft quotes.</summary>
        Public ReadOnly Property DraftDocuments As Integer
        ''' <summary>Gets the count of valid approved quotes.</summary>
        Public ReadOnly Property ApprovedDocuments As Integer
        ''' <summary>Gets the aggregate total of valid quotes.</summary>
        Public ReadOnly Property AggregateTotal As Decimal
    End Class

    ''' <summary>Builds read-only operational diagnostics without customer names or line descriptions.</summary>
    Public NotInheritable Class QuoteDiagnostics
        Private ReadOnly _catalog As QuoteCatalog

        ''' <summary>Creates diagnostics using the provided catalog or the default catalog.</summary>
        Public Sub New(Optional catalog As QuoteCatalog = Nothing)
            _catalog = If(catalog, New QuoteCatalog())
        End Sub

        ''' <summary>Inspects one directory and returns aggregate counts without mutating persisted documents.</summary>
        Public Function Inspect(directoryPath As String) As QuoteDiagnosticSnapshot
            Dim result = _catalog.Search(directoryPath)
            Dim draftCount = result.Entries.Where(Function(entry) entry.Status = QuoteStatus.Draft).Count()
            Dim approvedCount = result.Entries.Where(Function(entry) entry.Status = QuoteStatus.Approved).Count()
            Dim aggregateTotal = result.Entries.Sum(Function(entry) entry.Total)
            Return New QuoteDiagnosticSnapshot(Path.GetFullPath(directoryPath), result.Entries.Count, result.Issues.Count, draftCount, approvedCount, aggregateTotal)
        End Function
    End Class

    ''' <summary>Formats operational diagnostics as deterministic human-readable text without PII.</summary>
    Public NotInheritable Class QuoteDiagnosticsFormatter
        Private Sub New()
        End Sub

        ''' <summary>Returns stable key/value lines suitable for console output or support tickets.</summary>
        Public Shared Function Format(snapshot As QuoteDiagnosticSnapshot) As String
            ArgumentNullException.ThrowIfNull(snapshot)
            Dim builder As New StringBuilder()
            builder.AppendLine($"valid={snapshot.ValidDocuments}")
            builder.AppendLine($"invalid={snapshot.InvalidDocuments}")
            builder.AppendLine($"draft={snapshot.DraftDocuments}")
            builder.AppendLine($"approved={snapshot.ApprovedDocuments}")
            builder.Append($"aggregate_total={snapshot.AggregateTotal.ToString("0.00", CultureInfo.InvariantCulture)}")
            Return builder.ToString()
        End Function
    End Class
End Namespace
