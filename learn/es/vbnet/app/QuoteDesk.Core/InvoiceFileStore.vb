Imports System.IO
Imports System.Text.Json

Namespace Core
    ''' <summary>Persists immutable invoice snapshots as versioned JSON documents.</summary>
    Public NotInheritable Class InvoiceFileStore
        Private Const CurrentSchemaVersion As Integer = 1
        Private ReadOnly _options As New JsonSerializerOptions With {.WriteIndented = True}

        ''' <summary>Saves an invoice through a temporary sibling file before replacing the destination.</summary>
        Public Sub Save(filePath As String, invoice As InvoiceDocument)
            If String.IsNullOrWhiteSpace(filePath) Then Throw New ArgumentException("La ruta es obligatoria.", NameOf(filePath))
            ArgumentNullException.ThrowIfNull(invoice)
            Dim fullPath = Path.GetFullPath(filePath)
            Dim directoryPath = Path.GetDirectoryName(fullPath)
            If Not String.IsNullOrEmpty(directoryPath) Then Directory.CreateDirectory(directoryPath)

            Dim payload = InvoiceFileData.FromInvoice(invoice)
            Dim tempPath = fullPath & ".tmp"
            Try
                File.WriteAllText(tempPath, JsonSerializer.Serialize(payload, _options))
                File.Move(tempPath, fullPath, True)
            Finally
                If File.Exists(tempPath) Then File.Delete(tempPath)
            End Try
        End Sub

        ''' <summary>Loads a versioned invoice snapshot and rejects malformed, inconsistent or unsupported external data.</summary>
        Public Function Load(filePath As String) As InvoiceDocument
            If String.IsNullOrWhiteSpace(filePath) Then Throw New ArgumentException("La ruta es obligatoria.", NameOf(filePath))
            Try
                Dim payload = JsonSerializer.Deserialize(Of InvoiceFileData)(File.ReadAllText(Path.GetFullPath(filePath)), _options)
                If payload Is Nothing Then Throw New InvalidDataException("El archivo no contiene una factura.")
                If payload.SchemaVersion <> CurrentSchemaVersion Then Throw New InvalidDataException($"Versión de factura no soportada: {payload.SchemaVersion}.")
                If payload.Lines Is Nothing OrElse payload.Lines.Count = 0 Then Throw New InvalidDataException("La factura necesita al menos una partida.")
                Dim lines = payload.Lines.Select(Function(item) New QuoteLine(item.Description, item.Quantity, item.UnitPrice)).ToList()
                Dim calculatedSubtotal = lines.Sum(Function(line) line.LineTotal)
                If payload.Subtotal <> calculatedSubtotal Then Throw New InvalidDataException("El subtotal persistido no coincide con las partidas.")
                If payload.TaxAmount < 0D Then Throw New InvalidDataException("El impuesto persistido no puede ser negativo.")
                If payload.Total <> payload.Subtotal + payload.TaxAmount Then Throw New InvalidDataException("El total persistido no coincide con subtotal más impuesto.")
                Return New InvoiceDocument(payload.InvoiceNumber, payload.CustomerName, lines, payload.Subtotal, payload.TaxAmount, payload.Total)
            Catch ex As JsonException
                Throw New InvalidDataException("El archivo de factura contiene JSON inválido.", ex)
            Catch ex As ArgumentException
                Throw New InvalidDataException("El archivo de factura contiene datos inválidos.", ex)
            End Try
        End Function

        Private NotInheritable Class InvoiceFileData
            Public Property SchemaVersion As Integer
            Public Property InvoiceNumber As String = String.Empty
            Public Property CustomerName As String = String.Empty
            Public Property Subtotal As Decimal
            Public Property TaxAmount As Decimal
            Public Property Total As Decimal
            Public Property Lines As List(Of QuoteLineData) = New List(Of QuoteLineData)()

            Public Shared Function FromInvoice(invoice As InvoiceDocument) As InvoiceFileData
                Dim result = New InvoiceFileData With {.SchemaVersion = CurrentSchemaVersion, .InvoiceNumber = invoice.InvoiceNumber, .CustomerName = invoice.CustomerName, .Subtotal = invoice.Subtotal, .TaxAmount = invoice.TaxAmount, .Total = invoice.Total}
                For Each line In invoice.Lines
                    result.Lines.Add(New QuoteLineData With {.Description = line.Description, .Quantity = line.Quantity, .UnitPrice = line.UnitPrice})
                Next
                Return result
            End Function
        End Class

        Private NotInheritable Class QuoteLineData
            Public Property Description As String = String.Empty
            Public Property Quantity As Integer
            Public Property UnitPrice As Decimal
        End Class
    End Class
End Namespace
