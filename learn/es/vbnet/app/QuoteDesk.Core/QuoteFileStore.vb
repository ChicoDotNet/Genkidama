Imports System.IO
Imports System.Text.Json

Namespace Core
    ''' <summary>Persists one quote as versioned JSON at an explicit file-system boundary.</summary>
    Public NotInheritable Class QuoteFileStore
        Private Const CurrentSchemaVersion As Integer = 1
        Private ReadOnly _options As New JsonSerializerOptions With {.WriteIndented = True}

        ''' <summary>Saves a quote using a temporary sibling file before replacing the destination.</summary>
        Public Sub Save(filePath As String, quote As QuoteDraft)
            If String.IsNullOrWhiteSpace(filePath) Then Throw New ArgumentException("La ruta es obligatoria.", NameOf(filePath))
            ArgumentNullException.ThrowIfNull(quote)

            Dim fullPath = Path.GetFullPath(filePath)
            Dim directoryPath = Path.GetDirectoryName(fullPath)
            If Not String.IsNullOrEmpty(directoryPath) Then Directory.CreateDirectory(directoryPath)

            Dim payload = QuoteFileData.FromQuote(quote)
            Dim json = JsonSerializer.Serialize(payload, _options)
            Dim tempPath = fullPath & ".tmp"

            Try
                File.WriteAllText(tempPath, json)
                File.Move(tempPath, fullPath, True)
            Finally
                If File.Exists(tempPath) Then File.Delete(tempPath)
            End Try
        End Sub

        ''' <summary>Loads and validates a persisted quote; malformed or unsupported data fails explicitly.</summary>
        Public Function Load(filePath As String) As QuoteDraft
            If String.IsNullOrWhiteSpace(filePath) Then Throw New ArgumentException("La ruta es obligatoria.", NameOf(filePath))

            Try
                Dim json = File.ReadAllText(Path.GetFullPath(filePath))
                Dim payload = JsonSerializer.Deserialize(Of QuoteFileData)(json, _options)
                If payload Is Nothing Then Throw New InvalidDataException("El archivo no contiene una cotización.")
                If payload.SchemaVersion <> CurrentSchemaVersion Then Throw New InvalidDataException($"Versión de cotización no soportada: {payload.SchemaVersion}.")
                If payload.Lines Is Nothing Then Throw New InvalidDataException("La colección de partidas es obligatoria.")
                If Not [Enum].IsDefined(payload.Status) Then Throw New InvalidDataException($"Estado de cotización no soportado: {payload.Status}.")

                Dim quote = New QuoteDraft With {
                    .CustomerName = payload.CustomerName,
                    .TaxRate = payload.TaxRate
                }

                For Each item In payload.Lines
                    If item Is Nothing Then Throw New InvalidDataException("Una partida no puede ser nula.")
                    quote.AddLine(New QuoteLine(item.Description, item.Quantity, item.UnitPrice))
                Next

                quote.RestoreStatus(payload.Status)
                Return quote
            Catch ex As JsonException
                Throw New InvalidDataException("El archivo de cotización contiene JSON inválido.", ex)
            Catch ex As ArgumentException
                Throw New InvalidDataException("El archivo de cotización contiene datos inválidos.", ex)
            Catch ex As InvalidOperationException
                Throw New InvalidDataException("El archivo de cotización viola las reglas del dominio.", ex)
            End Try
        End Function

        Private NotInheritable Class QuoteFileData
            Public Property SchemaVersion As Integer
            Public Property CustomerName As String = String.Empty
            Public Property TaxRate As Decimal
            Public Property Status As QuoteStatus
            Public Property Lines As List(Of QuoteLineData) = New List(Of QuoteLineData)()

            Public Shared Function FromQuote(quote As QuoteDraft) As QuoteFileData
                Dim result = New QuoteFileData With {
                    .SchemaVersion = CurrentSchemaVersion,
                    .CustomerName = quote.CustomerName,
                    .TaxRate = quote.TaxRate,
                    .Status = quote.Status
                }

                For Each line In quote.Lines
                    result.Lines.Add(New QuoteLineData With {
                        .Description = line.Description,
                        .Quantity = line.Quantity,
                        .UnitPrice = line.UnitPrice
                    })
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
