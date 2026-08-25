Imports System.Collections.ObjectModel

Namespace Core
    ''' <summary>Immutable invoice created from an approved quote.</summary>
    Public NotInheritable Class InvoiceDocument
        Private ReadOnly _lines As IReadOnlyList(Of QuoteLine)

        ''' <summary>Creates an immutable invoice and verifies that supplied totals agree with its lines.</summary>
        Public Sub New(invoiceNumber As String, customerName As String, lines As IEnumerable(Of QuoteLine), subtotal As Decimal, taxAmount As Decimal, total As Decimal)
            If String.IsNullOrWhiteSpace(invoiceNumber) Then Throw New ArgumentException("El folio de factura es obligatorio.", NameOf(invoiceNumber))
            If String.IsNullOrWhiteSpace(customerName) Then Throw New ArgumentException("El cliente es obligatorio.", NameOf(customerName))
            ArgumentNullException.ThrowIfNull(lines)

            Dim lineList = lines.ToList()
            If lineList.Count = 0 Then Throw New ArgumentException("La factura necesita al menos una partida.", NameOf(lines))
            If lineList.Any(Function(line) line Is Nothing) Then Throw New ArgumentException("Una partida de factura no puede ser nula.", NameOf(lines))
            Dim calculatedSubtotal = lineList.Sum(Function(line) line.LineTotal)
            If subtotal <> calculatedSubtotal Then Throw New ArgumentException("El subtotal no coincide con las partidas.", NameOf(subtotal))
            If taxAmount < 0D Then Throw New ArgumentOutOfRangeException(NameOf(taxAmount), "El impuesto no puede ser negativo.")
            If total <> subtotal + taxAmount Then Throw New ArgumentException("El total no coincide con subtotal más impuesto.", NameOf(total))

            Me.InvoiceNumber = invoiceNumber.Trim()
            Me.CustomerName = customerName.Trim()
            _lines = New ReadOnlyCollection(Of QuoteLine)(lineList)
            Me.Subtotal = subtotal
            Me.TaxAmount = taxAmount
            Me.Total = total
        End Sub

        ''' <summary>Gets the caller-provided invoice identifier.</summary>
        Public ReadOnly Property InvoiceNumber As String
        ''' <summary>Gets the customer copied from the approved quote.</summary>
        Public ReadOnly Property CustomerName As String
        ''' <summary>Gets an immutable snapshot of invoiced lines.</summary>
        Public ReadOnly Property Lines As IReadOnlyList(Of QuoteLine)
            Get
                Return _lines
            End Get
        End Property
        ''' <summary>Gets the invoice subtotal.</summary>
        Public ReadOnly Property Subtotal As Decimal
        ''' <summary>Gets the invoice tax amount.</summary>
        Public ReadOnly Property TaxAmount As Decimal
        ''' <summary>Gets the invoice total.</summary>
        Public ReadOnly Property Total As Decimal
    End Class

    ''' <summary>Converts approved quotes into immutable invoice documents.</summary>
    Public NotInheritable Class QuoteInvoiceService
        Private Sub New()
        End Sub

        ''' <summary>Creates an invoice without mutating the approved quote.</summary>
        Public Shared Function CreateInvoice(quote As QuoteDraft, invoiceNumber As String) As InvoiceDocument
            ArgumentNullException.ThrowIfNull(quote)
            If quote.Status <> QuoteStatus.Approved Then
                Throw New InvalidOperationException("Sólo una cotización aprobada puede convertirse en factura.")
            End If

            Return New InvoiceDocument(invoiceNumber, quote.CustomerName, quote.Lines, quote.Subtotal, quote.TaxAmount, quote.Total)
        End Function
    End Class
End Namespace
