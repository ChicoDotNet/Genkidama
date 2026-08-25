Imports System.Collections.ObjectModel

Namespace Core
    ''' <summary>Lifecycle states for a quote before it becomes an invoice.</summary>
    Public Enum QuoteStatus
        Draft = 0
        Approved = 1
    End Enum

    ''' <summary>Owns editable quote state without depending on Windows Forms or persistence.</summary>
    Public NotInheritable Class QuoteDraft
        Private ReadOnly _lines As New List(Of QuoteLine)()
        Private _customerName As String = String.Empty
        Private _taxRate As Decimal = 0.16D
        Private _status As QuoteStatus = QuoteStatus.Draft

        ''' <summary>Gets or changes the customer while the quote remains a draft.</summary>
        Public Property CustomerName As String
            Get
                Return _customerName
            End Get
            Set(value As String)
                EnsureEditable()
                _customerName = If(value, String.Empty).Trim()
            End Set
        End Property

        ''' <summary>Gets or changes the tax rate as a fraction from 0 through 1 while editable.</summary>
        Public Property TaxRate As Decimal
            Get
                Return _taxRate
            End Get
            Set(value As Decimal)
                EnsureEditable()
                If value < 0D OrElse value > 1D Then
                    Throw New ArgumentOutOfRangeException(NameOf(value), "La tasa debe estar entre 0 y 1.")
                End If
                _taxRate = value
            End Set
        End Property

        ''' <summary>Gets the current quote lifecycle state.</summary>
        Public ReadOnly Property Status As QuoteStatus
            Get
                Return _status
            End Get
        End Property

        ''' <summary>Gets an immutable view of the current line collection.</summary>
        Public ReadOnly Property Lines As IReadOnlyList(Of QuoteLine)
            Get
                Return New ReadOnlyCollection(Of QuoteLine)(_lines)
            End Get
        End Property

        ''' <summary>Gets the sum of all line totals.</summary>
        Public ReadOnly Property Subtotal As Decimal
            Get
                Return _lines.Sum(Function(line) line.LineTotal)
            End Get
        End Property

        ''' <summary>Gets tax rounded to currency precision using away-from-zero midpoint rounding.</summary>
        Public ReadOnly Property TaxAmount As Decimal
            Get
                Return Math.Round(Subtotal * TaxRate, 2, MidpointRounding.AwayFromZero)
            End Get
        End Property

        ''' <summary>Gets subtotal plus tax.</summary>
        Public ReadOnly Property Total As Decimal
            Get
                Return Subtotal + TaxAmount
            End Get
        End Property

        ''' <summary>Adds a validated line while the quote is editable.</summary>
        Public Sub AddLine(line As QuoteLine)
            EnsureEditable()
            ArgumentNullException.ThrowIfNull(line)
            _lines.Add(line)
        End Sub

        ''' <summary>Replaces a line by zero-based index while the quote is editable.</summary>
        Public Sub ReplaceLine(index As Integer, line As QuoteLine)
            EnsureEditable()
            ArgumentNullException.ThrowIfNull(line)
            If index < 0 OrElse index >= _lines.Count Then Throw New ArgumentOutOfRangeException(NameOf(index))
            _lines(index) = line
        End Sub

        ''' <summary>Removes a line by zero-based index while the quote is editable.</summary>
        Public Sub RemoveLine(index As Integer)
            EnsureEditable()
            If index < 0 OrElse index >= _lines.Count Then Throw New ArgumentOutOfRangeException(NameOf(index))
            _lines.RemoveAt(index)
        End Sub

        ''' <summary>Approves a non-empty quote with a customer and freezes its editable fields.</summary>
        Public Sub Approve()
            EnsureEditable()
            If String.IsNullOrWhiteSpace(CustomerName) Then Throw New InvalidOperationException("La cotización necesita un cliente antes de aprobarse.")
            If _lines.Count = 0 Then Throw New InvalidOperationException("La cotización necesita al menos una partida antes de aprobarse.")
            _status = QuoteStatus.Approved
        End Sub

        Friend Sub RestoreStatus(status As QuoteStatus)
            If status = QuoteStatus.Approved Then Approve()
        End Sub

        Private Sub EnsureEditable()
            If _status <> QuoteStatus.Draft Then
                Throw New InvalidOperationException("La cotización aprobada ya no puede editarse.")
            End If
        End Sub
    End Class
End Namespace
