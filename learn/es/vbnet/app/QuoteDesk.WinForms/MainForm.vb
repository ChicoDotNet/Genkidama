Imports System.Windows.Forms
Imports QuoteDesk.Core

Namespace WinForms
    Public NotInheritable Class MainForm
        Inherits Form
        Implements IQuoteView

        Private ReadOnly customerBox As New TextBox()
        Private ReadOnly descriptionBox As New TextBox()
        Private ReadOnly quantityBox As New TextBox() With {.Text = "1"}
        Private ReadOnly priceBox As New TextBox() With {.Text = "0"}
        Private ReadOnly linesList As New ListBox()
        Private ReadOnly totalLabel As New Label() With {.AutoSize = True, .Text = "Total: 0.00"}
        Private ReadOnly errorLabel As New Label() With {.AutoSize = True}
        Private ReadOnly addButton As New Button() With {.Text = "Agregar partida", .AutoSize = True}
        Private ReadOnly presenter As QuotePresenter

        Public Sub New()
            Text = "QuoteDesk — Cotización"
            Width = 720
            Height = 520

            Dim layout As New TableLayoutPanel() With {.Dock = DockStyle.Fill, .ColumnCount = 2, .RowCount = 7, .Padding = New Padding(16)}
            layout.ColumnStyles.Add(New ColumnStyle(SizeType.AutoSize))
            layout.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 100))

            AddRow(layout, 0, "Cliente", customerBox)
            AddRow(layout, 1, "Descripción", descriptionBox)
            AddRow(layout, 2, "Cantidad", quantityBox)
            AddRow(layout, 3, "Precio unitario", priceBox)
            layout.Controls.Add(addButton, 1, 4)
            layout.Controls.Add(linesList, 0, 5)
            layout.SetColumnSpan(linesList, 2)
            linesList.Dock = DockStyle.Fill
            layout.Controls.Add(totalLabel, 0, 6)
            layout.Controls.Add(errorLabel, 1, 6)
            Controls.Add(layout)

            AddHandler addButton.Click, Sub() RaiseEvent AddLineRequested(Me, EventArgs.Empty)
            presenter = New QuotePresenter(Me)
        End Sub

        Public Event AddLineRequested As EventHandler Implements IQuoteView.AddLineRequested

        Public ReadOnly Property CustomerNameInput As String Implements IQuoteView.CustomerNameInput
            Get
                Return customerBox.Text
            End Get
        End Property

        Public ReadOnly Property DescriptionInput As String Implements IQuoteView.DescriptionInput
            Get
                Return descriptionBox.Text
            End Get
        End Property

        Public ReadOnly Property QuantityInput As String Implements IQuoteView.QuantityInput
            Get
                Return quantityBox.Text
            End Get
        End Property

        Public ReadOnly Property UnitPriceInput As String Implements IQuoteView.UnitPriceInput
            Get
                Return priceBox.Text
            End Get
        End Property

        Public Sub Render(draft As QuoteDraft) Implements IQuoteView.Render
            linesList.Items.Clear()
            For Each line In draft.Lines
                linesList.Items.Add($"{line.Quantity} × {line.Description} = {line.LineTotal:N2}")
            Next
            totalLabel.Text = $"Subtotal: {draft.Subtotal:N2} · Impuesto: {draft.TaxAmount:N2} · Total: {draft.Total:N2}"
            Text = $"QuoteDesk — {draft.Status}"
            errorLabel.Text = String.Empty
        End Sub

        Public Sub ShowError(message As String) Implements IQuoteView.ShowError
            errorLabel.Text = message
        End Sub

        Private Shared Sub AddRow(layout As TableLayoutPanel, row As Integer, labelText As String, control As Control)
            layout.Controls.Add(New Label() With {.Text = labelText, .AutoSize = True, .Anchor = AnchorStyles.Left}, 0, row)
            control.Dock = DockStyle.Fill
            layout.Controls.Add(control, 1, row)
        End Sub
    End Class
End Namespace
