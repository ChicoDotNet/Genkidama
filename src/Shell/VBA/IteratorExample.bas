Attribute VB_Name = "IteratorExample"
Option Explicit

Public Sub RunIteratorExample()
    Dim values As Collection
    Set values = New Collection
    values.Add 10
    values.Add 20
    values.Add 30

    Dim current As Variant
    Dim rendered As String
    For Each current In values
        If Len(rendered) > 0 Then rendered = rendered & ","
        rendered = rendered & CStr(current)
    Next current

    If rendered <> "10,20,30" Then Err.Raise 5, , "Iterator contract failed"
    Debug.Print "iterator=" & rendered
End Sub
