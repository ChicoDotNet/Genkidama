VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CompositeFolder"
Option Explicit

Implements ICompositeComponent

Private mChildren As Collection

Private Sub Class_Initialize()
    Set mChildren = New Collection
End Sub

Public Sub Add(ByVal child As ICompositeComponent)
    mChildren.Add child
End Sub

Private Function ICompositeComponent_Size() As Long
    Dim child As ICompositeComponent
    Dim total As Long

    For Each child In mChildren
        total = total + child.Size
    Next child

    ICompositeComponent_Size = total
End Function
