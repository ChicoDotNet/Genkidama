VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "CompositeFileLeaf"
Option Explicit

Implements ICompositeComponent

Private mBytes As Long

Public Sub Initialize(ByVal bytes As Long)
    mBytes = bytes
End Sub

Private Function ICompositeComponent_Size() As Long
    ICompositeComponent_Size = mBytes
End Function
