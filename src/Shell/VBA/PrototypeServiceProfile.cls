VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "PrototypeServiceProfile"
Option Explicit

Private mName As String
Private mFeatures As Collection

Private Sub Class_Initialize()
    Set mFeatures = New Collection
End Sub

Public Property Get Name() As String
    Name = mName
End Property

Public Property Let Name(ByVal value As String)
    mName = value
End Property

Public Sub AddFeature(ByVal feature As String)
    mFeatures.Add feature
End Sub

Public Function CloneProfile() As PrototypeServiceProfile
    Dim copy As New PrototypeServiceProfile
    Dim feature As Variant

    copy.Name = mName
    For Each feature In mFeatures
        copy.AddFeature CStr(feature)
    Next feature

    Set CloneProfile = copy
End Function

Public Function Describe() As String
    Dim feature As Variant
    Dim text As String

    For Each feature In mFeatures
        If Len(text) > 0 Then text = text & ","
        text = text & CStr(feature)
    Next feature

    Describe = mName & ": " & text
End Function
