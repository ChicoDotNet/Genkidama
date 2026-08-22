VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "MuteBridgeRemote"
Option Explicit

Private device As IBridgeDevice

Public Sub Initialize(ByVal target As IBridgeDevice)
    Set device = target
End Sub

Public Function Activate() As String
    Activate = device.Mute()
End Function
