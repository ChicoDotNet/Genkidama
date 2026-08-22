VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "TvBridgeDevice"
Option Explicit
Implements IBridgeDevice

Private Function IBridgeDevice_PowerOn() As String
    IBridgeDevice_PowerOn = "TV:on"
End Function

Private Function IBridgeDevice_Mute() As String
    IBridgeDevice_Mute = "TV:muted"
End Function
