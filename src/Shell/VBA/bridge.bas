Attribute VB_Name = "BridgeExample"
Option Explicit

Public Sub RunBridgeExample()
    Dim tv As IBridgeDevice
    Dim radio As IBridgeDevice
    Dim basic As BasicBridgeRemote
    Dim muting As MuteBridgeRemote

    Set tv = New TvBridgeDevice
    Set radio = New RadioBridgeDevice
    Set basic = New BasicBridgeRemote
    Set muting = New MuteBridgeRemote

    basic.Initialize tv
    Debug.Print "basic-tv=" & basic.Activate()
    basic.Initialize radio
    Debug.Print "basic-radio=" & basic.Activate()

    muting.Initialize tv
    Debug.Print "mute-tv=" & muting.Activate()
    muting.Initialize radio
    Debug.Print "mute-radio=" & muting.Activate()
End Sub
