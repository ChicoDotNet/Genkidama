Imports System

Public Interface IDevice
    Function TurnOn() As String
    Function Mute() As String
End Interface

Public NotInheritable Class TvDevice
    Implements IDevice
    Public Function TurnOn() As String Implements IDevice.TurnOn
        Return "TV:on"
    End Function
    Public Function Mute() As String Implements IDevice.Mute
        Return "TV:muted"
    End Function
End Class

Public NotInheritable Class RadioDevice
    Implements IDevice
    Public Function TurnOn() As String Implements IDevice.TurnOn
        Return "Radio:on"
    End Function
    Public Function Mute() As String Implements IDevice.Mute
        Return "Radio:muted"
    End Function
End Class

Public MustInherit Class Remote
    Protected ReadOnly Device As IDevice
    Protected Sub New(device As IDevice)
        Me.Device = device
    End Sub
    Public MustOverride Function Execute() As String
End Class

Public NotInheritable Class BasicRemote
    Inherits Remote
    Public Sub New(device As IDevice)
        MyBase.New(device)
    End Sub
    Public Overrides Function Execute() As String
        Return Device.TurnOn()
    End Function
End Class

Public NotInheritable Class MuteRemote
    Inherits Remote
    Public Sub New(device As IDevice)
        MyBase.New(device)
    End Sub
    Public Overrides Function Execute() As String
        Return Device.Mute()
    End Function
End Class

Module BridgeExample
    Sub Main()
        Console.WriteLine("basic-tv=" & New BasicRemote(New TvDevice()).Execute())
        Console.WriteLine("basic-radio=" & New BasicRemote(New RadioDevice()).Execute())
        Console.WriteLine("mute-tv=" & New MuteRemote(New TvDevice()).Execute())
        Console.WriteLine("mute-radio=" & New MuteRemote(New RadioDevice()).Execute())
    End Sub
End Module
