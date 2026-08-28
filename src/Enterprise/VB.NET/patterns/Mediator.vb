Imports System
Imports System.Collections.Generic
Friend Module MediatorExample
    Friend Function Run() As Boolean
        Dim events As New List(Of String)
        Dim notify As Action(Of String,String)=Sub(sender,e)
            If sender="button" AndAlso e="click" Then events.Add("panel.refresh")
            If sender="panel" AndAlso e="loaded" Then events.Add("button.enable")
        End Sub
        notify("button","click") : notify("panel","loaded")
        Return String.Join(">",events)="panel.refresh>button.enable"
    End Function
End Module
