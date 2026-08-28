Imports System
Friend Module StateExample
    Friend Function Run() As Boolean
        Dim transition As Func(Of String,String,String)=Function(state,action)
            If state="locked" AndAlso action="unlock" Then Return "unlocked"
            If state="unlocked" AndAlso action="lock" Then Return "locked"
            Return state
        End Function
        Return transition(transition("locked","unlock"),"lock")="locked"
    End Function
End Module
