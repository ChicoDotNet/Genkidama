Imports System
Friend Module VisitorExample
    Friend Function Run() As Boolean
        Dim area=Math.PI*2*2+3*4
        Return Math.Abs(area-(4*Math.PI+12))<0.000000001
    End Function
End Module
