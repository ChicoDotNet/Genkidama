Friend Module InterpreterExample
    Private Function EvalExpr(kind As String,a As Integer,b As Integer) As Integer
        Return If(kind="add",a+b,a*b)
    End Function
    Friend Function Run() As Boolean
        Return EvalExpr("add",7,EvalExpr("mul",3,4))=19
    End Function
End Module
