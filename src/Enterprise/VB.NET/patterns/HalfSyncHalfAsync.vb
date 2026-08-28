Imports System.Linq
Friend Module HalfSyncHalfAsyncExample
    Friend Function Run() As Boolean
        Return String.Join(">",{"job-1","job-2","job-3"}.Select(Function(j)$"done:{j}"))="done:job-1>done:job-2>done:job-3"
    End Function
End Module
