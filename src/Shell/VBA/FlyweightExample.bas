Option Explicit

Private mStyles As Object

Private Function StylePool() As Object
    If mStyles Is Nothing Then
        Set mStyles = CreateObject("Scripting.Dictionary")
    End If
    Set StylePool = mStyles
End Function

Public Function GetStyleId(ByVal font As String, ByVal fontSize As Long, ByVal color As String) As Long
    Dim pool As Object
    Dim key As String
    Set pool = StylePool()
    key = font & "|" & CStr(fontSize) & "|" & color
    If Not pool.Exists(key) Then
        pool.Add key, pool.Count + 1
    End If
    GetStyleId = CLng(pool(key))
End Function

Public Sub DemoFlyweight()
    Dim red1 As Long
    Dim red2 As Long
    Dim blue As Long
    red1 = GetStyleId("Inter", 12, "red")
    red2 = GetStyleId("Inter", 12, "red")
    blue = GetStyleId("Inter", 12, "blue")
    Debug.Assert blue <> red1
    Debug.Print "styles=" & CStr(StylePool().Count) & _
                ";shared=" & LCase$(CStr(red1 = red2)) & ";text=ABC"
End Sub
