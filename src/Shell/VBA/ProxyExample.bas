Option Explicit

Private mBackendCreated As Long
Private mFetches As Long
Private mCache As Object

Private Function CacheStore() As Object
    If mCache Is Nothing Then
        Set mCache = CreateObject("Scripting.Dictionary")
    End If
    Set CacheStore = mCache
End Function

Private Function RemoteGet(ByVal id As Long) As String
    mFetches = mFetches + 1
    RemoteGet = "doc(" & CStr(id) & ")"
End Function

Public Function ProxyGet(ByVal id As Long) As String
    Dim cache As Object
    Dim key As String
    Set cache = CacheStore()
    key = CStr(id)

    If cache.Exists(key) Then
        ProxyGet = CStr(cache(key))
        Exit Function
    End If

    If mBackendCreated = 0 Then mBackendCreated = 1
    cache.Add key, RemoteGet(id)
    ProxyGet = CStr(cache(key))
End Function

Public Sub DemoProxy()
    Dim first As String
    Dim second As String

    first = ProxyGet(42)
    second = ProxyGet(42)

    Debug.Assert mBackendCreated = 1
    Debug.Assert mFetches = 1
    Debug.Print "backend=" & CStr(mBackendCreated) & ";fetches=" & CStr(mFetches) & _
                ";first=" & first & ";second=" & second
End Sub
