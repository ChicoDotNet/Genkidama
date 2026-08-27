Imports System.Collections.Generic

Public Interface IDocumentStore
    Function [Get](id As Integer) As String
End Interface

Public NotInheritable Class RemoteDocumentStore
    Implements IDocumentStore

    Private _fetchCount As Integer

    Public ReadOnly Property FetchCount As Integer
        Get
            Return _fetchCount
        End Get
    End Property

    Public Function [Get](id As Integer) As String Implements IDocumentStore.Get
        _fetchCount += 1
        Return $"doc({id})"
    End Function
End Class

Public NotInheritable Class DocumentStoreProxy
    Implements IDocumentStore

    Private _backend As RemoteDocumentStore
    Private ReadOnly _cache As New Dictionary(Of Integer, String)()

    Public ReadOnly Property BackendCount As Integer
        Get
            Return If(_backend Is Nothing, 0, 1)
        End Get
    End Property

    Public ReadOnly Property FetchCount As Integer
        Get
            Return If(_backend Is Nothing, 0, _backend.FetchCount)
        End Get
    End Property

    Public Function [Get](id As Integer) As String Implements IDocumentStore.Get
        Dim cached As String = Nothing
        If _cache.TryGetValue(id, cached) Then
            Return cached
        End If

        If _backend Is Nothing Then
            _backend = New RemoteDocumentStore()
        End If

        Dim value = _backend.Get(id)
        _cache(id) = value
        Return value
    End Function
End Class

Module Program
    Sub Main()
        Dim store = New DocumentStoreProxy()
        Dim first = store.Get(42)
        Dim secondValue = store.Get(42)
        Console.WriteLine($"backend={store.BackendCount};fetches={store.FetchCount};first={first};second={secondValue}")
    End Sub
End Module
