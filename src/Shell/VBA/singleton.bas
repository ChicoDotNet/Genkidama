Option Explicit

Private Type RegistryState
    Count As Long
End Type

Private SharedRegistry As RegistryState
Private Initialized As Boolean

Private Sub EnsureRegistry()
    If Not Initialized Then
        SharedRegistry.Count = 0
        Initialized = True
    End If
End Sub

Public Sub IncrementRegistry()
    EnsureRegistry
    SharedRegistry.Count = SharedRegistry.Count + 1
End Sub

Public Function RegistryCount() As Long
    EnsureRegistry
    RegistryCount = SharedRegistry.Count
End Function

Public Function RegistryIdentity() As String
    EnsureRegistry
    RegistryIdentity = "registry"
End Function

Public Sub SingletonExample()
    Dim firstIdentity As String
    Dim secondIdentity As String

    firstIdentity = RegistryIdentity()
    secondIdentity = RegistryIdentity()
    IncrementRegistry

    Debug.Print "same=" & LCase$(CStr(firstIdentity = secondIdentity))
    Debug.Print "count=" & CStr(RegistryCount())
End Sub
