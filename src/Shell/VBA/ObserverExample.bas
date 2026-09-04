Option Explicit

Private mSubscribers As Collection
Private mAuditLog As Collection
Private mDashboardLog As Collection

Private Sub ResetObserverExample()
    Set mSubscribers = New Collection
    Set mAuditLog = New Collection
    Set mDashboardLog = New Collection
End Sub

Private Function Subscribe(ByVal handlerName As String) As Boolean
    On Error GoTo DuplicateHandler
    mSubscribers.Add handlerName, handlerName
    Subscribe = True
    Exit Function

DuplicateHandler:
    If Err.Number = 457 Then
        Err.Clear
        Subscribe = False
        Exit Function
    End If
    Err.Raise Err.Number, Err.Source, Err.Description
End Function

Private Function Unsubscribe(ByVal handlerName As String) As Boolean
    On Error GoTo MissingHandler
    mSubscribers.Remove handlerName
    Unsubscribe = True
    Exit Function

MissingHandler:
    Err.Clear
    Unsubscribe = False
End Function

Private Sub Publish(ByVal eventValue As String)
    Dim handlerName As Variant
    For Each handlerName In mSubscribers
        Application.Run CStr(handlerName), eventValue
    Next handlerName
End Sub

Public Sub AuditObserver(ByVal eventValue As String)
    mAuditLog.Add eventValue
End Sub

Public Sub DashboardObserver(ByVal eventValue As String)
    mDashboardLog.Add eventValue
End Sub

Public Function ObserverExamplePasses() As Boolean
    ResetObserverExample

    If Not Subscribe("AuditObserver") Then Exit Function
    If Not Subscribe("DashboardObserver") Then Exit Function
    If Subscribe("AuditObserver") Then Exit Function

    Publish "ready"
    If mAuditLog.Count <> 1 Then Exit Function
    If mDashboardLog.Count <> 1 Then Exit Function

    If Not Unsubscribe("DashboardObserver") Then Exit Function
    If Unsubscribe("DashboardObserver") Then Exit Function

    Publish "running"
    If mAuditLog.Count <> 2 Then Exit Function
    If mDashboardLog.Count <> 1 Then Exit Function

    ObserverExamplePasses = True
End Function
