' Memento — procedural VBA representation
Option Explicit

Private Type MementoSnapshot
    Title As String
    Tags As String
End Type

Private Type Document
    Title As String
    Tags As String
End Type

Private Function SaveMemento(ByRef originator As Document) As MementoSnapshot
    Dim snapshot As MementoSnapshot
    snapshot.Title = originator.Title
    snapshot.Tags = originator.Tags
    SaveMemento = snapshot
End Function

Private Sub RestoreMemento(ByRef originator As Document, ByRef snapshot As MementoSnapshot)
    originator.Title = snapshot.Title
    originator.Tags = snapshot.Tags
End Sub

Public Sub VerifyMementoCanonical()
    Dim originator As Document
    Dim caretakerSnapshot As MementoSnapshot

    originator.Title = "draft"
    originator.Tags = "pattern"
    caretakerSnapshot = SaveMemento(originator)

    originator.Title = "published"
    originator.Tags = "pattern,edited"
    Debug.Assert caretakerSnapshot.Title = "draft"
    Debug.Assert caretakerSnapshot.Tags = "pattern"

    RestoreMemento originator, caretakerSnapshot
    Debug.Assert originator.Title = "draft"
    Debug.Assert originator.Tags = "pattern"

    originator.Title = "restored-edit"
    originator.Tags = "restored"
    Debug.Assert caretakerSnapshot.Title = "draft"
    Debug.Assert caretakerSnapshot.Tags = "pattern"
End Sub
