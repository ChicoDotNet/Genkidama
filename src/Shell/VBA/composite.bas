Attribute VB_Name = "CompositeExample"
Option Explicit

Private Function FileNode(ByVal bytes As Long) As ICompositeComponent
    Dim leaf As New CompositeFileLeaf
    leaf.Initialize bytes
    Set FileNode = leaf
End Function

Public Sub RunCompositeExample()
    Dim readme As ICompositeComponent
    Set readme = FileNode(2)

    Dim docs As New CompositeFolder
    docs.Add FileNode(3)
    docs.Add FileNode(5)
    Dim docsComponent As ICompositeComponent
    Set docsComponent = docs

    Dim root As New CompositeFolder
    root.Add readme
    root.Add docsComponent
    Dim rootComponent As ICompositeComponent
    Set rootComponent = root

    Debug.Print "leaf=" & readme.Size
    Debug.Print "docs=" & docsComponent.Size
    Debug.Print "root=" & rootComponent.Size
End Sub
