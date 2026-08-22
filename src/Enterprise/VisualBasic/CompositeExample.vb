Imports System
Imports System.Collections.Generic

Public Interface IComponent
    Function Size() As Integer
End Interface

Public NotInheritable Class FileLeaf
    Implements IComponent

    Private ReadOnly _bytes As Integer

    Public Sub New(bytes As Integer)
        _bytes = bytes
    End Sub

    Public Function Size() As Integer Implements IComponent.Size
        Return _bytes
    End Function
End Class

Public NotInheritable Class FolderComposite
    Implements IComponent

    Private ReadOnly _children As New List(Of IComponent)()

    Public Sub Add(child As IComponent)
        _children.Add(child)
    End Sub

    Public Function Size() As Integer Implements IComponent.Size
        Dim total = 0
        For Each child In _children
            total += child.Size()
        Next
        Return total
    End Function
End Class

Public Module CompositeExample
    Public Sub Main()
        Dim readme As IComponent = New FileLeaf(2)
        Dim docs = New FolderComposite()
        docs.Add(New FileLeaf(3))
        docs.Add(New FileLeaf(5))

        Dim root = New FolderComposite()
        root.Add(New FileLeaf(2))
        root.Add(docs)

        Console.WriteLine($"leaf={readme.Size()}")
        Console.WriteLine($"docs={docs.Size()}")
        Console.WriteLine($"root={root.Size()}")
    End Sub
End Module
