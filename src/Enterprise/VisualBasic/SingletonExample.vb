Public NotInheritable Class Registry
    Private Shared ReadOnly _instance As New Registry()
    Private _count As Integer

    Private Sub New()
    End Sub

    Public Shared ReadOnly Property Instance As Registry
        Get
            Return _instance
        End Get
    End Property

    Public Sub Increment()
        _count += 1
    End Sub

    Public ReadOnly Property Count As Integer
        Get
            Return _count
        End Get
    End Property
End Class

Module SingletonExample
    Sub Main()
        Dim first = Registry.Instance
        Dim second = Registry.Instance
        first.Increment()

        Console.WriteLine($"same={first Is second}".ToLowerInvariant())
        Console.WriteLine($"count={second.Count}")
    End Sub
End Module
