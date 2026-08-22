Attribute VB_Name = "PrototypeExample"
Option Explicit

Public Sub RunPrototypeExample()
    Dim original As New PrototypeServiceProfile
    Dim canary As PrototypeServiceProfile

    original.Name = "orders"
    original.AddFeature "metrics"

    Set canary = original.CloneProfile()
    canary.Name = "orders-canary"
    canary.AddFeature "tracing"

    Debug.Print "original=" & original.Describe()
    Debug.Print "clone=" & canary.Describe()
End Sub
