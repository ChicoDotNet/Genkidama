Attribute VB_Name = "StrategyExample"
Option Explicit

Private Function ApplyPricing(ByVal amount As Currency, ByVal pricing As IStrategyPricing) As Currency
    ApplyPricing = pricing.Price(amount)
End Function

Public Sub RunStrategyExample()
    Dim regular As IStrategyPricing
    Dim vip As IStrategyPricing
    Dim regularPrice As Currency
    Dim vipPrice As Currency

    Set regular = New StrategyRegularPricing
    Set vip = New StrategyVipPricing

    regularPrice = ApplyPricing(100, regular)
    vipPrice = ApplyPricing(100, vip)

    If regularPrice <> 100 Then Err.Raise vbObjectError + 1, "StrategyExample", "regular strategy failed"
    If vipPrice <> 80 Then Err.Raise vbObjectError + 2, "StrategyExample", "VIP strategy failed"

    Debug.Print "regular=" & CStr(regularPrice) & ";vip=" & CStr(vipPrice)
End Sub
