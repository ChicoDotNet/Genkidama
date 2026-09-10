VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "StrategyRegularPricing"
Option Explicit
Implements IStrategyPricing

Private Function IStrategyPricing_Price(ByVal amount As Currency) As Currency
    IStrategyPricing_Price = amount
End Function
