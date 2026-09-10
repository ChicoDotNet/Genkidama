VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "StrategyVipPricing"
Option Explicit
Implements IStrategyPricing

Private Function IStrategyPricing_Price(ByVal amount As Currency) As Currency
    IStrategyPricing_Price = amount * 0.8
End Function
