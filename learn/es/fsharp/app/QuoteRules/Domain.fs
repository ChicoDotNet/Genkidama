namespace QuoteRules

type CustomerTier =
    | Standard
    | Preferred
    | Partner

type QuoteLine =
    { Description: string
      Quantity: int
      UnitPrice: decimal }

type Quote =
    { Lines: QuoteLine list
      Subtotal: decimal
      DiscountRate: decimal
      Discount: decimal
      Total: decimal }
