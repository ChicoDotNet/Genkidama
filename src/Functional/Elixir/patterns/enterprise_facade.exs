stock_available = fn -> true end
charge = fn -> :paid end

checkout = fn ->
  if stock_available.(), do: charge.(), else: :sold_out
end

unless checkout.() == :paid do
  raise "Facade"
end
