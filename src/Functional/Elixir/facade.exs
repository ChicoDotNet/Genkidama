defmodule AuthService do
  def authenticate(user), do: "auth(#{user})"
end

defmodule InventoryService do
  def reserve(sku), do: "reserve(#{sku})"
end

defmodule BillingService do
  def charge(cents), do: "charge(#{cents})"
end

defmodule CheckoutFacade do
  def checkout(user, sku, cents) do
    [
      AuthService.authenticate(user),
      InventoryService.reserve(sku),
      BillingService.charge(cents)
    ]
    |> Enum.join(">")
  end
end

IO.puts("checkout=#{CheckoutFacade.checkout("alice", "SKU-42", 499)}")
