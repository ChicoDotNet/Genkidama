defmodule Payment do
  def receive({:from, :inventory, :reserved}), do: :payment_ack
  def receive(_), do: :ignored
end

defmodule Inventory do
  def receive({:from, :payment, :paid}), do: :reserve_stock
  def receive(_), do: :ignored
end

defmodule Mediator do
  def new do
    %{
      payment: &Payment.receive/1,
      inventory: &Inventory.receive/1
    }
  end

  def send(mediator, recipient, message) do
    case Map.fetch(mediator, recipient) do
      {:ok, receiver} -> {:ok, receiver.(message)}
      :error -> {:error, :unknown_colleague}
    end
  end
end

mediator = Mediator.new()

unless Mediator.send(mediator, :inventory, {:from, :payment, :paid}) == {:ok, :reserve_stock} do
  raise "Mediator payment-to-inventory routing failed"
end

unless Mediator.send(mediator, :payment, {:from, :inventory, :reserved}) == {:ok, :payment_ack} do
  raise "Mediator inventory-to-payment routing failed"
end

unless Mediator.send(mediator, :shipping, {:from, :payment, :paid}) == {:error, :unknown_colleague} do
  raise "Mediator unknown-colleague failure mode failed"
end
