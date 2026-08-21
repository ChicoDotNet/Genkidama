defmodule Registry do
  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> 0 end, name: @name)
  end

  def instance, do: Process.whereis(@name)
  def increment, do: Agent.update(@name, &(&1 + 1))
  def count, do: Agent.get(@name, & &1)
end

{:ok, _pid} = Registry.start_link()
first = Registry.instance()
second = Registry.instance()
Registry.increment()

IO.puts("same=#{if first == second, do: "true", else: "false"}")
IO.puts("count=#{Registry.count()}")
