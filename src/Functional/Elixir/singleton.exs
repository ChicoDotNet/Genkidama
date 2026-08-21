defmodule SingletonRegistry do
  @name __MODULE__

  def start_link do
    Agent.start_link(fn -> 0 end, name: @name)
  end

  def instance, do: Process.whereis(@name)
  def increment, do: Agent.update(@name, &(&1 + 1))
  def count, do: Agent.get(@name, & &1)
end

{:ok, _pid} = SingletonRegistry.start_link()
first = SingletonRegistry.instance()
second = SingletonRegistry.instance()
SingletonRegistry.increment()

IO.puts("same=#{if first == second, do: "true", else: "false"}")
IO.puts("count=#{SingletonRegistry.count()}")
