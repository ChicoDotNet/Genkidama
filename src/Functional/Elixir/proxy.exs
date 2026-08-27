defmodule DocumentProxy do
  def start do
    Agent.start_link(fn -> %{created: 0, fetches: 0, cache: %{}} end)
  end

  def get(agent, id) do
    Agent.get_and_update(agent, fn state ->
      case Map.fetch(state.cache, id) do
        {:ok, value} ->
          {value, state}

        :error ->
          created = if state.created == 0, do: 1, else: state.created
          value = "doc(#{id})"
          next = %{
            state
            | created: created,
              fetches: state.fetches + 1,
              cache: Map.put(state.cache, id, value)
          }

          {value, next}
      end
    end)
  end

  def stats(agent), do: Agent.get(agent, &Map.take(&1, [:created, :fetches]))
end

{:ok, proxy} = DocumentProxy.start()
first = DocumentProxy.get(proxy, 42)
second = DocumentProxy.get(proxy, 42)
%{created: created, fetches: fetches} = DocumentProxy.stats(proxy)
IO.puts("backend=#{created};fetches=#{fetches};first=#{first};second=#{second}")
