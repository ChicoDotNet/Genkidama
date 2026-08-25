defmodule StylePool do
  def get(pool, key, style) do
    case Map.fetch(pool, key) do
      {:ok, existing} -> {existing, pool}
      :error -> {style, Map.put(pool, key, style)}
    end
  end
end

pool = %{}
{red1, pool} = StylePool.get(pool, {"Inter", 12, "red"}, %{font: "Inter", size: 12, color: "red"})
{red2, pool} = StylePool.get(pool, {"Inter", 12, "red"}, %{font: "Inter", size: 12, color: "red"})
{blue, pool} = StylePool.get(pool, {"Inter", 12, "blue"}, %{font: "Inter", size: 12, color: "blue"})
true = blue.color == "blue"
shared = red1 === red2
IO.puts("styles=#{map_size(pool)};shared=#{shared};text=ABC")
