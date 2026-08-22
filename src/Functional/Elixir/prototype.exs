defmodule Prototype do
  def clone_profile(profile) do
    %{profile | features: Enum.map(profile.features, & &1)}
  end

  def describe(profile) do
    "#{profile.name}: #{Enum.join(profile.features, ",")}"
  end
end

original = %{name: "orders", features: ["metrics"]}
base_clone = Prototype.clone_profile(original)
canary = %{base_clone | name: "orders-canary", features: base_clone.features ++ ["tracing"]}

IO.puts("original=#{Prototype.describe(original)}")
IO.puts("clone=#{Prototype.describe(canary)}")
