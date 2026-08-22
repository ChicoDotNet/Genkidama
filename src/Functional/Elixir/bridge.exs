defmodule BridgeExample do
  def make_device(name) do
    %{
      power_on: fn -> "#{name}:on" end,
      mute: fn -> "#{name}:muted" end
    }
  end

  def activate_basic(device), do: device.power_on.()
  def activate_mute(device), do: device.mute.()
end

tv = BridgeExample.make_device("TV")
radio = BridgeExample.make_device("Radio")

IO.puts("basic-tv=#{BridgeExample.activate_basic(tv)}")
IO.puts("basic-radio=#{BridgeExample.activate_basic(radio)}")
IO.puts("mute-tv=#{BridgeExample.activate_mute(tv)}")
IO.puts("mute-radio=#{BridgeExample.activate_mute(radio)}")
