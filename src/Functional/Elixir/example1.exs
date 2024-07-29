defmodule UIFactory do
  def create_button(:dark), do: DarkButton.new()
  def create_button(:light), do: LightButton.new()
  def create_checkbox(:dark), do: DarkCheckbox.new()
  def create_checkbox(:light), do: LightCheckbox.new()
end

defmodule DarkButton do
  def new() do
    IO.puts "Dark Button"
  end
end

defmodule LightButton do
  def new() do
    IO.puts "Light Button"
  end
end

defmodule DarkCheckbox do
  def new() do
    IO.puts "Dark Checkbox"
  end
end

defmodule LightCheckbox do
  def new() do
    IO.puts "Light Checkbox"
  end
end

# Usage
UIFactory.create_button(:dark)
UIFactory.create_checkbox(:light)
