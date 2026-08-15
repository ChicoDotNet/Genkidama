# Abstract Factory
#
# The selected factory represents one coherent product family. Consumers receive
# both constructors together, so they cannot accidentally mix Dark and Light
# products while using the Abstract Factory boundary.

defmodule UIFactory do
  @moduledoc "A coherent family of UI-product constructors."

  @type factory :: %{
          create_button: (-> String.t()),
          create_checkbox: (-> String.t())
        }

  @spec dark() :: factory()
  def dark do
    %{
      create_button: fn -> "Dark Button" end,
      create_checkbox: fn -> "Dark Checkbox" end
    }
  end

  @spec light() :: factory()
  def light do
    %{
      create_button: fn -> "Light Button" end,
      create_checkbox: fn -> "Light Checkbox" end
    }
  end
end

defmodule Example1 do
  @moduledoc false

  @spec create_ui_components(UIFactory.factory()) :: {String.t(), String.t()}
  def create_ui_components(factory) do
    button = factory.create_button.()
    checkbox = factory.create_checkbox.()
    {button, checkbox}
  end

  @spec main() :: :ok
  def main do
    factory = UIFactory.dark()
    {button, checkbox} = create_ui_components(factory)

    IO.puts(button)
    IO.puts(checkbox)
  end
end

Example1.main()
