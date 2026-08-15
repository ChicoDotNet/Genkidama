alias UIFactory = NamedTuple(
  button: Proc(String),
  checkbox: Proc(String))

DARK_FACTORY = {
  button:   -> { "Dark Button" },
  checkbox: -> { "Dark Checkbox" },
}

LIGHT_FACTORY = {
  button:   -> { "Light Button" },
  checkbox: -> { "Light Checkbox" },
}

def create_ui_components(factory : UIFactory)
  puts factory[:button].call
  puts factory[:checkbox].call
end

create_ui_components(DARK_FACTORY)
create_ui_components(LIGHT_FACTORY)
