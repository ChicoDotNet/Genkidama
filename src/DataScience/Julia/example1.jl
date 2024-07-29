abstract type Button end
abstract type Checkbox end

struct DarkButton <: Button end
struct LightButton <: Button end

struct DarkCheckbox <: Checkbox end
struct LightCheckbox <: Checkbox end

render(b::DarkButton) = println("Dark Button")
render(b::LightButton) = println("Light Button")
render(c::DarkCheckbox) = println("Dark Checkbox")
render(c::LightCheckbox) = println("Light Checkbox")

abstract type UIFactory end

struct DarkFactory <: UIFactory end
struct LightFactory <: UIFactory end

create_button(::DarkFactory) = DarkButton()
create_button(::LightFactory) = LightButton()

create_checkbox(::DarkFactory) = DarkCheckbox()
create_checkbox(::LightFactory) = LightCheckbox()

function create_ui_components(factory::UIFactory)
    button = create_button(factory)
    checkbox = create_checkbox(factory)
    render(button)
    render(checkbox)
end

# Usage
create_ui_components(DarkFactory())
create_ui_components(LightFactory())
