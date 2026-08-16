local function make_factory(create_button, create_checkbox)
    return {
        create_button = create_button,
        create_checkbox = create_checkbox,
    }
end

local dark_factory = make_factory(
    function() return "Dark Button" end,
    function() return "Dark Checkbox" end
)

local light_factory = make_factory(
    function() return "Light Button" end,
    function() return "Light Checkbox" end
)

local function render_ui(factory)
    print(factory.create_button())
    print(factory.create_checkbox())
end

render_ui(dark_factory)
render_ui(light_factory)
