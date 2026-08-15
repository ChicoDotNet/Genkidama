function example1()
  dark_factory = make_factory(@() "Dark Button", @() "Dark Checkbox");
  light_factory = make_factory(@() "Light Button", @() "Light Checkbox");

  render_ui(dark_factory);
  render_ui(light_factory);
end

function factory = make_factory(create_button, create_checkbox)
  factory = struct(
    "create_button", create_button,
    "create_checkbox", create_checkbox
  );
end

function render_ui(factory)
  disp(factory.create_button());
  disp(factory.create_checkbox());
end
