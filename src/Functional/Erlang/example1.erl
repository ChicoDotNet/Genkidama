-module(example1).
-export([dark_factory/0, light_factory/0, create_ui_components/1, main/0]).

%% A factory is one value that owns the constructors for an entire family.
dark_factory() ->
    #{button => fun dark_button/0, checkbox => fun dark_checkbox/0}.

light_factory() ->
    #{button => fun light_button/0, checkbox => fun light_checkbox/0}.

create_ui_components(Factory) ->
    CreateButton = maps:get(button, Factory),
    CreateCheckbox = maps:get(checkbox, Factory),
    Button = CreateButton(),
    Checkbox = CreateCheckbox(),
    io:format("~s~n~s~n", [Button, Checkbox]).

main() ->
    create_ui_components(dark_factory()),
    create_ui_components(light_factory()).

%% Concrete products stay private; clients receive them through one family value.
dark_button() -> "Dark Button".
light_button() -> "Light Button".
dark_checkbox() -> "Dark Checkbox".
light_checkbox() -> "Light Checkbox".
