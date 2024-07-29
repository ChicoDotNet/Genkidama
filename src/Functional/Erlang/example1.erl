-module(example1).
-export([create_button/1, create_checkbox/1]).

% Abstract Factory
create_button(dark) -> dark_button;
create_button(light) -> light_button.

create_checkbox(dark) -> dark_checkbox;
create_checkbox(light) -> light_checkbox.

% Concrete Products
dark_button() -> io:format("Dark Button~n").
light_button() -> io:format("Light Button~n").

dark_checkbox() -> io:format("Dark Checkbox~n").
light_checkbox() -> io:format("Light Checkbox~n").
