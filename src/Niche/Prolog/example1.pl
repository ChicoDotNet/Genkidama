% Abstract Factory
create_button(dark, dark_button).
create_button(light, light_button).

create_checkbox(dark, dark_checkbox).
create_checkbox(light, light_checkbox).

% Concrete Products
dark_button :- write('Dark Button'), nl.
light_button :- write('Light Button'), nl.

dark_checkbox :- write('Dark Checkbox'), nl.
light_checkbox :- write('Light Checkbox'), nl.

% Usage
run :-
    create_button(dark, Button1),
    call(Button1),
    create_checkbox(light, Checkbox1),
    call(Checkbox1).
