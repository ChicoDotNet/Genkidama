% Abstract Factory
%
% A factory term represents one coherent product family. The consumer selects
% the family once and receives both products from that same factory.

ui_factory(dark, factory(dark_button, dark_checkbox)).
ui_factory(light, factory(light_button, light_checkbox)).

create_ui_components(factory(Button, Checkbox)) :-
    call(Button),
    call(Checkbox).

% Concrete Products
dark_button :- write('Dark Button'), nl.
light_button :- write('Light Button'), nl.

dark_checkbox :- write('Dark Checkbox'), nl.
light_checkbox :- write('Light Checkbox'), nl.

% Usage
run :-
    ui_factory(dark, Factory),
    create_ui_components(Factory).
