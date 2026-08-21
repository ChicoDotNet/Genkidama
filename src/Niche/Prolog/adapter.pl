legacy_read_fahrenheit(86).

read_celsius(Celsius) :-
    legacy_read_fahrenheit(Fahrenheit),
    Celsius is ((Fahrenheit - 32) * 5) // 9.

run :-
    legacy_read_fahrenheit(Fahrenheit),
    read_celsius(Celsius),
    format('legacy=~dF~n', [Fahrenheit]),
    format('adapted=~dC~n', [Celsius]).
