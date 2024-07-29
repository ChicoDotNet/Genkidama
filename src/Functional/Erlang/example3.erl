-module(example3).
-export([generate_report/1]).

% Abstract Factory
generate_report(pdf) -> generate_pdf_report();
generate_report(html) -> generate_html_report().

% Concrete Implementations
generate_pdf_report() -> io:format("Generating PDF report~n").
generate_html_report() -> io:format("Generating HTML report~n").

% Usage
main() ->
    generate_report(pdf),
    generate_report(html).
