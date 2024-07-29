% Abstract Factory
create_report(pdf, pdf_report).
create_report(html, html_report).

% Concrete Products
generate(pdf_report) :- write('Generating PDF report'), nl.
generate(html_report) :- write('Generating HTML report'), nl.

% Usage
run :-
    create_report(pdf, PDFReport),
    generate(PDFReport),
    create_report(html, HTMLReport),
    generate(HTMLReport).
