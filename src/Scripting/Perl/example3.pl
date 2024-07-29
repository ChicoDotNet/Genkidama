#!/usr/bin/perl
use strict;
use warnings;

# Abstract Factory
sub generate_report {
    my $report_type = shift;
    if ($report_type eq 'pdf') {
        generate_pdf_report();
    } elsif ($report_type eq 'html') {
        generate_html_report();
    }
}

# Concrete Implementations
sub generate_pdf_report {
    print "Generating PDF report\n";
}

sub generate_html_report {
    print "Generating HTML report\n";
}

# Usage
generate_report('pdf');
generate_report('html');
