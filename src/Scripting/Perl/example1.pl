#!/usr/bin/perl
use strict;
use warnings;

# Abstract Factory
sub create_button {
    my $theme = shift;
    return $theme eq 'dark' ? \&dark_button : \&light_button;
}

sub create_checkbox {
    my $theme = shift;
    return $theme eq 'dark' ? \&dark_checkbox : \&light_checkbox;
}

# Concrete Products
sub dark_button {
    print "Dark Button\n";
}

sub light_button {
    print "Light Button\n";
}

sub dark_checkbox {
    print "Dark Checkbox\n";
}

sub light_checkbox {
    print "Light Checkbox\n";
}

# Usage
my $button = create_button('dark');
$button->();

my $checkbox = create_checkbox('light');
$checkbox->();
