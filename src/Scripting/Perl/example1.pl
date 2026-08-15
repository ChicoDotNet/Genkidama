#!/usr/bin/perl
use strict;
use warnings;

# Concrete products
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

# Concrete factories. Each value represents one coherent product family.
sub dark_factory {
    return {
        create_button   => \&dark_button,
        create_checkbox => \&dark_checkbox,
    };
}

sub light_factory {
    return {
        create_button   => \&light_button,
        create_checkbox => \&light_checkbox,
    };
}

sub create_ui_components {
    my ($factory) = @_;
    $factory->{create_button}->();
    $factory->{create_checkbox}->();
}

# Usage: select the family once, then obtain every related product from it.
create_ui_components(dark_factory());
create_ui_components(light_factory());
