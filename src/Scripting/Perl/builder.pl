use strict;
use warnings;

sub text_builder {
    my @parts;
    return {
        reset => sub { @parts = (); },
        add_title => sub { push @parts, '# ' . $_[0]; },
        add_section => sub { push @parts, '## ' . $_[0], $_[1]; },
        build => sub { join "\n", @parts },
    };
}

sub html_builder {
    my @parts;
    return {
        reset => sub { @parts = (); },
        add_title => sub { push @parts, '<h1>' . $_[0] . '</h1>'; },
        add_section => sub { push @parts, '<h2>' . $_[0] . '</h2>', '<p>' . $_[1] . '</p>'; },
        build => sub { join '', @parts },
    };
}

sub build_availability_report {
    my ($builder) = @_;
    $builder->{reset}->();
    $builder->{add_title}->('Service status');
    $builder->{add_section}->('Availability', '99.95%');
    return $builder->{build}->();
}

print build_availability_report(text_builder()), "\n---\n";
print build_availability_report(html_builder()), "\n";
