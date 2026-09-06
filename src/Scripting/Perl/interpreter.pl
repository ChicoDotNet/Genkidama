use strict;
use warnings;

sub number {
    my ($value) = @_;
    return { kind => 'number', value => $value };
}

sub add {
    my ($left, $right) = @_;
    return { kind => 'add', left => $left, right => $right };
}

sub interpret {
    my ($expr) = @_;
    return $expr->{value} if $expr->{kind} eq 'number';
    return interpret($expr->{left}) + interpret($expr->{right}) if $expr->{kind} eq 'add';
    die "unknown expression\n";
}

my $expression = add(add(number(2), number(3)), number(4));
my $value = interpret($expression);
die "unexpected value\n" unless $value == 9;
print "interpreter=$value\n";
