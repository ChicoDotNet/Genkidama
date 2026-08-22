use strict;
use warnings;

sub plain {
    return 'alert';
}

sub audit {
    my ($inner) = @_;
    return sub { return 'audit(' . $inner->() . ')'; };
}

sub encrypt {
    my ($inner) = @_;
    return sub { return 'enc(' . $inner->() . ')'; };
}

my $component = \&plain;
my $audited = audit($component);
my $encrypted = encrypt($component);
my $stacked = audit(encrypt($component));

print 'base=' . $component->() . "\n";
print 'audit=' . $audited->() . "\n";
print 'encrypted=' . $encrypted->() . "\n";
print 'stacked=' . $stacked->() . "\n";
