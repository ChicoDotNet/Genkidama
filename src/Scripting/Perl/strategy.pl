use strict;
use warnings;

sub apply_pricing {
    my ($amount, $strategy) = @_;
    die "strategy must be a coderef\n" unless ref($strategy) eq 'CODE';
    return $strategy->($amount);
}

sub regular {
    my ($amount) = @_;
    return $amount;
}

sub vip {
    my ($amount) = @_;
    return int($amount * 80 / 100);
}

sub campaign {
    my ($amount) = @_;
    return $amount >= 100 ? $amount - 25 : $amount;
}

sub assert_equal {
    my ($actual, $expected, $label) = @_;
    die "$label: expected $expected, got $actual\n" unless $actual == $expected;
}

assert_equal(apply_pricing(100, \&regular), 100, 'regular strategy');
assert_equal(apply_pricing(100, \&vip), 80, 'vip strategy');
assert_equal(apply_pricing(100, \&campaign), 75, 'campaign threshold');
assert_equal(apply_pricing(80, \&campaign), 80, 'campaign below threshold');

my $invalid_strategy_rejected = eval { apply_pricing(100, 'vip'); 1 } ? 0 : 1;
die "non-coderef strategy must fail\n" unless $invalid_strategy_rejected;

print "Perl Strategy: passed\n";
