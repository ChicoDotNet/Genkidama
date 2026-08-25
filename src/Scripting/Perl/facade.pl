use strict;
use warnings;

sub authenticate {
    my ($user) = @_;
    return "auth($user)";
}

sub reserve {
    my ($sku) = @_;
    return "reserve($sku)";
}

sub charge {
    my ($cents) = @_;
    return "charge($cents)";
}

sub checkout {
    my ($user, $sku, $cents) = @_;
    return 'checkout=' . authenticate($user) . '>' . reserve($sku) . '>' . charge($cents);
}

print checkout('alice', 'SKU-42', 499), "\n";
