use strict;
use warnings;

use constant LOCKED => 'locked';
use constant UNLOCKED => 'unlocked';

sub transition {
    my ($state, $event) = @_;

    die "unknown state: $state\n"
        unless $state eq LOCKED || $state eq UNLOCKED;

    return UNLOCKED if $state eq LOCKED && $event eq 'coin';
    return LOCKED   if $state eq UNLOCKED && $event eq 'push';
    return $state   if $event eq 'coin' || $event eq 'push';

    die "unknown event: $event\n";
}

sub assert_state {
    my ($actual, $expected, $label) = @_;
    die "$label: expected $expected, got $actual\n" unless $actual eq $expected;
}

my $state = LOCKED;
assert_state($state, LOCKED, 'initial state');

$state = transition($state, 'push');
assert_state($state, LOCKED, 'push while locked is a no-op');

$state = transition($state, 'coin');
assert_state($state, UNLOCKED, 'coin unlocks');

$state = transition($state, 'coin');
assert_state($state, UNLOCKED, 'duplicate coin is a no-op');

$state = transition($state, 'push');
assert_state($state, LOCKED, 'push locks again');

my $invalid_state_rejected = eval { transition('jammed', 'coin'); 1 } ? 0 : 1;
die "unknown state must fail\n" unless $invalid_state_rejected;

print "perl-state: passed\n";
