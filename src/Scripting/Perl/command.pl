use strict;
use warnings;

my $balance = 100;
my @queue = (
    { op => 'deposit', amount => 50 },
    { op => 'withdraw', amount => 20 },
);

sub execute_command {
    my ($command) = @_;
    if ($command->{op} eq 'deposit') {
        $balance += $command->{amount};
    } elsif ($command->{op} eq 'withdraw') {
        $balance -= $command->{amount};
    } else {
        die "unknown command\n";
    }
}

execute_command($_) for @queue;
die "unexpected balance\n" unless $balance == 130;
print "balance=$balance;commands=" . scalar(@queue) . "\n";
