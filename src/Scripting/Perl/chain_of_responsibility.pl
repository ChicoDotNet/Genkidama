use strict;
use warnings;

my @visited;
my @handlers = (
    { name => 'faq',        limit => 50 },
    { name => 'billing',    limit => 500 },
    { name => 'escalation', limit => undef },
);

sub handle_refund {
    my ($amount) = @_;
    for my $handler (@handlers) {
        push @visited, $handler->{name};
        if (!defined $handler->{limit} || $amount <= $handler->{limit}) {
            return ($handler->{name}, "refund($amount)");
        }
    }
    die "unhandled request\n";
}

my ($handled, $result) = handle_refund(250);
print 'visited=' . join('>', @visited) . ";handled=$handled;result=$result\n";
