use strict;
use warnings;

sub iterator_for {
    my ($items) = @_;
    my $index = 0;
    return sub {
        return undef if $index >= @$items;
        return $items->[$index++];
    };
}

my $next = iterator_for([10, 20, 30]);
my @visited;
while (defined(my $value = $next->())) {
    push @visited, $value;
}
die "iterator contract failed\n" unless join(',', @visited) eq '10,20,30' && !defined($next->());
print "iterator=10,20,30\n";
