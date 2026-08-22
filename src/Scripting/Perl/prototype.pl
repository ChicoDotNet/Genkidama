use strict;
use warnings;

sub new_profile {
    my ($name, @features) = @_;
    return {
        name => $name,
        features => [@features],
    };
}

sub clone_profile {
    my ($source) = @_;
    return {
        name => $source->{name},
        features => [@{$source->{features}}],
    };
}

sub describe {
    my ($profile) = @_;
    return $profile->{name} . ': ' . join(',', @{$profile->{features}});
}

my $original = new_profile('orders', 'metrics');
my $canary = clone_profile($original);
$canary->{name} = 'orders-canary';
push @{$canary->{features}}, 'tracing';

print 'original=' . describe($original) . "\n";
print 'clone=' . describe($canary) . "\n";
