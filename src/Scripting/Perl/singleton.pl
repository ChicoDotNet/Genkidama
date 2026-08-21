use strict;
use warnings;
use Scalar::Util qw(refaddr);

{
    package Registry;
    my $instance;

    sub instance {
        $instance //= bless { count => 0 }, __PACKAGE__;
        return $instance;
    }

    sub increment {
        my ($self) = @_;
        $self->{count}++;
    }

    sub count {
        my ($self) = @_;
        return $self->{count};
    }
}

my $first = Registry::instance();
my $second = Registry::instance();
$first->increment();

print 'same=', refaddr($first) == refaddr($second) ? 'true' : 'false', "\n";
print 'count=', $second->count(), "\n";
