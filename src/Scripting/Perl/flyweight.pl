use strict;
use warnings;
use Scalar::Util qw(refaddr);

my %styles;
sub get_style {
    my ($font, $size, $color) = @_;
    my $key = join '|', $font, $size, $color;
    $styles{$key} //= { font => $font, size => $size, color => $color };
    return $styles{$key};
}

my $red1 = get_style('Inter', 12, 'red');
my $red2 = get_style('Inter', 12, 'red');
get_style('Inter', 12, 'blue');
my $shared = refaddr($red1) == refaddr($red2) ? 'true' : 'false';
print 'styles=' . scalar(keys %styles) . ";shared=$shared;text=ABC\n";
