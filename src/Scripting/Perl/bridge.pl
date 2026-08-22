use strict;
use warnings;

sub device {
    my ($name) = @_;
    return {
        turn_on => sub { return "$name:on" },
        mute    => sub { return "$name:muted" },
    };
}

sub basic_remote {
    my ($device) = @_;
    return $device->{turn_on}->();
}

sub mute_remote {
    my ($device) = @_;
    return $device->{mute}->();
}

my $tv = device('TV');
my $radio = device('Radio');
print 'basic-tv=' . basic_remote($tv) . "\n";
print 'basic-radio=' . basic_remote($radio) . "\n";
print 'mute-tv=' . mute_remote($tv) . "\n";
print 'mute-radio=' . mute_remote($radio) . "\n";
