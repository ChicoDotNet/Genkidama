use strict;
use warnings;

package LegacyFahrenheitSensor;
sub new { bless {}, shift }
sub read_fahrenheit { return 86 }

package FahrenheitSensorAdapter;
sub new {
    my ($class, $legacy) = @_;
    return bless { legacy => $legacy }, $class;
}
sub read_celsius {
    my ($self) = @_;
    my $fahrenheit = $self->{legacy}->read_fahrenheit();
    return int((($fahrenheit - 32) * 5 / 9) + 0.5);
}

package main;
my $legacy = LegacyFahrenheitSensor->new();
my $reader = FahrenheitSensorAdapter->new($legacy);
print 'legacy=' . $legacy->read_fahrenheit() . "F\n";
print 'adapted=' . $reader->read_celsius() . "C\n";
