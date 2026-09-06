use strict;
use warnings;

my %receivers;

sub register_colleague {
    my ($name, $receiver) = @_;
    die "receiver must be a code reference\n" unless ref($receiver) eq 'CODE';
    $receivers{$name} = $receiver;
}

sub send_message {
    my ($sender, $recipient, $message) = @_;
    my $receiver = $receivers{$recipient};
    die "UnknownColleague:$recipient\n" unless defined $receiver;
    return $receiver->($sender, $message);
}

my @payment_events;
my @inventory_events;

register_colleague('payment', sub {
    my ($sender, $message) = @_;
    push @payment_events, "$sender:$message";
    return 'payment-ack';
});

register_colleague('inventory', sub {
    my ($sender, $message) = @_;
    push @inventory_events, "$sender:$message";
    return 'inventory-ack';
});

my $inventory_ack = send_message('payment', 'inventory', 'paid');
my $payment_ack = send_message('inventory', 'payment', 'reserved');

die "inventory routing failed\n"
    unless $inventory_ack eq 'inventory-ack'
    && join(',', @inventory_events) eq 'payment:paid';

die "payment routing failed\n"
    unless $payment_ack eq 'payment-ack'
    && join(',', @payment_events) eq 'inventory:reserved';

my $unknown_error = '';
eval { send_message('payment', 'shipping', 'dispatch'); 1 } or $unknown_error = $@;
die "missing unknown colleague failure\n"
    unless $unknown_error =~ /UnknownColleague:shipping/;

print "Perl Mediator: passed\n";
