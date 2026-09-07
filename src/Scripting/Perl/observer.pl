#!/usr/bin/env perl
use strict;
use warnings;

my %subscribers;

sub subscribe {
    my ($name, $handler) = @_;
    return 0 if exists $subscribers{$name};
    $subscribers{$name} = $handler;
    return 1;
}

sub unsubscribe {
    my ($name) = @_;
    return 0 unless exists $subscribers{$name};
    delete $subscribers{$name};
    return 1;
}

sub publish {
    my ($event) = @_;
    $_->($event) for values %subscribers;
}

my (@audit_log, @dashboard_log);
my $audit = sub { push @audit_log, shift };
my $dashboard = sub { push @dashboard_log, shift };

sub example_passes {
    %subscribers = ();
    @audit_log = ();
    @dashboard_log = ();

    return 0 unless subscribe('audit', $audit);
    return 0 unless subscribe('dashboard', $dashboard);
    return 0 if subscribe('audit', $audit);

    publish('created');
    return 0 unless join(',', @audit_log) eq 'created';
    return 0 unless join(',', @dashboard_log) eq 'created';

    return 0 unless unsubscribe('dashboard');
    return 0 if unsubscribe('dashboard');

    publish('approved');
    return 0 unless join(',', @audit_log) eq 'created,approved';
    return 0 unless join(',', @dashboard_log) eq 'created';

    return 1;
}

if (example_passes()) {
    print "OBSERVER_PERL_OK\n";
    exit 0;
}

die "Observer contract failed\n";
