#!/usr/bin/env perl
use strict;
use warnings;

sub save_memento {
    my ($document) = @_;
    return {
        title => $document->{title},
        tags  => [ @{ $document->{tags} } ],
    };
}

sub restore_memento {
    my ($document, $snapshot) = @_;
    $document->{title} = $snapshot->{title};
    $document->{tags}  = [ @{ $snapshot->{tags} } ];
    return $document;
}

sub verify_memento_canonical {
    my $document = {
        title => 'draft',
        tags  => ['pattern'],
    };
    my $snapshot = save_memento($document);

    $document->{title} = 'published';
    push @{ $document->{tags} }, 'edited';

    die "snapshot changed after originator mutation\n"
        unless $snapshot->{title} eq 'draft'
        && join(',', @{ $snapshot->{tags} }) eq 'pattern';

    restore_memento($document, $snapshot);
    die "restore did not recover snapshot state\n"
        unless $document->{title} eq 'draft'
        && join(',', @{ $document->{tags} }) eq 'pattern';

    $document->{tags}[0] = 'restored';
    die "restored state aliases caretaker snapshot\n"
        unless $snapshot->{tags}[0] eq 'pattern';

    return 1;
}

verify_memento_canonical();
print "Perl Memento: passed\n";
