use strict;
use warnings;

package RemoteDocumentStore;
sub new { bless { fetch_count => 0 }, shift }
sub get {
    my ($self, $id) = @_;
    $self->{fetch_count}++;
    return "doc($id)";
}

package DocumentStoreProxy;
sub new { bless { backend => undef, cache => {} }, shift }
sub get {
    my ($self, $id) = @_;
    return $self->{cache}{$id} if exists $self->{cache}{$id};
    $self->{backend} //= RemoteDocumentStore->new();
    my $value = $self->{backend}->get($id);
    $self->{cache}{$id} = $value;
    return $value;
}
sub backend_count { defined $_[0]{backend} ? 1 : 0 }
sub fetch_count { defined $_[0]{backend} ? $_[0]{backend}{fetch_count} : 0 }

package main;
my $store = DocumentStoreProxy->new();
my $first = $store->get(42);
my $second = $store->get(42);
print 'backend=', $store->backend_count(), ';fetches=', $store->fetch_count(), ";first=$first;second=$second\n";
