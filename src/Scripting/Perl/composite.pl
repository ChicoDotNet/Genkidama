use strict;
use warnings;

package FileLeaf;
sub new {
    my ($class, $bytes) = @_;
    return bless { bytes => $bytes }, $class;
}
sub size { return $_[0]->{bytes}; }

package FolderComposite;
sub new {
    my ($class, @children) = @_;
    return bless { children => \@children }, $class;
}
sub size {
    my ($self) = @_;
    my $total = 0;
    $total += $_->size() for @{$self->{children}};
    return $total;
}

package main;
my $readme = FileLeaf->new(2);
my $docs = FolderComposite->new(FileLeaf->new(3), FileLeaf->new(5));
my $root = FolderComposite->new($readme, $docs);

print "leaf=" . $readme->size() . "\n";
print "docs=" . $docs->size() . "\n";
print "root=" . $root->size() . "\n";
