#!/usr/bin/perl
use strict;
use warnings;

# Abstract Factory
sub connect_db {
    my $db_type = shift;
    if ($db_type eq 'postgresql') {
        connect_postgresql();
    } elsif ($db_type eq 'mysql') {
        connect_mysql();
    }
}

sub query_db {
    my $db_type = shift;
    if ($db_type eq 'postgresql') {
        query_postgresql();
    } elsif ($db_type eq 'mysql') {
        query_mysql();
    }
}

# Concrete Implementations
sub connect_postgresql {
    print "Connecting to PostgreSQL\n";
}

sub query_postgresql {
    print "Querying PostgreSQL\n";
}

sub connect_mysql {
    print "Connecting to MySQL\n";
}

sub query_mysql {
    print "Querying MySQL\n";
}

# Usage
connect_db('postgresql');
query_db('postgresql');
connect_db('mysql');
query_db('mysql');
