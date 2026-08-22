use strict;
use warnings;

sub create_postgres {
    return {
        connect => sub { print "PostgreSQL connect\n" },
        query   => sub { print "PostgreSQL query\n" },
    };
}

sub create_mysql {
    return {
        connect => sub { print "MySQL connect\n" },
        query   => sub { print "MySQL query\n" },
    };
}

sub use_database {
    my ($create_database) = @_;
    my $database = $create_database->();
    $database->{connect}->();
    $database->{query}->();
}

use_database(\&create_postgres);
use_database(\&create_mysql);
