const std = @import("std");

const Database = struct {
    connect: fn () void,
    query: fn () void,
};

fn connectPostgres() void {
    std.debug.print("Connecting to PostgreSQL\n", .{});
}

fn queryPostgres() void {
    std.debug.print("Querying PostgreSQL\n", .{});
}

fn connectMySQL() void {
    std.debug.print("Connecting to MySQL\n", .{});
}

fn queryMySQL() void {
    std.debug.print("Querying MySQL\n", .{});
}

const PostgresFactory = struct {
    pub fn createDatabase() Database {
        return Database{
            .connect = connectPostgres,
            .query = queryPostgres,
        };
    }
};

const MySQLFactory = struct {
    pub fn createDatabase() Database {
        return Database{
            .connect = connectMySQL,
            .query = queryMySQL,
        };
    }
};

fn useDatabase(factory: anytype) void {
    const db = factory.createDatabase();
    db.connect();
    db.query();
}

pub fn main() void {
    useDatabase(PostgresFactory);
    useDatabase(MySQLFactory);
}
