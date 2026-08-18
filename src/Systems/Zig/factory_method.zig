const std = @import("std");

const DatabaseAction = *const fn () void;

const Database = struct {
    connect: DatabaseAction,
    query: DatabaseAction,
};

const FactoryMethod = *const fn () Database;

fn postgresConnect() void {
    std.debug.print("PostgreSQL connect\n", .{});
}

fn postgresQuery() void {
    std.debug.print("PostgreSQL query\n", .{});
}

fn mysqlConnect() void {
    std.debug.print("MySQL connect\n", .{});
}

fn mysqlQuery() void {
    std.debug.print("MySQL query\n", .{});
}

fn createPostgres() Database {
    return .{ .connect = postgresConnect, .query = postgresQuery };
}

fn createMySql() Database {
    return .{ .connect = mysqlConnect, .query = mysqlQuery };
}

fn useDatabase(createDatabase: FactoryMethod) void {
    const database = createDatabase();
    database.connect();
    database.query();
}

pub fn main() void {
    useDatabase(createPostgres);
    useDatabase(createMySql);
}
