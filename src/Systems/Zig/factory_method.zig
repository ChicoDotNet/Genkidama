const std = @import("std");

const DatabaseKind = enum { postgres, mysql };
const FactoryMethod = *const fn () DatabaseKind;

fn createPostgres() DatabaseKind {
    return .postgres;
}

fn createMySql() DatabaseKind {
    return .mysql;
}

fn useDatabase(createDatabase: FactoryMethod) void {
    switch (createDatabase()) {
        .postgres => {
            std.debug.print("PostgreSQL connect\n", .{});
            std.debug.print("PostgreSQL query\n", .{});
        },
        .mysql => {
            std.debug.print("MySQL connect\n", .{});
            std.debug.print("MySQL query\n", .{});
        },
    }
}

pub fn main() void {
    useDatabase(createPostgres);
    useDatabase(createMySql);
}
