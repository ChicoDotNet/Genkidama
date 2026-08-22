const std = @import("std");

const Node = union(enum) {
    file: i32,
    folder: []const Node,

    fn size(self: Node) i32 {
        return switch (self) {
            .file => |bytes| bytes,
            .folder => |children| blk: {
                var total: i32 = 0;
                for (children) |child| {
                    total += child.size();
                }
                break :blk total;
            },
        };
    }
};

pub fn main() void {
    const readme = Node{ .file = 2 };
    const docs_children = [_]Node{ Node{ .file = 3 }, Node{ .file = 5 } };
    const docs = Node{ .folder = &docs_children };
    const root_children = [_]Node{ readme, docs };
    const root = Node{ .folder = &root_children };

    std.debug.print("leaf={d}\n", .{readme.size()});
    std.debug.print("docs={d}\n", .{docs.size()});
    std.debug.print("root={d}\n", .{root.size()});
}
