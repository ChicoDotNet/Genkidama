const std = @import("std");

pub const DocumentState = enum {
    draft,
    published,
};

pub const MementoSnapshot = struct {
    state: DocumentState,
};

pub const Document = struct {
    state: DocumentState,

    pub fn save(self: Document) MementoSnapshot {
        return .{ .state = self.state };
    }

    pub fn restore(self: *Document, snapshot: MementoSnapshot) void {
        self.state = snapshot.state;
    }
};

pub fn verifyMementoCanonical() bool {
    var document = Document{ .state = .draft };
    const snapshot = document.save();

    document.state = .published;
    if (document.state != .published) return false;
    if (snapshot.state != .draft) return false;

    document.restore(snapshot);
    if (document.state != .draft) return false;

    document.state = .published;
    return snapshot.state == .draft;
}

pub fn main() void {
    std.debug.assert(verifyMementoCanonical());
    std.debug.print("Zig Memento: passed\n", .{});
}
