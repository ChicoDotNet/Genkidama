const std = @import("std");

fn writeStdout(bytes: []const u8) !void {
    const written = std.os.linux.syscall3(.write, 1, @intFromPtr(bytes.ptr), bytes.len);
    if (written != bytes.len) return error.StdoutWriteFailed;
}

const RealDocumentStore = struct {
    fetches: usize = 0,

    fn get(self: *RealDocumentStore, id: u32, buffer: []u8) ![]const u8 {
        self.fetches += 1;
        return std.fmt.bufPrint(buffer, "doc({d})", .{id});
    }
};

const DocumentStoreProxy = struct {
    backend: ?RealDocumentStore = null,
    cached_id: ?u32 = null,
    cached_value: [32]u8 = undefined,
    cached_len: usize = 0,

    fn get(self: *DocumentStoreProxy, id: u32) ![]const u8 {
        if (self.cached_id != null and self.cached_id.? == id) {
            return self.cached_value[0..self.cached_len];
        }

        if (self.backend == null) self.backend = RealDocumentStore{};

        var fetched: [32]u8 = undefined;
        const value = try self.backend.?.get(id, &fetched);
        @memcpy(self.cached_value[0..value.len], value);
        self.cached_len = value.len;
        self.cached_id = id;
        return self.cached_value[0..self.cached_len];
    }
};

pub fn main() !void {
    var proxy = DocumentStoreProxy{};

    const first = try proxy.get(42);
    var first_copy: [32]u8 = undefined;
    @memcpy(first_copy[0..first.len], first);
    const first_len = first.len;

    const second = try proxy.get(42);
    const backend_count: usize = if (proxy.backend == null) 0 else 1;
    const fetches = if (proxy.backend) |backend| backend.fetches else 0;

    var output: [128]u8 = undefined;
    const rendered = try std.fmt.bufPrint(&output, "backend={d};fetches={d};first={s};second={s}", .{
        backend_count,
        fetches,
        first_copy[0..first_len],
        second,
    });
    try writeStdout(rendered);
}
