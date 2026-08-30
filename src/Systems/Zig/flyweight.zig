const std = @import("std");

const Style = struct {
    font: []const u8,
    size: u8,
    color: []const u8,
};

const Factory = struct {
    styles: [2]Style = undefined,
    used: usize = 0,

    fn get(self: *Factory, font: []const u8, size: u8, color: []const u8) *const Style {
        for (self.styles[0..self.used]) |*style| {
            if (style.size == size and std.mem.eql(u8, style.font, font) and std.mem.eql(u8, style.color, color)) {
                return style;
            }
        }
        if (self.used == self.styles.len) @panic("style pool exhausted");
        self.styles[self.used] = .{ .font = font, .size = size, .color = color };
        self.used += 1;
        return &self.styles[self.used - 1];
    }
};

pub fn main(init: std.process.Init) !void {
    var factory = Factory{};
    const red1 = factory.get("Inter", 12, "red");
    const red2 = factory.get("Inter", 12, "red");
    const blue = factory.get("Inter", 12, "blue");
    if (!std.mem.eql(u8, blue.color, "blue")) return error.BlueStyleMissing;

    var buffer: [64]u8 = undefined;
    const output = try std.fmt.bufPrint(&buffer, "styles={d};shared={s};text=ABC", .{
        factory.used,
        if (red1 == red2) "true" else "false",
    });
    try std.Io.File.stdout().writeStreamingAll(init.io, output);
}
