const std = @import("std");

const Format = enum { text, html };

const ReportBuilder = struct {
    format: Format,
    title: []const u8 = "",
    heading: []const u8 = "",
    body: []const u8 = "",

    fn reset(self: *ReportBuilder) void {
        self.title = "";
        self.heading = "";
        self.body = "";
    }

    fn addTitle(self: *ReportBuilder, title: []const u8) void { self.title = title; }
    fn addSection(self: *ReportBuilder, heading: []const u8, body: []const u8) void {
        self.heading = heading;
        self.body = body;
    }

    fn write(self: ReportBuilder, writer: anytype) !void {
        switch (self.format) {
            .text => try writer.print("# {s}\n## {s}\n{s}\n", .{ self.title, self.heading, self.body }),
            .html => try writer.print("<h1>{s}</h1><h2>{s}</h2><p>{s}</p>\n", .{ self.title, self.heading, self.body }),
        }
    }
};

fn buildAvailabilityReport(builder: *ReportBuilder, writer: anytype) !void {
    builder.reset();
    builder.addTitle("Service status");
    builder.addSection("Availability", "99.95%");
    try builder.write(writer);
}

pub fn main() !void {
    const out = std.io.getStdOut().writer();
    var text = ReportBuilder{ .format = .text };
    var html = ReportBuilder{ .format = .html };
    try buildAvailabilityReport(&text, out);
    try out.writeAll("---\n");
    try buildAvailabilityReport(&html, out);
}
