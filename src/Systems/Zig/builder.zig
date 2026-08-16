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

    fn addTitle(self: *ReportBuilder, title: []const u8) void {
        self.title = title;
    }

    fn addSection(self: *ReportBuilder, heading: []const u8, body: []const u8) void {
        self.heading = heading;
        self.body = body;
    }

    fn print(self: ReportBuilder) void {
        switch (self.format) {
            .text => std.debug.print("# {s}\n## {s}\n{s}\n", .{ self.title, self.heading, self.body }),
            .html => std.debug.print("<h1>{s}</h1><h2>{s}</h2><p>{s}</p>\n", .{ self.title, self.heading, self.body }),
        }
    }
};

fn buildAvailabilityReport(builder: *ReportBuilder) void {
    builder.reset();
    builder.addTitle("Service status");
    builder.addSection("Availability", "99.95%");
    builder.print();
}

pub fn main() void {
    var text = ReportBuilder{ .format = .text };
    var html = ReportBuilder{ .format = .html };
    buildAvailabilityReport(&text);
    std.debug.print("---\n", .{});
    buildAvailabilityReport(&html);
}
