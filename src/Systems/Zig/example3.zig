const std = @import("std");

const Report = struct {
    generate: fn () void,
};

fn generatePDFReport() void {
    std.debug.print("Generating PDF report\n", .{});
}

fn generateHTMLReport() void {
    std.debug.print("Generating HTML report\n", .{});
}

const PDFReportFactory = struct {
    pub fn createReport() Report {
        return Report{
            .generate = generatePDFReport,
        };
    }
};

const HTMLReportFactory = struct {
    pub fn createReport() Report {
        return Report{
            .generate = generateHTMLReport,
        };
    }
};

fn useFactory(factory: anytype) void {
    const report = factory.createReport();
    report.generate();
}

pub fn main() void {
    useFactory(PDFReportFactory);
    useFactory(HTMLReportFactory);
}
