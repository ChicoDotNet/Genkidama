const std = @import("std");

const ProductFactory = *const fn () []const u8;

const UIFactory = struct {
    create_button: ProductFactory,
    create_checkbox: ProductFactory,
};

fn darkButton() []const u8 {
    return "Dark Button";
}

fn darkCheckbox() []const u8 {
    return "Dark Checkbox";
}

fn lightButton() []const u8 {
    return "Light Button";
}

fn lightCheckbox() []const u8 {
    return "Light Checkbox";
}

const dark_factory = UIFactory{
    .create_button = darkButton,
    .create_checkbox = darkCheckbox,
};

const light_factory = UIFactory{
    .create_button = lightButton,
    .create_checkbox = lightCheckbox,
};

fn createUIComponents(factory: UIFactory) void {
    std.debug.print("{s}\n{s}\n", .{
        factory.create_button(),
        factory.create_checkbox(),
    });
}

pub fn main() void {
    createUIComponents(dark_factory);
    createUIComponents(light_factory);
}
