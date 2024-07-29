const std = @import("std");

const Button = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Button\n", .{});
    }
};

const DarkButton = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Dark Button\n", .{});
    }
};

const LightButton = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Light Button\n", .{});
    }
};

const Checkbox = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Checkbox\n", .{});
    }
};

const DarkCheckbox = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Dark Checkbox\n", .{});
    }
};

const LightCheckbox = struct {
    pub fn render(self: @This()) void {
        std.debug.print("Light Checkbox\n", .{});
    }
};

const UIFactory = struct {
    pub fn createButton(self: @This()) Button {
        return Button{};
    }
    pub fn createCheckbox(self: @This()) Checkbox {
        return Checkbox{};
    }
};

const DarkFactory = struct {
    pub fn createButton(self: @This()) DarkButton {
        return DarkButton{};
    }
    pub fn createCheckbox(self: @This()) DarkCheckbox {
        return DarkCheckbox{};
    }
};

const LightFactory = struct {
    pub fn createButton(self: @This()) LightButton {
        return LightButton{};
    }
    pub fn createCheckbox(self: @This()) LightCheckbox {
        return LightCheckbox{};
    }
};

fn createUIComponents(factory: UIFactory) void {
    const button = factory.createButton();
    const checkbox = factory.createCheckbox();
    button.render();
    checkbox.render();
}

pub fn main() void {
    const darkFactory = DarkFactory{};
    const lightFactory = LightFactory{};
    createUIComponents(darkFactory);
    createUIComponents(lightFactory);
}
