const std = @import("std");

const TokenKind = enum { number, plus };

const Token = struct {
    kind: TokenKind,
    value: i32 = 0,
};

fn interpret(tokens: []const Token) !i32 {
    if (tokens.len == 0 or tokens[0].kind != .number) return error.InvalidExpression;

    var result = tokens[0].value;
    var index: usize = 1;
    while (index < tokens.len) : (index += 2) {
        if (index + 1 >= tokens.len or tokens[index].kind != .plus or tokens[index + 1].kind != .number) {
            return error.InvalidExpression;
        }
        result += tokens[index + 1].value;
    }
    return result;
}

pub fn main() !void {
    const tokens = [_]Token{
        .{ .kind = .number, .value = 2 },
        .{ .kind = .plus },
        .{ .kind = .number, .value = 3 },
        .{ .kind = .plus },
        .{ .kind = .number, .value = 4 },
    };
    const result = try interpret(&tokens);
    if (result != 9) return error.UnexpectedInterpretation;
    std.debug.print("interpreter={d}\n", .{result});
}
