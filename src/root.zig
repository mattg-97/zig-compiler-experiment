const std = @import("std");
const testing = std.testing;
//pub const lexer = @import("frontend/lexer.zig");
//pub const parser = @import("frontend/parser/parser.zig");
//pub const Evaluator = @import("backend/evaluator/evaluator.zig");
pub const Code = @import("./backend/compiler/code.zig");
pub const Compiler = @import("./backend/compiler/compiler.zig");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "test suite" {
    std.testing.refAllDecls(@This());
}
