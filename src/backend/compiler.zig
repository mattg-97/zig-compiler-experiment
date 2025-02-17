const Compiler = @This();

const std = @import("std");

const Token = @import("../frontend/tokens.zig");

const Local = struct {
    token: Token,
    depth: usize,
};

alloc: std.mem.Allocator,
locals: std.ArrayList(Local),
scopeDepth: usize,

pub fn init(alloc: std.mem.Allocator) !*Compiler {
    const compiler = try alloc.create(Compiler);
    compiler.* = .{
        .alloc = alloc,
        .locals = std.ArrayList(Local).init(alloc),
        .scopeDepth = 0,
    };
    return compiler;
}
