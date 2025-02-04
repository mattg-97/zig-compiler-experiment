const ByteCode = @This();

const std = @import("std");

const Chunk = @import("utils/chunk.zig");
const AST = @import("../frontend/parser/ast.zig");
const Program = AST.Program;

alloc: std.mem.Allocator,
chunk: *Chunk,
program: *Program,
globals: std.AutoHashMap(usize, []const u8),
strings: std.AutoHashMap(usize, []const u8),

pub fn init(alloc: std.mem.Allocator, program: *Program) *ByteCode {
    const bc = try alloc.create(ByteCode);
    bc.* = .{
        .alloc = alloc,
        .chunk = undefined,
        .globals = std.AutoHashMap(usize, []const u8).init(alloc),
        .strings = std.AutoHashMap(usize, []const u8).init(alloc),
        .program = program,
    };
    return bc;
}

fn generateIntegerExpression(_: *ByteCode, _: AST.Expression) void {}

fn generateExpression(self: *ByteCode, expr: AST.Expression) void {
    switch (expr) {
        .Integer => |e| self.generateIntegerExpression(e),
        else => std.debug.print("Not done yet\n", .{}),
    }
}

fn generateStatement(self: *ByteCode, stmt: AST.Statement) void {
    switch (stmt) {
        .Expr => |s| self.generateExpression(s.expression),
        else => std.debug.print("Not done yet\n", .{}),
    }
}

pub fn generate(self: *ByteCode) void {
    for (self.program.statements.items) |stmt| {
        self.generateStatement(stmt);
    }
}
