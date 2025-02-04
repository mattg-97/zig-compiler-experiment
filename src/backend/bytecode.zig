const ByteCode = @This();

const std = @import("std");

const Value = @import("utils/value.zig");
const Chunk = @import("utils/chunk.zig");
const OpCode = Chunk.OpCode;
const AST = @import("../frontend/parser/ast.zig");
const Program = AST.Program;

alloc: std.mem.Allocator,
chunk: *Chunk,
program: *Program,
globals: std.AutoHashMap(usize, []const u8),
strings: std.AutoHashMap(usize, []const u8),

pub fn init(alloc: std.mem.Allocator, chunk: *Chunk) *ByteCode {
    const bc = try alloc.create(ByteCode);
    bc.* = .{
        .alloc = alloc,
        .chunk = chunk,
        .globals = std.AutoHashMap(usize, []const u8).init(alloc),
        .strings = std.AutoHashMap(usize, []const u8).init(alloc),
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

pub fn generate(self: *ByteCode, program: *AST.Program) void {
    for (program.statements.items) |stmt| {
        self.generateStatement(stmt);
    }
}

fn emitByte(self: *ByteCode, byte: u8, line: usize) !void {
    try self.chunk.writeChunk(byte, line);
}

fn emitBytes(self: *ByteCode, byte1: u8, byte2: u8, line: usize) !void {
    try self.chunk.writeChunk(byte1, line);
    try self.chunk.writeChunk(byte2, line);
}
fn createConstant(self: *ByteCode, value: Value) !usize {
    return try self.chunk.addConstant(value);
}

fn emitConstant(self: *ByteCode, value: Value, line: usize) !void {
    try self.emitBytes(OpCode.OP_CONSTANT, try self.createConstant(value), line);
}
