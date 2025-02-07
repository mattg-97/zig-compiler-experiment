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

pub fn init(alloc: std.mem.Allocator, program: *Program) !*ByteCode {
    const bc = try alloc.create(ByteCode);
    bc.* = .{
        .alloc = alloc,
        .chunk = try Chunk.init(alloc),
        .globals = std.AutoHashMap(usize, []const u8).init(alloc),
        .strings = std.AutoHashMap(usize, []const u8).init(alloc),
        .program = program,
    };
    return bc;
}

fn generateIntegerExpression(self: *ByteCode, expr: AST.IntegerExpression) !void {
    try self.emitConstant(Value.intValue(expr.value), expr.token.Line);
}

fn generateExpression(self: *ByteCode, expr: AST.Expression) !void {
    switch (expr) {
        .Integer => |e| try self.generateIntegerExpression(e),
        else => std.debug.print("Not done yet\n", .{}),
    }
}

fn generateLetStatement(self: *ByteCode, stmt: AST.LetStatement) !void {
    try self.generateExpression(stmt.value.*);
    const constVal = try self.createConstant(Value.stringValue(stmt.name.value));
    try self.emitBytes(OpCode.OP_SET_GLOBAL.asByte(), constVal, stmt.token.Line);
}

fn generateStatement(self: *ByteCode, stmt: AST.Statement) !void {
    switch (stmt) {
        .Expr => |s| try self.generateExpression(s.expression.*),
        .Let => |s| try self.generateLetStatement(s),
        else => std.debug.print("Not done yet\n", .{}),
    }
}

pub fn generate(self: *ByteCode) !void {
    for (self.program.statements.items) |stmt| {
        try self.generateStatement(stmt);
    }
    try self.emitByte(OpCode.OP_RETURN.asByte(), 0);
}

fn emitByte(self: *ByteCode, byte: u8, line: usize) !void {
    try self.chunk.writeChunk(byte, line);
}

fn emitBytes(self: *ByteCode, byte1: u8, byte2: u8, line: usize) !void {
    try self.chunk.writeChunk(byte1, line);
    try self.chunk.writeChunk(byte2, line);
}
fn createConstant(self: *ByteCode, value: Value) !u8 {
    const constant = try self.chunk.addConstant(value);
    if (constant > @as(usize, std.math.maxInt(u8))) {
        @panic("Too many constants in a single chunk.\n");
    }
    return @intCast(constant);
}

fn emitConstant(self: *ByteCode, value: Value, line: usize) !void {
    try self.emitBytes(OpCode.OP_CONSTANT.asByte(), try self.createConstant(value), line);
}
