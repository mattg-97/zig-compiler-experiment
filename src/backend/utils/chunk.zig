const Chunk = @This();

const std = @import("std");

const Value = @import("value.zig");

const OpCode = enum(u8) {
    OP_CONSTANT,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_RETURN,
    OP_POP,
    OP_NEGATE,
    OP_PRINT,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GREATER,
};

alloc: std.mem.Allocator,
codes: std.ArrayList(u8),
constants: std.ArrayList(Value),
lines: std.ArrayList(usize),

pub fn init(alloc: std.mem.Allocator) !*Chunk {
    const chunk = try alloc.create(Chunk);
    chunk.* = .{
        .alloc = alloc,
        .codes = std.ArrayList(u8).init(alloc),
        .constants = std.ArrayList(Value).init(alloc),
        .lines = std.ArrayList(usize).init(alloc),
    };
    return chunk;
}

pub fn writeChunk(self: *Chunk, byte: u8, line: usize) !void {
    try self.codes.append(byte);
    try self.lines.append(line);
}

pub fn addConstant(self: *Chunk, value: Value) !usize {
    try self.constants.append(value);
    return self.constants.items.len - 1;
}

pub fn writeConstant(self: *Chunk, value: Value, line: usize) !void {
    const constIndex = try self.addConstant(value);
    try self.codes.append((constIndex >> 16) & 0xFF);
    try self.lines.append(line);
    try self.codes.append((constIndex >> 8) & 0xFF);
    try self.lines.append(line);
    try self.codes.append(constIndex & 0xFF);
    try self.lines.append(line);
}
