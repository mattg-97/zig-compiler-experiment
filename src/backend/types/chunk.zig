const Chunk = @This();

const std = @import("std");

const Object = @import("object.zig").Object;

pub const OpCode = enum(u8) {
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
    pub fn asByte(self: OpCode) u8 {
        return @as(u8, @intFromEnum(self));
    }
};

alloc: std.mem.Allocator,
codes: std.ArrayList(u8),
constants: std.ArrayList(Object),
lines: std.ArrayList(usize),

pub fn init(alloc: std.mem.Allocator) !*Chunk {
    const chunk = try alloc.create(Chunk);
    chunk.* = .{
        .alloc = alloc,
        .codes = std.ArrayList(u8).init(alloc),
        .constants = std.ArrayList(Object).init(alloc),
        .lines = std.ArrayList(usize).init(alloc),
    };
    return chunk;
}

pub fn writeChunk(self: *Chunk, byte: u8, line: usize) !void {
    try self.codes.append(byte);
    try self.lines.append(line);
}

pub fn addConstant(self: *Chunk, object: Object) !usize {
    try self.constants.append(object);
    return self.constants.items.len - 1;
}

pub fn writeConstant(self: *Chunk, object: Object, line: usize) !void {
    const constIndex = try self.addConstant(object);
    try self.codes.append((constIndex >> 16) & 0xFF);
    try self.lines.append(line);
    try self.codes.append((constIndex >> 8) & 0xFF);
    try self.lines.append(line);
    try self.codes.append(constIndex & 0xFF);
    try self.lines.append(line);
}
