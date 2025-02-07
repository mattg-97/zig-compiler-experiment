const VM = @This();

const std = @import("std");
const hash = std.hash.Fnv1a_32.hash;

const Chunk = @import("utils/chunk.zig");
const OpCode = Chunk.OpCode;
const Value = @import("utils/value.zig");
const ByteCode = @import("bytecode.zig");

alloc: std.mem.Allocator,
hasher: std.hash.Fnv1a_32,
chunk: *Chunk,
ip: std.ArrayList(u8),
ipCurrent: usize,
stack: std.ArrayList(Value),
strings: std.AutoHashMap(usize, []const u8),
globals: std.AutoHashMap(usize, Value),

const Result = enum {
    OK,
    RUNTIME_ERROR,
};

pub fn init(alloc: std.mem.Allocator, byteCode: *ByteCode) !*VM {
    const vm = try alloc.create(VM);
    vm.* = .{
        .alloc = alloc,
        .chunk = byteCode.chunk,
        .ip = byteCode.chunk.codes,
        .ipCurrent = 0,
        .strings = byteCode.strings,
        .globals = byteCode.globals,
        .stack = std.ArrayList(Value).init(alloc),
        .hasher = std.hash.Fnv1a_32.init(),
    };
    return vm;
}

fn readByte(self: *VM) u8 {
    self.ipCurrent += 1;
    return self.ip.items[self.ipCurrent - 1];
}

fn readConstant(self: *VM) Value {
    return self.chunk.constants.items[self.readByte()];
}

fn readString(self: *VM) []const u8 {
    return Value.asString(self.readConstant());
}

fn peek(self: *VM) Value {
    return self.stack.items[self.stack.items.len - 1];
}

pub fn run(self: *VM) !Result {
    while (true) {
        const instruction = self.readByte();
        switch (instruction) {
            OpCode.OP_CONSTANT.asByte() => {
                const val = self.readConstant();
                try self.stack.append(val);
            },
            OpCode.OP_ADD.asByte() => {
                const a = Value.asInt(self.stack.pop());
                const b = Value.asInt(self.stack.pop());
                try self.stack.append(Value.intValue(a + b));
            },
            OpCode.OP_SUBTRACT.asByte() => {
                const a = Value.asInt(self.stack.pop());
                const b = Value.asInt(self.stack.pop());
                try self.stack.append(Value.intValue(a - b));
            },
            @as(u8, @intFromEnum(OpCode.OP_MULTIPLY)) => {
                const a = Value.asInt(self.stack.pop());
                const b = Value.asInt(self.stack.pop());
                try self.stack.append(Value.intValue(a * b));
            },
            @as(u8, @intFromEnum(OpCode.OP_DIVIDE)) => {
                const a = Value.asInt(self.stack.pop());
                const b = Value.asInt(self.stack.pop());
                try self.stack.append(Value.intValue(@divExact(a, b)));
            },
            @as(u8, @intFromEnum(OpCode.OP_NEGATE)) => {
                try self.stack.append(self.stack.pop());
            },
            @as(u8, @intFromEnum(OpCode.OP_POP)) => {
                _ = self.stack.pop();
            },
            @as(u8, @intFromEnum(OpCode.OP_PRINT)) => {
                Value.printValue(self.stack.pop());
            },
            OpCode.OP_SET_GLOBAL.asByte() => {
                const name = self.readString();
                const hashKey = @as(usize, hash(name));
                try self.globals.put(hashKey, self.peek());
                _ = self.stack.pop();
            },
            OpCode.OP_GET_GLOBAL.asByte() => {
                const name = self.readString();
                const hashKey = @as(usize, hash(name));
                const val = self.globals.get(hashKey);
                if (val == null) {
                    std.debug.print("Undefined variable: {s}\n", .{name});
                    return Result.RUNTIME_ERROR;
                }
                try self.stack.append(val.?);
            },
            OpCode.OP_DEFINE_GLOBAL.asByte() => {
                const name = self.readString();
                const global = try self.alloc.create(Value);
                global.* = self.peek();
                const hashKey = @as(usize, hash(name));
                try self.globals.put(hashKey, global.*);
                _ = self.stack.pop();
            },
            @as(u8, @intFromEnum(OpCode.OP_RETURN)) => return Result.OK,
            else => @panic("SHIT"),
        }
    }
}
