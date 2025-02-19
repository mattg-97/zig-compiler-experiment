const VM = @This();

const std = @import("std");
const hash = std.hash.Fnv1a_32.hash;

const Chunk = @import("types/chunk.zig");
const OpCode = Chunk.OpCode;
const Object = @import("types/object.zig");
const ByteCode = @import("bytecode.zig");
const Debug = @import("../utils/debug.zig");

alloc: std.mem.Allocator,
hasher: std.hash.Fnv1a_32,
chunk: *Chunk,
ip: std.ArrayList(u8),
ipCurrent: usize,
stack: std.ArrayList(Object),
strings: std.AutoHashMap(usize, []const u8),
globals: std.AutoHashMap(usize, Object),

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
        .stack = std.ArrayList(Object).init(alloc),
        .hasher = std.hash.Fnv1a_32.init(),
    };
    return vm;
}

fn readByte(self: *VM) u8 {
    self.ipCurrent += 1;
    return self.ip.items[self.ipCurrent - 1];
}

fn readConstant(self: *VM) Object {
    return self.chunk.constants.items[self.readByte()];
}

fn readString(self: *VM) []const u8 {
    return Object.asString(self.readConstant());
}

fn peek(self: *VM) Object {
    return self.stack.items[self.stack.items.len - 1];
}

pub fn run(self: *VM) !Result {
    while (true) {
        std.debug.print("           ", .{});
        for (self.stack.items) |slot| {
            std.debug.print("[", .{});
            Object.printObject(slot);
            std.debug.print("]", .{});
        }
        std.debug.print("\n", .{});
        const instruction = self.readByte();
        switch (instruction) {
            OpCode.OP_CONSTANT.asByte() => {
                const val = self.readConstant();
                try self.stack.append(val);
            },
            OpCode.OP_ADD.asByte() => {
                const a = Object.asInt(self.stack.pop());
                const b = Object.asInt(self.stack.pop());
                try self.stack.append(Object.intObject(a + b));
            },
            OpCode.OP_SUBTRACT.asByte() => {
                const a = Object.asInt(self.stack.pop());
                const b = Object.asInt(self.stack.pop());
                try self.stack.append(Object.intObject(a - b));
            },
            OpCode.OP_MULTIPLY.asByte() => {
                const a = Object.asInt(self.stack.pop());
                const b = Object.asInt(self.stack.pop());
                try self.stack.append(Object.intObject(a * b));
            },
            OpCode.OP_DIVIDE.asByte() => {
                const a = Object.asInt(self.stack.pop());
                const b = Object.asInt(self.stack.pop());
                try self.stack.append(Object.intObject(@divExact(a, b)));
            },
            OpCode.OP_NEGATE.asByte() => {
                try self.stack.append(self.stack.pop());
            },
            OpCode.OP_POP.asByte() => {
                _ = self.stack.pop();
            },
            OpCode.OP_PRINT.asByte() => {
                Object.printObject(self.stack.pop());
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
                const global = try self.alloc.create(Object);
                global.* = self.peek();
                const hashKey = @as(usize, hash(name));
                try self.globals.put(hashKey, global.*);
                _ = self.stack.pop();
            },
            OpCode.OP_RETURN.asByte() => return Result.OK,
            else => @panic("SHIT"),
        }
    }
}
