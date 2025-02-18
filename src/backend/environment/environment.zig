const Environment = @This();

const std = @import("std");
const hash = std.hash.Fnv1a_32.hash;

const Value = @import("../types/value.zig");

store: std.AutoHashMap(u32, Value),
alloc: std.mem.Allocator,

pub fn init(alloc: std.mem.Allocator) !*Environment {
    const env = try alloc.create(Environment);
    env.* = .{
        .store = std.AutoHashMap(u32, Value).init(alloc),
        .alloc = alloc,
    };
    return env;
}

pub fn get(self: *Environment, name: []const u8) ?Value {
    const hashKey = hash(name);
    return self.store.get(hashKey);
}

pub fn set(self: *Environment, name: []const u8, value: Value) !Value {
    const hashKey = hash(name);
    try self.store.put(hashKey, value);
    return value;
}
