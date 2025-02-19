const Environment = @This();

const std = @import("std");
const hash = std.hash.Fnv1a_32.hash;

const Object = @import("../types/object.zig");

store: std.AutoHashMap(u32, Object),
alloc: std.mem.Allocator,
outer: ?*Environment,

pub fn init(alloc: std.mem.Allocator) !*Environment {
    const env = try alloc.create(Environment);
    env.* = .{
        .store = std.AutoHashMap(u32, Object).init(alloc),
        .alloc = alloc,
        .outer = null,
    };
    return env;
}

pub fn newEnclosed(outer: *Environment) !*Environment {
    const env = try outer.alloc.create(Environment);
    env.* = .{
        .store = std.AutoHashMap(u32, Object).init(outer.alloc),
        .alloc = outer.alloc,
        .outer = outer,
    };
    return env;
}

pub fn get(self: *Environment, name: []const u8) ?Object {
    const hashKey = hash(name);
    const val = self.store.get(hashKey);
    if (val != null) return val;
    if (self.outer) |outer| {
        return outer.get(name);
    } else {
        return null;
    }
}

pub fn set(self: *Environment, name: []const u8, object: Object) !Object {
    const hashKey = hash(name);
    try self.store.put(hashKey, object);
    return object;
}
