const Environment = @This();

const std = @import("std");
const hash = std.hash.Fnv1a_32.hash;

const Object = @import("../types/object.zig").Object;

store: std.StringHashMap(*Object),
alloc: std.mem.Allocator,
outer: ?*Environment,

pub fn init(alloc: std.mem.Allocator) Environment {
    return .{
        .store = std.StringHashMap(*Object).init(alloc),
        .alloc = alloc,
        .outer = null,
    };
}

pub fn newEnclosed(outer: *Environment) Environment {
    return .{
        .store = std.StringHashMap(*Object).init(outer.alloc),
        .alloc = outer.alloc,
        .outer = outer,
    };
}

pub fn get(self: *Environment, name: []const u8) ?*Object {
    const val = self.store.get(name);
    if (val != null) return val;
    if (self.outer) |outer| {
        return outer.get(name);
    } else {
        return null;
    }
}

pub fn set(self: *Environment, name: []const u8, object: *Object) !void {
    try self.store.put(name, object);
}
