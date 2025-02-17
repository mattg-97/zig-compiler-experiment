const std = @import("std");

pub fn BoxValue(comptime T: type, value: T, alloc: *std.mem.Allocator) !*T {
    const ptr = try alloc.create(T);
    ptr.* = value;
    return ptr;
}
