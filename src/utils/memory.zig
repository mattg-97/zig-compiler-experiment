const Memory = @This();

const std = @import("std");
const print = std.debug.print;

const Objects = @import("../backend/types/object.zig");

pub fn debugMemory(obj: *const Objects.Object) u64 {
    var byteCount: u64 = 0;
    switch (obj.data) {
        .array => |arr| {
            for (arr.elements.items) |item| {
                byteCount += debugMemory(item);
            }
        },
        .boolean => byteCount += 1,
        .builtin => {},
        .err => |err| {
            byteCount += std.mem.toBytes(err).len;
        },
        .function => |func| {
            byteCount += std.mem.toBytes(func).len;
        },
        .string => |string| {
            byteCount += std.mem.toBytes(string).len;
            byteCount += string.value.len;
        },
        else => |case| byteCount += std.mem.toBytes(case).len,
    }
    return byteCount;
}
