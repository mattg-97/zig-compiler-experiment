const Bits = @This();

const std = @import("std");

pub fn PutUint16(b: *[]u8, v: u16) void {
    b[0] = @as(u8, (v >> 8));
    b[1] = @as(u8, v);
}
