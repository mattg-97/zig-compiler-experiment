const std = @import("std");
const check = std.heap.Check;
const repl = @import("frontend/repl.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    defer {
        const leaked = gpa.detectLeaks();
        std.debug.assert(!leaked);
    }
    try repl.startRepl(alloc);
}
