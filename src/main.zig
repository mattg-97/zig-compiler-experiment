const std = @import("std");
const check = std.heap.Check;

const repl = @import("frontend/repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    try repl.startRepl(alloc);
}
