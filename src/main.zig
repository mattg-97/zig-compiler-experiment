const std = @import("std");
const check = std.heap.Check;
const string = @import("string").String;

const repl = @import("frontend/repl.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    try repl.startRepl(&arena);
}
