const std = @import("std");

const Environment = @import("./backend/environment/environment.zig");
const Evaluator = @import("./backend/evaluator/evaluator.zig").Evaluator;
const Lexer = @import("./frontend/lexer.zig");
const Parser = @import("./frontend/parser/parser.zig").Parser;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var env = Environment.init(alloc);
    var evaluator = Evaluator.init(alloc);

    try repl(alloc, &env, &evaluator);
}

fn repl(alloc: std.mem.Allocator, env: *Environment, evaluator: *Evaluator) anyerror!void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    // store input strings
    var inputBuf: [65536]u8 = undefined;
    var lastPos: usize = 0;

    while (true) {
        try stdout.print(">> ", .{});

        if (try stdin.readUntilDelimiterOrEof(inputBuf[lastPos..], '\n')) |input| {
            lastPos += input.len;

            const lexer = try Lexer.init(alloc, input);
            var parser = try Parser.init(alloc, lexer);
            var program = parser.parseProgram() catch |err| {
                try stdout.print("Parsing error: {}\n", .{err});
                continue;
            };

            const object = evaluator.evaluateProgram(&program, env) catch |err| {
                try stdout.print("Evaluating error: {}\n", .{err});
                continue;
            };
            switch (object.*) {
                .err => |error_| {
                    try stdout.print("Error: {s}\n", .{error_.message});
                },
                else => {},
            }
        } else {
            break;
        }
    }
}
