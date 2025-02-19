const std = @import("std");

const lexer = @import("lexer.zig");
const ByteCode = @import("../backend/bytecode.zig");
const VM = @import("../backend/vm.zig");
const Environment = @import("../backend/environment/environment.zig");
const Parser = @import("parser/parser.zig").Parser;
const tokens = @import("tokens.zig");
const Evaluator = @import("../backend/evaluator/evaluator.zig").Evaluator;
const stdout = std.io.getStdOut().writer();

pub fn startRepl(arena: *std.heap.ArenaAllocator) !void {
    const alloc = arena.*.allocator();
    std.debug.print("Welcome to zig compiler repl: \\q to quit repl.\n", .{});
    var input: []u8 = undefined;

    const env = try Environment.init(alloc);

    while (!std.mem.eql(u8, input, "\\q")) {
        std.debug.print(">> ", .{});
        var buf: [1024]u8 = undefined;
        input = try getInput(&buf);
        try processInput(arena, alloc, input, env);
    }
}

fn processInput(_: *std.heap.ArenaAllocator, alloc: std.mem.Allocator, input: []u8, env: *Environment) !void {
    const lex = try lexer.init(alloc, input);
    var parser = try Parser.init(alloc, lex);
    var program = try parser.parseProgram();
    var evaluator = Evaluator.init(alloc);
    _ = try evaluator.evaluateProgram(&program, env);
    // const bc = try ByteCode.init(alloc, program);
    // try bc.generate();
    // const vm = try VM.init(alloc, bc);
    // _ = try vm.run();
    //std.debug.assert(arena.*.reset(std.heap.ArenaAllocator.ResetMode.free_all));
}

fn getInput(buf: *[1024]u8) ![]u8 {
    const reader = std.io.getStdIn().reader();
    const input = try reader.readUntilDelimiter(buf, '\n');
    if (std.mem.eql(u8, input, "\\q")) exitProgram();
    return input;
}

fn exitProgram() void {
    std.debug.print("Thanks for coming!\n", .{});
    std.process.exit(0);
}
