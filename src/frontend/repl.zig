const std = @import("std");

const lexer = @import("lexer.zig");
const ByteCode = @import("../backend/bytecode.zig");
const VM = @import("../backend/vm.zig");
const Parser = @import("parser/parser.zig");
const tokens = @import("tokens.zig");

pub fn startRepl(alloc: std.mem.Allocator) !void {
    std.debug.print("Welcome to zig compiler repl: \\q to quit repl.\n", .{});
    var input: []u8 = undefined;

    while (!std.mem.eql(u8, input, "\\q")) {
        std.debug.print(">> ", .{});
        var buf: [1024]u8 = undefined;
        input = try getInput(&buf);
        try processInput(alloc, input);
    }
}

fn processInput(alloc: std.mem.Allocator, input: []u8) !void {
    var tok = tokens.Token{
        .Literal = "",
        .Type = tokens.TokenType.ILLEGAL,
        .Line = 0,
        .Length = 0,
    };
    const lex = try lexer.init(alloc, input);
    while (tok.Type != tokens.TokenType.EOF) {
        tok = try lex.nextToken();
        std.debug.print("Token -> Type: {s}, Literal: {s}\n", .{ std.enums.tagName(tokens.TokenType, tok.Type).?, tok.Literal });
    }
    const parser = try Parser.init(alloc, lex);
    const program = try parser.parseProgram();
    const bc = try ByteCode.init(alloc, program);
    try bc.generate();
    const vm = try VM.init(alloc, bc);
    _ = try vm.run();
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
