const Debug = @This();

const std = @import("std");
const print = std.debug.print;

const Value = @import("../backend/types/object.zig");
const Chunk = @import("../backend/types/chunk.zig");
const OpCode = Chunk.OpCode;
const Token = @import("../frontend/tokens.zig");
const TokenType = Token.TokenType;

pub fn debugToken(token: Token) void {
    const tokType = std.enums.tagName(TokenType, token.Type).?;
    std.debug.print("{s}\n", .{tokType});
}

pub fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", name);
    return (offset + 1);
}

pub fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const consant = chunk.codes.items[offset + 1];
    print("{s: <15} {d: <4}", .{ name, consant });
    Value.printValue(chunk.constants.items[consant]);
    print("\n", .{});
    return (offset + 2);
}

pub fn disassemleInstruction(chunk: *Chunk, offset: usize) usize {
    print("{d} ", offset);
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("    | ", .{});
    } else {
        print("{d: <4} ", .{chunk.lines.items[offset]});
    }
    const instruction = chunk.codes.items[offset];
    switch (instruction) {
        OpCode.OP_ADD.asByte() => return simpleInstruction("OP_ADD", offset),
        OpCode.OP_MULTIPLY.asByte() => return simpleInstruction("OP_MULTIPLY", offset),
        OpCode.OP_SUBTRACT.asByte() => return simpleInstruction("OP_SUBTRACT", offset),
        OpCode.OP_CONSTANT.asByte() => return constantInstruction("OP_CONSTANT", chunk, offset),
        else => {
            print("no idea\n", .{});
            return (offset + 1);
        },
    }
}
