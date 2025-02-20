const Compiler = @This();

const std = @import("std");
const testing = std.testing;

const Objects = @import("../types/object.zig");
const Object = Objects.Object;
const AST = @import("../../frontend/parser/ast.zig");
const Lexer = @import("../../frontend/lexer.zig");
const Parser = @import("../../frontend/parser/parser.zig").Parser;
const Code = @import("./code.zig");

pub const CompilationError = error{
    Unknown,
};

pub const ByteCode = struct {
    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Object),
};

alloc: std.mem.Allocator,
instructions: std.ArrayList(u8),
constants: std.ArrayList(Object),

pub fn init(alloc: std.mem.Allocator) !*Compiler {
    const ptr = try alloc.create(Compiler);
    ptr.* = .{
        .alloc = alloc,
        .constants = std.ArrayList(Object).init(alloc),
        .instructions = std.ArrayList(u8).init(alloc),
    };
    return ptr;
}

pub fn parse(alloc: std.mem.Allocator, input: []const u8) !AST.Program {
    const lex = try Lexer.init(alloc, input);
    try lex.tokenize();

    const parser = try Parser.init(alloc, lex);

    const program = try parser.parseProgram();

    return program;
}

pub fn compile(_: *Compiler, _: AST.Program) CompilationError!void {
    return;
}

pub fn byteCode(self: *Compiler) CompilationError!*ByteCode {
    const ptr = try self.alloc.create(ByteCode);
    ptr.* = .{
        .constants = self.constants,
        .instructions = self.instructions,
    };
    return ptr;
}

const ConstantTypes = union(enum) {
    integer: i64,
};
const CompilerTestCases = struct {
    input: []const u8,
    expectedConstants: []ConstantTypes,
    expectedInstructions: []Code.Instructions,
};

fn runCompilerTests(alloc: std.mem.Allocator, tests: []CompilerTestCases) !void {
    for (tests) |t| {
        const program = parse(t.input);

        const compiler = try init(alloc);

        try compiler.compile(program);

        const bc = try compiler.byteCode();

        try testInstructions(t.expectedInstructions, bc.instructions.items);

        try testConstants(t.expectedConstants, bc.constants.items);
    }
}

fn testInstructions(expected: []Code.Instructions, actual: Code.Instructions) !void {
    const concatted = concatInstructions(expected);
    try testing.expectEqual(actual.len, concatted.len);
    for (concatted, 0..) |ins, i| {
        try testing.expectEqual(actual[i], ins);
    }
}

fn testConstants(expected: []ConstantTypes, actual: []Object) !void {
    try testing.expectEqual(expected.len, actual.len);

    for (expected, 0..) |constant, i| {
        switch (constant) {
            .integer => |int| {
                try testIntegerObject(int, actual[i]);
            },
            else => return,
        }
    }
}

fn testIntegerObject(expected: i64, actual: Object) !void {
    var integer: Objects.Integer = undefined;
    switch (actual.data) {
        .integer => |int| integer = int,
        else => @panic("Expected integer"),
    }

    try testing.expectEqual(expected, integer.value);
}

fn concatInstructions(alloc: std.mem.Allocator, s: []Code.Instructions) Code.Instructions {
    var arrList = std.ArrayList(u8).init(alloc);
    for (s) |ins| {
        for (ins) |b| {
            try arrList.append(b);
        }
    }
    return arrList.items;
}

test "test integer arithmetic" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();

    const testCases = [_]CompilerTestCases{.{
        .input = "1 + 2",
        .expectedConstants = [_]ConstantTypes{
            .{ .integer = 1 },
            .{ .integer = 2 },
        },
        .expectedInstructions = []Code.Instructions{
            Code.Make(.OpConstant, 0),
            Code.Make(.OpConstant, 1),
        },
    }};

    try runCompilerTests(testingAlloc, testCases);
}
