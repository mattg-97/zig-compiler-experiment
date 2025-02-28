const Code = @This();

const std = @import("std");
const testing = std.testing;

const bits = @import("./bits.zig");
const Compiler = @import("./compiler.zig");
const concat = Compiler.concatInstructions;

pub const Instructions = []const u8;

fn instructionToString(ins: Instructions) ![]const u8 {
    var out: [1024]u8 = undefined;
    var i: usize = 0;

    while (i < ins.len) {
        const op: OpCode = OpCode.fromOp(ins[i]);
        var def = op.toDefinition();
        const ops = try readOperands(&def, ins[i + 1 ..]);
        std.debug.print("{d} {s}\n", .{ i, fmtInstruction(&def, ops.operands) });
        i = i + 1 + ops.operands.len;
    }

    const toReturn = out[0..];
    return toReturn;
}

fn fmtInstruction(def: *Definition, operands: []i64) []const u8 {
    const operandCount = def.operandWidths.len;
    if (operands.len != operandCount) {
        return "ERROR: operands length does not match defined";
    }
    var buf: [1024]u8 = undefined;
    switch (operandCount) {
        1 => {
            return std.fmt.bufPrint(&buf, "{s} {d}", .{ def.name, operands[0] }) catch @panic("Buf print error");
        },
        else => return "ERROR: unhandled operandCount",
    }
}
const Op = u8;

pub const OpCode = enum(Op) {
    OpConstant,
    pub fn toDefinition(self: OpCode) Definition {
        var definition: Definition = undefined;
        switch (self) {
            .OpConstant => {
                const buf = &[_]usize{2};
                definition = .{ .name = "OpConstant", .operandWidths = buf[0..] };
            },
        }
        return definition;
    }
    pub fn fromOp(op: Op) OpCode {
        return @as(OpCode, @enumFromInt(op));
    }
};

const Definition = struct {
    name: []const u8,
    operandWidths: []const usize,
};

pub fn Make(op: OpCode, operands: []const u16) []u8 {
    const def = op.toDefinition();
    var instructionLen: usize = 1;
    for (def.operandWidths) |w| {
        instructionLen += w;
    }
    var tempArr = [_]u8{0} ** std.math.maxInt(u8);
    var instruction: []u8 = tempArr[0..instructionLen];
    instruction[0] = @intFromEnum(op);
    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.operandWidths[i];
        switch (width) {
            2 => {
                instruction[offset] = @as(u8, @truncate(operand >> 8));
                instruction[offset + 1] = @as(u8, @truncate(operand));
            },
            else => {},
        }
        offset += width;
    }
    return instruction;
}

fn arrayToSlice(comptime T: type, comptime arr: []T) []T {
    return arr[0..arr.len];
}

const OperandsResult = struct {
    operands: []i64,
    offset: u64,
};

fn readUint16(ins: Instructions) u16 {
    return @as(u16, ins[1]) | (@as(u16, (ins[0])) << 0x08);
}

fn readOperands(def: *Definition, ins: Instructions) !OperandsResult {
    var operands = [_]i64{0} ** 4096;

    var offset: usize = 0;

    for (def.operandWidths, 0..) |width, idx| {
        switch (width) {
            2 => operands[idx] = @as(i64, readUint16(ins[offset..])),
            else => @panic("Unable to read operands of this width"),
        }
        offset += width;
    }
    return .{
        .operands = operands[0..],
        .offset = @intCast(offset),
    };
}

test "test make" {
    const TestCase = struct {
        op: OpCode,
        operands: []const u16,
        expected: []const u8,
    };

    const cases = [_]TestCase{
        .{ .op = .OpConstant, .operands = &[_]u16{65534}, .expected = &[_]u8{ @as(u8, @intFromEnum(OpCode.OpConstant)), 255, 254 } },
    };

    for (cases) |case| {
        const instruction = Make(case.op, case.operands);

        try testing.expectEqual(instruction.len, case.expected.len);
        for (case.expected, 0..) |byte, idx| {
            try testing.expectEqual(instruction[idx], byte);
        }
    }
}

test "test bytecode disaseembly" {
    var testArena = std.heap.ArenaAllocator.init(testing.allocator);
    const testingAlloc = testArena.allocator();
    defer testArena.deinit();

    var instruction1 = Code.Make(.OpConstant, &[_]u16{0});
    var instruction2 = Code.Make(.OpConstant, &[_]u16{1});
    var instruction3 = Code.Make(.OpConstant, &[_]u16{65535});

    var expectedInstructions = [3][]const u8{
        instruction1[0..instruction1.len],
        instruction2[0..instruction2.len],
        instruction3[0..instruction3.len],
    };

    const expected =
        \\0000 OpConstant 1
        \\0003 OpConstant 2
        \\0006 OpConstant 65535
    ;
    const concatted: Instructions = try concat(testingAlloc, &expectedInstructions);
    try testing.expectEqualStrings(try instructionToString(concatted), expected);
}

test "test read operands" {
    const TestCase = struct {
        op: OpCode,
        operands: []const u16,
        bytesRead: u64,
    };

    const cases = [_]TestCase{
        .{ .op = .OpConstant, .operands = &[_]u16{65535}, .bytesRead = 2 },
    };

    for (cases) |case| {
        const instruction = Make(case.op, case.operands);

        var def = case.op.toDefinition();

        const operandsRead = try readOperands(&def, instruction[1..]);
        try testing.expectEqual(operandsRead.offset, case.bytesRead);

        for (case.operands, 0..) |want, idx| {
            const readOp: u16 = @intCast(operandsRead.operands[idx]);
            try testing.expectEqual(want, readOp);
        }
    }
}
